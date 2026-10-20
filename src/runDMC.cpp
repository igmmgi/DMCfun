#include "runDMC.h"
#include <boost/random.hpp>
#include <chrono>
#include <map>
#include <mutex>
#include <thread>
#include <vector>
#include <algorithm>
#include <numeric>
#include <cmath>
#include <omp.h>

std::mutex m;

RNG random_engine(const Prms &p, int sign) {
  if (p.setSeed) {
    RNG rng(p.seedValue + sign);
    return rng;
  }
  auto seed = std::chrono::duration_cast<std::chrono::milliseconds>(
    std::chrono::system_clock::now().time_since_epoch()).count();
  RNG rng(seed + sign);
  return rng;
}

std::vector<double> automatic_activation(const Prms &p) {
  std::vector<double> aa(p.tmax);
  for (unsigned int i = 0; i < p.tmax; i++)
    aa[i] = p.amp * exp(-(i + 1.0) / p.tau) * pow((exp(1.0) * (i + 1.0) / (p.aaShape - 1) / p.tau), p.aaShape - 1);
  return aa;
}

std::vector<double> time_dependent_drift(const Prms &p, int sign, const std::vector<double> &automatic_activation) {
  std::vector<double> tdd(p.tmax);
  for (auto i = 0u; i < tdd.size(); i++)
    tdd[i] = sign * automatic_activation[i] * ((p.aaShape - 1) / (i + 1.0) - 1 / p.tau);
  return tdd;
}

std::vector<double> boundary(const Prms &p) {
  std::vector<double> bnds(p.tmax, p.bnds);
  if (p.bndsRate != 0) {
    for (unsigned int i = 0; i < p.tmax; i++) {
      bnds[i] = bnds[i] * (1 - (p.bndsRate * (i / (i + p.bndsSaturation))));
    }
  }
  return bnds;
}

// Single-pass calculation of mean and SD
std::vector<double> calculate_summary(const std::vector<double> &rts,
                                      const std::vector<double> &errs,
                                      const std::vector<double> &slows,
                                      const unsigned long nTrl) {
  std::vector<double> res(6, 0.0);
  if (!rts.empty()) {
    double sum = 0.0, sq_sum = 0.0;
    for (double v : rts) { sum += v; sq_sum += v * v; }
    double mean_val = sum / rts.size();
    res[0] = mean_val;
    res[1] = std::sqrt(std::max(0.0, sq_sum / rts.size() - mean_val * mean_val));
  }
  res[2] = (errs.size() / static_cast<double>(nTrl)) * 100.0;
  if (!errs.empty()) {
    double sum = 0.0, sq_sum = 0.0;
    for (double v : errs) { sum += v; sq_sum += v * v; }
    double mean_val = sum / errs.size();
    res[3] = mean_val;
    res[4] = std::sqrt(std::max(0.0, sq_sum / errs.size() - mean_val * mean_val));
  }
  res[5] = (slows.size() / static_cast<double>(nTrl)) * 100.0;
  return res;
}

std::vector<double> calculate_percentile(const std::vector<double> &vDelta,
                                         std::vector<double> &rts,
                                         const int type) {
  unsigned int nDelta = vDelta.size() - 2;
  std::vector<double> res_p(nDelta, 0.0);
  std::vector<int> pct_idx_int(nDelta);

  if (rts.size() >= nDelta) {
    std::sort(rts.begin(), rts.end());
    for (unsigned int i = 0; i < nDelta; i++) {
      double pct_idx = (vDelta[i + 1] / 100.0) * (rts.size() - 1);
      pct_idx_int[i] = int(pct_idx);
      double pct_idx_dec = pct_idx - static_cast<double>(pct_idx_int[i]);
      res_p[i] = rts[pct_idx_int[i]];
      if (static_cast<size_t>(pct_idx_int[i] + 1) < rts.size()) {
        res_p[i] += ((rts[pct_idx_int[i] + 1] - rts[pct_idx_int[i]]) * pct_idx_dec);
      }
    }
  }
  if (type == 1) {
    return res_p;
  }

  std::vector<double> res_b(nDelta + 1, 0.0);
  unsigned long idxStart = 0;
  for (unsigned long i = 0; i < pct_idx_int.size() + 1; i++) {
    unsigned long idxEnd = i < pct_idx_int.size() ? pct_idx_int[i] : rts.size();
    double sum = 0.0;
    for (unsigned long j = idxStart; j < idxEnd; j++) {
      sum += rts[j];
    }
    res_b[i] = (idxEnd > idxStart) ? (sum / (idxEnd - idxStart)) : 0.0;
    idxStart = idxEnd;
  }
  return res_b;
}

// Linear two-pointer CAF calculation (No 100,000-element pair vector allocation)
std::vector<double> calculate_caf(const std::vector<double> &rts,
                                  const std::vector<double> &errs,
                                  const int nCAF) {

  std::vector<double> res(nCAF, 0.0);

  if (rts.size() + errs.size() != 0) {

    std::vector<std::pair<double, bool>> comb;
    comb.reserve(rts.size() + errs.size());
    for (double rt : rts)
      comb.emplace_back(rt, false);
    for (double err : errs)
      comb.emplace_back(err, true);

    std::sort(comb.begin(), comb.end());
    std::vector<int> bins(comb.size());
    for (auto i = 0u; i < comb.size(); i++)
      bins[i] = int(nCAF * (i) / comb.size());

    std::vector<long int> countErr(nCAF, 0);
    std::vector<long int> countCor(nCAF, 0);
    for (auto i = 0u; i < bins.size(); i++)
      (comb[i].second == 0) ? countCor[bins[i]]++ : countErr[bins[i]]++;
    for (auto i = 0u; i < countCor.size(); i++)
      res[i] = 1.0 - (countErr[i] / float(countCor[i] + countErr[i]));
  }

  return res;
}

void calculate_delta(std::map<std::string, std::vector<double>> &rdelta) {
  for (auto i = 0u; i < rdelta["delta_correct_comp"].size(); i++) {
    rdelta["delta_correct_mean"].push_back((rdelta["delta_correct_comp"][i] + rdelta["delta_correct_incomp"][i]) / 2.0);
    rdelta["delta_correct_delta"].push_back(rdelta["delta_correct_incomp"][i] - rdelta["delta_correct_comp"][i]);
  }
  for (auto i = 0u; i < rdelta["delta_errors_comp"].size(); i++) {
    rdelta["delta_errors_mean"].push_back((rdelta["delta_errors_comp"][i] + rdelta["delta_errors_incomp"][i]) / 2.0);
    rdelta["delta_errors_delta"].push_back(rdelta["delta_errors_incomp"][i] - rdelta["delta_errors_comp"][i]);
  }
}

// Helpers for on-the-fly random parameter draws (Zero intermediate vector allocations)
inline double sample_dr(const Prms &p, RNG &rng,
                        boost::random::beta_distribution<double> &bdDR,
                        boost::random::uniform_real_distribution<double> &unDR) {
  if (p.drDist == 1) return bdDR(rng) * (p.drLimHigh - p.drLimLow) + p.drLimLow;
  if (p.drDist == 2) return unDR(rng);
  return p.drc;
}

inline double sample_sp(const Prms &p, RNG &rng,
                        boost::random::beta_distribution<double> &bdSP,
                        boost::random::uniform_real_distribution<double> &unSP) {
  if (p.spDist == 1) return (bdSP(rng) * (p.spLimHigh - p.spLimLow) + p.spLimLow) + p.spBias;
  if (p.spDist == 2) return unSP(rng) + p.spBias;
  return p.spBias;
}

inline double sample_res(const Prms &p, RNG &rng,
                         boost::random::normal_distribution<double> &normRes,
                         boost::random::uniform_real_distribution<double> &unRes) {
  if (p.resDist == 1) return std::max(0.0, normRes(rng));
  return std::max(0.0, unRes(rng));
}

// Fast trial simulation on the fly
inline void simulate_trial(const Prms &p, const double* u_vec, const double* bnds, RNG &rng,
                           boost::random::normal_distribution<double> &snd,
                           boost::random::beta_distribution<double> &bdDR,
                           boost::random::uniform_real_distribution<double> &unDR,
                           boost::random::beta_distribution<double> &bdSP,
                           boost::random::uniform_real_distribution<double> &unSP,
                           boost::random::normal_distribution<double> &normRes,
                           boost::random::uniform_real_distribution<double> &unRes,
                           double &out_rt, int &out_choice) {
  double sp_val = sample_sp(p, rng, bdSP, unSP);
  double dr_val = sample_dr(p, rng, bdDR, unDR);
  double res_rt = sample_res(p, rng, normRes, unRes);

  double act = sp_val;
  out_rt = p.rtMax;
  out_choice = 0; // 0 = slow/unbound, 1 = correct, -1 = error

  for (unsigned int i = 0; i < p.tmax; ++i) {
    act += u_vec[i] + (p.sigm * snd(rng));
    if (i >= p.drOnset) act += dr_val;

    if (act > bnds[i]) {
      double val = (i + res_rt + 1.0) - p.drOnset;
      out_rt = val;
      out_choice = (val < p.rtMax) ? 1 : 0;
      return;
    }
    if (act < -bnds[i]) {
      double val = (i + res_rt + 1.0) - p.drOnset;
      out_rt = val;
      out_choice = (val < p.rtMax) ? -1 : 0;
      return;
    }
  }
}

void run_dmc_sim(
    Prms &p, std::map<std::string, std::vector<double>> &rsum,
    std::map<std::string, std::vector<double>> &rsim,
    std::map<std::string, std::vector<std::vector<double>>> &trials) {

  // equation 4
  rsim["eq4"] = automatic_activation(p);
  const std::vector<double> u_comp = time_dependent_drift(p, 1, rsim["eq4"]);
  const std::vector<double> u_incomp = time_dependent_drift(p, -1, rsim["eq4"]);
  const std::vector<double> bnds = boundary(p);

  if (p.fullData) {
    // If full individual trial data is requested (for plotting traces)
    std::vector<std::thread> threads;
    const std::vector<std::string> compatibility{"comp", "incomp"};
    const std::vector<int> sign{1, -1};

    std::mutex m_local;
    for (int i = 0; i < 2; i++) {
      threads.emplace_back(run_dmc_sim_ci, std::ref(p), std::ref(rsum),
                           std::ref(rsim), std::ref(trials),
                           compatibility[i], sign[i],
                           std::ref(m_local));
    }
    for (auto &thread : threads)
      if (thread.joinable())
        thread.join();
    calculate_delta(rsum);
    return;
  }

  // Fast path: OpenMP trial-level parallelism, zero heap vector allocations
  const size_t total_trials = 2 * p.nTrl;
  std::vector<double> all_rts(total_trials);
  std::vector<int> all_choices(total_trials);

  #pragma omp parallel
  {
    int tid = omp_get_thread_num();
    uint64_t seed = (p.setSeed ? (p.seedValue + tid * 10007ULL) : 
                    (std::chrono::system_clock::now().time_since_epoch().count() + tid * 10007ULL));
    RNG rng(seed, 0xda3e39cb94b95bdbULL + (tid << 1));

    boost::random::normal_distribution<double> snd(0.0, 1.0);
    boost::random::beta_distribution<double> bdDR(p.drShape, p.drShape);
    boost::random::uniform_real_distribution<double> unDR(p.drLimLow, p.drLimHigh);
    boost::random::beta_distribution<double> bdSP(p.spShape, p.spShape);
    boost::random::uniform_real_distribution<double> unSP(p.spLimLow, p.spLimHigh);
    boost::random::normal_distribution<double> normRes(p.resMean, p.resSD);
    double range = std::max(0.01, p.resSD * std::sqrt(3.0));
    boost::random::uniform_real_distribution<double> unRes(p.resMean - range, p.resMean + range);

    #pragma omp for schedule(static)
    for (size_t idx = 0; idx < total_trials; ++idx) {
      bool is_comp = (idx < p.nTrl);
      const double* u_vec = is_comp ? u_comp.data() : u_incomp.data();
      simulate_trial(p, u_vec, bnds.data(), rng, snd, bdDR, unDR, bdSP, unSP, normRes, unRes,
                     all_rts[idx], all_choices[idx]);
    }
  }

  rsim["bnds"] = bnds;

  // Partition into rts, errs, slows for comp and incomp
  const std::vector<std::string> comps = {"comp", "incomp"};
  for (int c = 0; c < 2; ++c) {
    const std::string &comp = comps[c];
    size_t offset = (c == 0) ? 0 : p.nTrl;

    std::vector<double> rts, errs, slows;
    rts.reserve(p.nTrl);
    errs.reserve(p.nTrl / 10);
    slows.reserve(p.nTrl / 100);

    for (size_t i = 0; i < p.nTrl; ++i) {
      int ch = all_choices[offset + i];
      double val = all_rts[offset + i];
      if (ch == 1) rts.push_back(val);
      else if (ch == -1) errs.push_back(val);
      else slows.push_back(val);
    }

    rsim["rts_" + comp] = rts;
    rsim["errs_" + comp] = errs;
    rsim["slows_" + comp] = slows;

    rsum[comp] = calculate_summary(rts, errs, slows, p.nTrl);
    rsum["delta_correct_" + comp] = calculate_percentile(p.vDelta, rts, p.tDelta);
    if (p.deltaErrors) {
      rsum["delta_errors_" + comp] = calculate_percentile(p.vDelta, errs, p.tDelta);
    }
    rsum["caf_" + comp] = calculate_caf(rts, errs, p.nCAF);

    std::vector<double> rts_comb = rts;
    rts_comb.insert(rts_comb.end(), errs.begin(), errs.end());
    rsum["caf_rt_" + comp] = calculate_percentile(p.vCAF, rts_comb, 2);
  }

  calculate_delta(rsum);
}

// Full-data slow path (for individual trace plotting)
void run_dmc_sim_ci(const Prms &p,
                    std::map<std::string, std::vector<double>> &rsum,
                    std::map<std::string, std::vector<double>> &rsim,
                    std::map<std::string, std::vector<std::vector<double>>> &trials,
                    const std::string comp,
                    const int sign,
                    std::mutex &m) {

  RNG rng = random_engine(p, sign);

  std::vector<double> rts;
  std::vector<double> errs;
  std::vector<double> slows;
  rts.reserve(p.nTrl);
  errs.reserve(p.nTrl);
  slows.reserve(p.nTrl);
  std::vector<double> activation_sum(p.tmax);
  std::vector<std::vector<double>> trl_mat(
      p.nTrlData,
      std::vector<double>(p.tmax));

  const std::vector<double> u_vec = time_dependent_drift(p, sign, rsim["eq4"]);
  const std::vector<double> dr = drift_rate(p, rng);
  const std::vector<double> sp = starting_point(p, rng);
  const std::vector<double> bnds = boundary(p);

  if (p.fullData) {
    run_simulation(p, activation_sum, trl_mat, u_vec, sp, dr, bnds, rts, errs, slows, rng);
    trials[comp] = trl_mat;
  } else {
    run_simulation(p, u_vec, sp, dr, bnds, rts, errs, slows, rng);
  }

  m.lock();
  rsim["bnds"] = bnds;
  rsim["activation_" + comp] = activation_sum;
  rsim["rts_" + comp] = rts;
  rsim["errs_" + comp] = errs;
  rsim["slows_" + comp] = slows;

  rsum[comp] = calculate_summary(rts, errs, slows, p.nTrl);
  rsum["delta_correct_" + comp] = calculate_percentile(p.vDelta, rts, p.tDelta);
  if (p.deltaErrors) {
    rsum["delta_errors_" + comp] = calculate_percentile(p.vDelta, errs, p.tDelta);
  }
  rsum["caf_" + comp] = calculate_caf(rts, errs, p.nCAF);
  std::vector<double> rts_comb = rts;
  rts_comb.insert(rts_comb.end(), errs.begin(), errs.end());
  rsum["caf_rt_" + comp] = calculate_percentile(p.vCAF, rts_comb, 2);
  m.unlock();
}

std::vector<double> drift_rate(const Prms &p, RNG &rng) {
  std::vector<double> dr(p.nTrl, p.drc);
  if (p.drDist == 1) {
    boost::random::beta_distribution<double> bdDR(p.drShape, p.drShape);
    for (auto &i : dr) i = bdDR(rng) * (p.drLimHigh - p.drLimLow) + p.drLimLow;
  } else if (p.drDist == 2) {
    boost::random::uniform_real_distribution<double> unDR(p.drLimLow, p.drLimHigh);
    for (auto &i : dr) i = unDR(rng);
  }
  return dr;
}

std::vector<double> starting_point(const Prms &p, RNG &rng) {
  std::vector<double> sp(p.nTrl, p.spBias);
  if (p.spDist == 1) {
    boost::random::beta_distribution<double> bdSP(p.spShape, p.spShape);
    for (auto &i : sp) i = (bdSP(rng) * (p.spLimHigh - p.spLimLow) + p.spLimLow) + p.spBias;
  } else if (p.spDist == 2) {
    boost::random::uniform_real_distribution<double> unSP(p.spLimLow, p.spLimHigh);
    for (auto &i : sp) i = unSP(rng) + p.spBias;
  }
  return sp;
}

std::vector<double> residual_rt(const Prms &p, RNG &rng) {
  std::vector<double> residual_distribution(p.nTrl);
  if (p.resDist == 1) {
    boost::random::normal_distribution<double> dist(p.resMean, p.resSD);
    for (auto &i : residual_distribution)
      i = std::max(0.0, dist(rng));
  } else if (p.resDist == 2) {
    double range = std::max(0.01, p.resSD * std::sqrt(3.0));
    boost::random::uniform_real_distribution<double> dist(p.resMean - range, p.resMean + range);
    for (auto &i : residual_distribution)
      i = std::max(0.0, dist(rng));
  }
  return residual_distribution;
}

void run_simulation(const Prms &p,
                    const std::vector<double> &u_vec,
                    const std::vector<double> &sp,
                    const std::vector<double> &dr,
                    const std::vector<double> &bnds,
                    std::vector<double> &rts,
                    std::vector<double> &errs,
                    std::vector<double> &slows,
                    RNG rng) {
  boost::random::normal_distribution<double> snd(0.0, 1.0);
  const std::vector<double> residual_distribution = residual_rt(p, rng);

  for (auto trl = 0u; trl < p.nTrl; trl++) {
    double activation_trial = sp[trl];
    for (auto i = 0u; i < p.tmax; i++) {
      activation_trial += (u_vec[i] + (p.sigm * snd(rng)));
      if (i >= p.drOnset)
        activation_trial += dr[trl];
      if (activation_trial > bnds[i]) {
        double value = (i + residual_distribution[trl] + 1) - p.drOnset;
        (value < p.rtMax ? rts : slows).push_back(value);
        break;
      }
      if (activation_trial < -bnds[i]) {
        double value = (i + residual_distribution[trl] + 1) - p.drOnset;
        (value < p.rtMax ? errs : slows).push_back(value);
        break;
      }
    }
  }
}

void run_simulation(const Prms &p,
                    std::vector<double> &activation_sum,
                    std::vector<std::vector<double>> &trial_matrix,
                    const std::vector<double> &u_vec,
                    const std::vector<double> &sp,
                    const std::vector<double> &dr,
                    const std::vector<double> &bnds,
                    std::vector<double> &rts,
                    std::vector<double> &errs,
                    std::vector<double> &slows,
                    RNG rng) {
  boost::random::normal_distribution<double> snd(0.0, 1.0);
  const std::vector<double> residual_distribution = residual_rt(p, rng);

  for (auto trl = 0u; trl < p.nTrl; trl++) {
    bool criterion = false;
    double activation_trial = sp[trl];
    for (auto i = 0u; i < activation_sum.size(); i++) {
      activation_trial += u_vec[i] + (p.sigm * snd(rng));
      if (i >= p.drOnset)
        activation_trial += dr[trl];
      if (!criterion && activation_trial > bnds[i]) {
        double value = (i + residual_distribution[trl] + 1) - p.drOnset;
        (value < p.rtMax ? rts : slows).push_back(value);
        criterion = true;
      } else if (!criterion && activation_trial < -bnds[i]) {
        double value = (i + residual_distribution[trl] + 1) - p.drOnset;
        (value < p.rtMax ? errs : slows).push_back(value);
        criterion = true;
      }
      if (trl < p.nTrlData)
        trial_matrix[trl][i] = activation_trial;
      activation_sum[i] += activation_trial;
    }
  }
  for (auto i = 0u; i < p.tmax; i++)
    activation_sum[i] /= p.nTrl;
}
