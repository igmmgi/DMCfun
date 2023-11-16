#include "runDMC.h"
#include <boost/random.hpp>
#include <chrono>
#include <map>
#include <mutex>
#include <thread>

std::mutex m;

RNG random_engine(Prms &p, int sign) {
  if (p.setSeed) {
    RNG rng(p.seedValue + sign);
    return rng;
  } else {
    auto seed = std::chrono::duration_cast<std::chrono::milliseconds>(
                    std::chrono::system_clock::now().time_since_epoch())
                    .count();
    RNG rng(seed + sign);
    return rng;
  }
}

void run_dmc_sim(
    Prms &p, std::map<std::string, std::vector<double>> &rsum,
    std::map<std::string, std::vector<double>> &rsim,
    std::map<std::string, std::vector<std::vector<double>>> &trials) {

  // equation 4
  std::vector<double> eq4(p.tmax);
  for (unsigned int i = 0; i < p.tmax; i++)
    eq4[i] =
        p.amp * exp(-(i + 1.0) / p.tau) *
        pow((exp(1.0) * (i + 1.0) / (p.aaShape - 1) / p.tau), p.aaShape - 1);
  rsim["eq4"] = eq4;

  // run comp and incomp simulation
  std::vector<std::thread> threads;
  std::vector<std::string> compatibility{"comp", "incomp"};
  std::vector<int> sign{1, -1};

  for (int i = 0; i < 2; i++) {
    threads.emplace_back(run_dmc_sim_ci, std::ref(p), std::ref(rsum),
                         std::ref(rsim), std::ref(trials),
                         std::ref(compatibility[i]), std::ref(sign[i]));
  }

  for (auto &thread : threads)
    if (thread.joinable())
      thread.join();

  calculate_delta(rsum); // finalise results requiring both comp/incomp
}

void run_dmc_sim_ci(
    Prms &p, std::map<std::string, std::vector<double>> &rsum,
    std::map<std::string, std::vector<double>> &rsim,
    std::map<std::string, std::vector<std::vector<double>>> &trials,
    std::string comp, int sign) {

  RNG rng = random_engine(p, sign);

  std::vector<double> rts;
  std::vector<double> errs;
  std::vector<double> slows;
  std::vector<double> activation_sum(p.tmax);
  std::vector<std::vector<double>> trl_mat(
      p.nTrlData,
      std::vector<double>(p.tmax)); // needed if plotting individual trials

  std::vector<double> u_vec(p.tmax);
  for (auto i = 0u; i < u_vec.size(); i++)
    u_vec[i] =
        sign * rsim.at("eq4")[i] * ((p.aaShape - 1) / (i + 1.0) - 1 / p.tau);

  // variable drift rate?
  std::vector<double> dr(p.nTrl, p.drc);
  if (p.drDist != 0)
    variable_drift_rate(p, dr, rng);

  // variable starting point?
  std::vector<double> sp(p.nTrl, p.spBias);
  if (p.spDist != 0)
    variable_starting_point(p, sp, rng);

  // boundary
  std::vector<double> bnds(p.tmax, p.bnds);
  //if (p.bndsRate != 0)
  boundary(p, bnds);

  // run simulation and store rts for correct/incorrect trials
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
    rsum["delta_errors_" + comp] =
        calculate_percentile(p.vDelta, errs, p.tDelta);
  }
  rsum["caf_" + comp] = calculate_caf(rts, errs, p.nCAF);
  rsum["caf_rt_" + comp] = calculate_percentile(p.vCAF, rts, 2);
  m.unlock();
}

void variable_drift_rate(Prms &p, std::vector<double> &dr, RNG &rng) {
  if (p.drDist == 1) {
    boost::random::beta_distribution<double> bdDR(p.drShape, p.drShape);
    for (auto &i : dr)
      i = bdDR(rng) * (p.drLimHigh - p.drLimLow) + p.drLimLow;
  } else if (p.drDist == 2) {
    boost::random::uniform_real_distribution<double> unDR(p.drLimLow, p.drLimHigh);
    for (auto &i : dr)
      i = unDR(rng);
  }
}

void variable_starting_point(Prms &p, std::vector<double> &sp, RNG &rng) {
  if (p.spDist == 1) {
    boost::random::beta_distribution<double> bdSP(p.spShape, p.spShape);
    for (auto &i : sp)
      i = (bdSP(rng) * (p.spLimHigh - p.spLimLow) + p.spLimLow) + p.spBias;
  } else if (p.spDist == 2) {
    boost::random::uniform_real_distribution<double> unSP(p.spLimLow, p.spLimHigh);
    for (auto &i : sp)
      i = unSP(rng) + p.spBias;
  }
}


void boundary(Prms &p, std::vector<double> &bnds) {
  for (unsigned int i = 0; i < p.tmax; i++) {
    bnds[i] = bnds[i] * (1-(p.bndsRate * (i/(i+p.bndsSaturation))));
  }
}

void residual_rt(Prms &p, std::vector<double> &residual_distribution, RNG &rng) {
  if (p.resDist == 1) {
    // Standard normal distribution with mean + sd (NB make sure no -ve)
    boost::random::normal_distribution<double> dist(p.resMean, p.resSD);
    for (auto &i : residual_distribution)
      i = std::max(0.0, dist(rng));
  } else if (p.resDist == 2) {
    // Standard uniform distribution with mean + sd
    double range = std::max(0.01, sqrt((p.resSD * p.resSD / (1.0 / 12.0))) / 2);
    boost::random::uniform_real_distribution<double> dist(p.resMean - range, p.resMean + range);
    for (auto &i : residual_distribution)
      i = std::max(0.0, dist(rng));
  }
}

void run_simulation(Prms &p, std::vector<double> &u_vec,
                    std::vector<double> &sp, std::vector<double> &dr,
                    std::vector<double> &bnds,
                    std::vector<double> &rts, std::vector<double> &errs,
                    std::vector<double> &slows, RNG rng) {

  boost::random::normal_distribution<double> snd(0.0, 1.0);

  // residual RT distribution
  std::vector<double> residual_distribution(p.nTrl);
  residual_rt(p, residual_distribution, rng);

  double activation_trial;
  double value;
  for (auto trl = 0u; trl < p.nTrl; trl++) {
    activation_trial = sp[trl];
    for (auto i = 0u; i < p.tmax; i++) {
      activation_trial += (u_vec[i] + (p.sigm * snd(rng)));
      if (i >= p.drOnset)
        activation_trial += dr[trl];
      if (activation_trial > bnds[i]) {
        value = (i + residual_distribution[trl] + 1) - p.drOnset; // RT measured from onset of relevant dimension!
        (value < p.rtMax ? rts : slows).push_back(value);
        break;
      } else if (activation_trial < -bnds[i]) {
        value = (i + residual_distribution[trl] + 1) - p.drOnset;
        (value < p.rtMax ? errs : slows).push_back(value);
        break;
      }
    }
  }
}

void run_simulation(Prms &p, std::vector<double> &activation_sum,
                    std::vector<std::vector<double>> &trial_matrix,
                    std::vector<double> &u_vec, std::vector<double> &sp,
                    std::vector<double> &dr,
                    std::vector<double> &bnds,
                    std::vector<double> &rts,
                    std::vector<double> &errs, std::vector<double> &slows,
                    RNG rng) {

  boost::random::normal_distribution<double> snd(0.0, 1.0);

  // residual RT distribution
  std::vector<double> residual_distribution(p.nTrl);
  residual_rt(p, residual_distribution, rng);

  double activation_trial;
  bool criterion;
  double value;
  for (auto trl = 0u; trl < p.nTrl; trl++) {
    criterion = false;
    activation_trial = sp[trl];
    for (auto i = 0u; i < activation_sum.size(); i++) {
      activation_trial += u_vec[i] + (p.sigm * snd(rng));
      if (i >= p.drOnset)
        activation_trial += dr[trl];
      if (!criterion && activation_trial > bnds[i]) {
        value = (i + residual_distribution[trl] + 1) - p.drOnset;
        (value < p.rtMax ? rts : slows).push_back(value);
        criterion = true;
      } else if (!criterion && activation_trial < -bnds[i]) {
        value = (i + residual_distribution[trl] + 1) - p.drOnset;
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

std::vector<double> calculate_summary(std::vector<double> &rts,
                                      std::vector<double> &errs,
                                      std::vector<double> &slows,
                                      unsigned long nTrl) {

  // rtCor, sdRtCor, perErr, rtErr, sdRtErr, perSlow
  std::vector<double> res(6);
  res[0] = accumulate(rts.begin(), rts.end(), 0.0) / rts.size();
  res[1] =
      std::sqrt(std::inner_product(rts.begin(), rts.end(), rts.begin(), 0.0) /
                    rts.size() -
                res[0] * res[0]);
  res[2] = (errs.size() / static_cast<float>(nTrl)) * 100;
  res[3] = accumulate(errs.begin(), errs.end(), 0.0) / errs.size();
  res[4] = std::sqrt(
      std::inner_product(errs.begin(), errs.end(), errs.begin(), 0.0) /
          errs.size() -
      res[3] * res[3]);
  res[5] = (slows.size() / static_cast<double>(nTrl)) * 100;

  return res;
}

std::vector<double> calculate_percentile(std::vector<double> vDelta,
                                         std::vector<double> rts, int type) {

  unsigned int nDelta = vDelta.size() - 2;
  std::vector<double> res_p(nDelta, 0);

  double pct_idx;
  std::vector<int> pct_idx_int(nDelta);
  double pct_idx_dec;

  if (rts.size() >= nDelta) {
    std::sort(rts.begin(), rts.end());
    for (unsigned int i = 0; i < nDelta; i++) {
      pct_idx = (vDelta[i + 1] / 100.0) * (rts.size() - 1);
      pct_idx_int[i] = int(pct_idx);
      pct_idx_dec = pct_idx - static_cast<double>(pct_idx_int[i]);
      res_p[i] =
          rts[pct_idx_int[i]] +
          ((rts[pct_idx_int[i] + 1] - rts[pct_idx_int[i]]) * pct_idx_dec);
    }
  }
  if (type == 1) {
    return res_p;
  }

  std::vector<double> res_b(nDelta + 1, 0);
  unsigned long idxStart, idxEnd;
  idxStart = 0;
  for (unsigned long i = 0; i < pct_idx_int.size() + 1; i++) {
    idxEnd = i < pct_idx_int.size() ? pct_idx_int[i] : rts.size();
    for (unsigned long j = idxStart; j < idxEnd; j++) {
      res_b[i] += rts[j];
    }
    res_b[i] /= (idxEnd - idxStart);
    idxStart = idxEnd;
  }
  return res_b;
}

void calculate_delta(std::map<std::string, std::vector<double>> &rdelta) {

  for (auto i = 0u; i < rdelta["delta_correct_comp"].size(); i++) {
    rdelta["delta_correct_mean"].push_back(
        (rdelta["delta_correct_comp"][i] + rdelta["delta_correct_incomp"][i]) /
        2);
    rdelta["delta_correct_delta"].push_back(rdelta["delta_correct_incomp"][i] -
                                            rdelta["delta_correct_comp"][i]);
  }
  for (auto i = 0u; i < rdelta["delta_errors_comp"].size(); i++) {
    rdelta["delta_errors_mean"].push_back(
        (rdelta["delta_errors_comp"][i] + rdelta["delta_errors_incomp"][i]) /
        2);
    rdelta["delta_errors_delta"].push_back(rdelta["delta_errors_incomp"][i] -
                                           rdelta["delta_errors_comp"][i]);
  }
}

std::vector<double> calculate_caf(std::vector<double> &rts,
                                  std::vector<double> &errs, int nCAF) {

  std::vector<double> res(nCAF, 0);

  if (rts.size() + errs.size() != 0) {

    std::vector<std::pair<double, bool>> comb;
    comb.reserve(rts.size() + errs.size());

    for (double &rt : rts)
      comb.emplace_back(std::make_pair(rt, false));
    for (double &err : errs)
      comb.emplace_back(std::make_pair(err, true));

    std::sort(comb.begin(), comb.end());
    std::vector<int> bins(comb.size());
    for (auto i = 0u; i < comb.size(); i++)
      bins[i] = int(nCAF * (i) / comb.size());

    std::vector<long int> countErr(nCAF, 0);
    std::vector<long int> countCor(nCAF, 0);
    for (auto i = 0u; i < bins.size(); i++)
      (comb[i].second == 0) ? countCor[bins[i]]++ : countErr[bins[i]]++;
    for (auto i = 0u; i < countCor.size(); i++)
      res[i] = 1 - (countErr[i] / float(countCor[i] + countErr[i]));
  }

  return res;
}
