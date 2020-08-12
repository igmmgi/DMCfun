#include <boost/random.hpp>
#include <map>
#include <mutex>
#include <thread>
#include <vector>

#include "runDMC.h"

std::mutex m;

void run_dmc_sim(Prms &p, 
                 std::map<std::string, std::vector<double>> &rsum, 
                 std::map<std::string, std::vector<double>> &rsim, 
                 std::map<std::string, std::vector<std::vector<double>>> &trials) {
   
   // values for delta/CAF
   if (!p.pDelta.empty()) {
       p.vDelta = p.pDelta; // take specific values
       p.vDelta.insert(p.vDelta.begin(), 0);
       p.vDelta.push_back(100);
   } else {
       p.vDelta = linspace(0, 100, p.nDelta + 2);
   }
   p.vCAF = linspace(0, 100, p.nCAF + 1);
    
    // equation 4
    std::vector<double> eq4(p.tmax);
    for (unsigned int i = 1; i <= p.tmax; i++)
        eq4[i - 1] = (p.amp * exp(-(i / p.tau))) * pow(((exp(1.0) * i / (p.aaShape - 1) / p.tau)), (p.aaShape - 1));
    rsim["eq4"] = eq4;
    
    // run comp and incomp simulation 
    std::vector<std::thread> threads(2);
    std::vector<std::string> compatibility{"comp", "incomp"};
    std::vector<int> sign{1, -1};
    
    for (int i = 0; i < 2; i++) {
        threads.emplace_back(run_dmc_sim_ci,                                      
                             std::ref(p),                                                    
                             std::ref(rsum),                                               
                             std::ref(rsim),                                                  
                             std::ref(trials),                                               
                             std::ref(compatibility[i]),                                     
                             std::ref(sign[i]));      
    }    
   
    for (auto &thread : threads) 
        if (thread.joinable()) thread.join();
   
    calculate_delta(rsum); // finalise results requiring both comp/incomp
    
}


void run_dmc_sim_ci(Prms &p, 
                    std::map<std::string, std::vector<double>> &rsum, 
                    std::map<std::string, std::vector<double>> &rsim, 
                    std::map<std::string, std::vector<std::vector<double>>> &trials, 
                    std::string comp, 
                    int sign) {

    std::vector<double> rts;
    std::vector<double> errs;
    std::vector<double> activation_sum(p.tmax);
    std::vector<std::vector<double>> trl_mat(p.nTrlData, std::vector<double>(p.tmax));  // needed if plotting individual trials

    std::vector<double> mu_vec(p.tmax);
    for (auto i = 0u; i < mu_vec.size(); i++)
        mu_vec[i] = sign * rsim["eq4"][i] * ((p.aaShape - 1) / (i + 1) - 1 / p.tau);

    // variable drift rate/starting point?
    std::vector<double> dr(p.nTrl, p.mu);
    if (p.varDR) variable_drift_rate(p, dr, sign);
    std::vector<double> sp(p.nTrl);
    if (p.varSP) variable_starting_point(p, sp, sign);

    // run simulation and store rts for correct/incorrect trials
    if (p.fullData) {
        run_simulation(p, activation_sum, trl_mat, mu_vec, sp, dr, rts, errs, sign);
        trials[comp] = trl_mat;
    } else{
        run_simulation(p, mu_vec, sp, dr, rts, errs, sign);
    }
    
    m.lock();
    rsim["activation_" + comp] = activation_sum;
    rsim["rts_" + comp]        = rts;
    rsim["errs_" + comp]       = errs;
    rsum["resSum_" + comp]     = calculate_summary(rts, errs, p.nTrl );
    rsum["delta_pct_" + comp]  = calculate_percentile(p.vDelta, rts);
    rsum["caf_" + comp]        = calculate_caf(rts, errs, p.nCAF);
    m.unlock();

}

void variable_drift_rate(Prms &p, std::vector<double> &dr, int sign) {

    const uint32_t s = p.setSeed ? 1 : std::time(nullptr);
    boost::random::mt19937_64 rng(s + sign);
    boost::random::beta_distribution<double> bdDR(p.drShape, p.drShape);

    for (auto &i : dr) i = bdDR(rng) * (p.drLimHigh - p.drLimLow) + p.drLimLow;
}

void variable_starting_point(Prms &p, std::vector<double> &sp, int sign) {

    const uint32_t s = p.setSeed ? 1 : std::time(nullptr);
    boost::random::mt19937_64 rng(s + sign);
    boost::random::beta_distribution<double> bdSP(p.spShape, p.spShape);

    for (auto &i : sp) i = bdSP(rng) * (p.spLimHigh - p.spLimLow) + p.spLimLow;
}


void run_simulation(Prms &p, 
                    std::vector<double> &mu_vec, 
                    std::vector<double> &sp, 
                    std::vector<double> &dr, 
                    std::vector<double> &rts, 
                    std::vector<double> &errs, 
                    int sign) {

    const uint32_t s = p.setSeed ? 1 : std::time(nullptr);
    boost::random::mt19937_64 rng(s + sign);
    boost::random::normal_distribution<double> snd(0.0, 1.0);
    boost::random::normal_distribution<double> nd_mean_sd(p.resMean, p.resSD);

    double activation_trial = 0;
    for (auto trl = 0u; trl < p.nTrl; trl++) {
        activation_trial = sp[trl];
        for (auto i = 0u; i < p.tmax; i++) {
            activation_trial += mu_vec[i] + dr[trl] + (p.sigm * snd(rng));
            if (fabs(activation_trial) > p.bnds) {
                (activation_trial > p.bnds ? rts : errs).push_back(i + nd_mean_sd(rng) + 1); // zero index
                break;
            }
        }
    }

}

void run_simulation(Prms &p, 
                    std::vector<double> &activation_sum, 
                    std::vector<std::vector<double>> &trial_matrix, 
                    std::vector<double> &mu_vec, 
                    std::vector<double> &sp, 
                    std::vector<double> &dr, 
                    std::vector<double> &rts, 
                    std::vector<double> &errs, 
                    int sign ) {

    const uint32_t s = p.setSeed ? 1 : std::time(nullptr);
    boost::random::mt19937_64 rng(s + sign);
    boost::random::normal_distribution<double> snd(0.0, 1.0);
    boost::random::normal_distribution<double> nd_mean_sd(p.resMean, p.resSD);

    double activation_trial;
    bool criterion;
    for (auto trl = 0u; trl < p.nTrl; trl++) {
        criterion = false;
        activation_trial = sp[trl];
        for (auto i = 0u; i < activation_sum.size(); i++) {
            activation_trial += mu_vec[i] + dr[trl] + (p.sigm * snd(rng));
            if (!criterion && fabs(activation_trial) > p.bnds) {
                (activation_trial > p.bnds ? rts : errs).push_back(i + nd_mean_sd(rng) + 1); // zero index
                criterion = true;
            }
            if (trl < p.nTrlData) trial_matrix[trl][i] = activation_trial;
            activation_sum[i] += activation_trial;
        }
    }
    for (auto i = 0u; i < p.tmax; i++) 
        activation_sum[i] /= p.nTrl;

}

std::vector<double> calculate_summary(std::vector<double> &rts, std::vector<double> &errs, unsigned long nTrl) {

    // rtCor, sdRtCor, perErr, rtErr, sdRtErr
    std::vector<double> res(5);
    res[0] = accumulate(rts.begin(), rts.end(), 0.0) / rts.size();
    res[1] = std::sqrt(std::inner_product(rts.begin(), rts.end(), rts.begin(), 0.0) / rts.size() - res[0] * res[0]);
    res[2] = (errs.size() / static_cast<float>(nTrl)) * 100;
    res[3] = accumulate(errs.begin(), errs.end(), 0.0) / errs.size();
    res[4] = std::sqrt(std::inner_product(errs.begin(), errs.end(), errs.begin(), 0.0) / errs.size() - res[3] * res[3]);

    return res;

}

std::vector<double> calculate_percentile( std::vector<double> vDelta, std::vector<double> rts ) {

    int nDelta = vDelta.size() - 2;
    float pct_idx;
    int pct_idx_int;
    float pct_idx_dec;
    std::vector<double> res;
    
    std::sort(rts.begin(), rts.end());
    
    for (int i = 1; i <= nDelta; i++) {

        pct_idx     = (vDelta[i] / 100.0) * (rts.size()-1);
        pct_idx_int = int(pct_idx);
        pct_idx_dec = pct_idx - pct_idx_int;

        res.push_back(rts[pct_idx_int] + ((rts[pct_idx_int + 1] - rts[pct_idx_int]) * pct_idx_dec));

    }
    return res;
}

void calculate_delta( std::map<std::string, std::vector<double> > &rdelta) {
    
    for (auto i = 0u; i < rdelta["delta_pct_comp"].size(); i++) {
        rdelta["delta_pct_mean"].push_back((rdelta["delta_pct_comp"][i]   + rdelta["delta_pct_incomp"][i]) / 2);
        rdelta["delta_pct_delta"].push_back(rdelta["delta_pct_incomp"][i] - rdelta["delta_pct_comp"][i]);
    }
    
}

std::vector<double> calculate_caf(std::vector<double> &rts, std::vector<double> &errs, int nCAF) {

    std::vector<std::pair<double, bool>> comb;
    comb.reserve(rts.size() + errs.size());
    for (double & rt : rts)   comb.emplace_back(std::make_pair(rt, false));
    for (double & err : errs) comb.emplace_back(std::make_pair(err, true));

    std::sort(comb.begin(), comb.end());
    std::vector<int> bins(comb.size());
    for (auto i = 0u; i < comb.size(); i++) 
        bins[i] = int(nCAF * (i) / comb.size());

    std::vector<long int> nErr(nCAF, 0);
    std::vector<long int> nCor(nCAF, 0);
    for (auto i = 0u; i < bins.size(); i++) 
        (comb[i].second == 0) ? nCor[bins[i]]++ : nErr[bins[i]]++;

    std::vector<double> res;
    for (auto i = 0u; i < nCor.size(); i++) 
        res.push_back(1 - (nErr[i] / float(nCor[i] + nErr[i])));
    
    return res;

}

std::vector<double> linspace(int start, int end, int n) {
    double step = (end - start) / double(n-1);
    std::vector<double> out(n);
    double val = start;
    for (int i = 0; i < n; i++) {
        out[i] = val;
        val += step;
    }
    return out;
}

