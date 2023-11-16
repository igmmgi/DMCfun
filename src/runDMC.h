#ifndef CPP_RUNDMCSIM_HPP
#define CPP_RUNDMCSIM_HPP

#include <boost/random.hpp>
#include <map>
#include "inDMC.h"

typedef boost::random::mt19937_64 RNG;

void run_dmc_sim(
    Prms &p,
    std::map<std::string, std::vector<double>> &rsum,
    std::map<std::string, std::vector<double>> &rsim,
    std::map<std::string, std::vector<std::vector<double>>> &trials
  );

void run_dmc_sim_ci(
    Prms &p,
    std::map<std::string, std::vector<double>> &rsum,
    std::map<std::string, std::vector<double>> &rsim,
    std::map<std::string, std::vector<std::vector<double>>> &trials,
    std::string comp, int sign
  );

std::vector<double> calculate_summary(
    std::vector<double> &rts,
    std::vector<double> &errs,
    std::vector<double> &slows,
    unsigned long nTrl
  );

std::vector<double> calculate_percentile(
    std::vector<double> vDelta,
    std::vector<double> rts,
    int tDelta
  );

void calculate_delta(std::map<std::string, std::vector<double>> &rdelta);

void variable_drift_rate(Prms &p, std::vector<double> &dr, RNG &rng);

void variable_starting_point(Prms &p, std::vector<double> &sp, RNG &rng);

void boundary(Prms &p, std::vector<double> &bnds);

void residual_rt(Prms &p, std::vector<double> &residual_distribution, RNG &rng);

std::vector<double> calculate_caf(
    std::vector<double> &rts,
    std::vector<double> &errs,
    int nBins
  );

void run_simulation(
    Prms &p,
    std::vector<double> &activation_sum,
    std::vector<std::vector<double>> &trial_matrix,
    std::vector<double> &u_vec,
    std::vector<double> &sp,
    std::vector<double> &dr,
    std::vector<double> &bnds,
    std::vector<double> &rts,
    std::vector<double> &errs,
    std::vector<double> &slows,
    RNG rng
  );

void run_simulation(
    Prms &p,
    std::vector<double> &u_vec,
    std::vector<double> &sp,
    std::vector<double> &dr,
    std::vector<double> &bnds,
    std::vector<double> &rts,
    std::vector<double> &errs,
    std::vector<double> &slows,
    RNG rng
  );

#endif //CPP_RUNDMCSIM_HPP
