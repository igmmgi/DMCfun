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
    const Prms &p,
    std::map<std::string, std::vector<double>> &rsum,
    std::map<std::string, std::vector<double>> &rsim,
    std::map<std::string, std::vector<std::vector<double>>> &trials,
    const std::string comp,
    const int sign
  );

const std::vector<double> automatic_activation(const Prms &p);
const std::vector<double> time_dependent_drift(const Prms &p, int sign, std::vector<double> &automatic_activation);

std::vector<double> calculate_summary(
    const std::vector<double> &rts,
    const std::vector<double> &errs,
    const std::vector<double> &slows,
    const unsigned long nTrl
  );

std::vector<double> calculate_percentile(
    std::vector<double> vDelta,
    std::vector<double> rts,
    int tDelta
  );

void calculate_delta(std::map<std::string, std::vector<double>> &rdelta);

const std::vector<double> drift_rate(const Prms &p, RNG &rng);
const std::vector<double> starting_point(const Prms &p, RNG &rng);
const std::vector<double> boundary(const Prms &p);
const std::vector<double> residual_rt(const Prms &p,  RNG &rng);

std::vector<double> calculate_caf(
    const std::vector<double> &rts,
    const std::vector<double> &errs,
    const int nBins
  );

void run_simulation(
    const Prms &p,
    std::vector<double> &activation_sum,
    std::vector<std::vector<double>> &trial_matrix,
    const std::vector<double> &u_vec,
    const std::vector<double> &sp,
    const std::vector<double> &dr,
    const std::vector<double> &bnds,
    std::vector<double> &rts,
    std::vector<double> &errs,
    std::vector<double> &slows,
    RNG rng
  );

void run_simulation(
    const Prms &p,
    const std::vector<double> &u_vec,
    const std::vector<double> &sp,
    const std::vector<double> &dr,
    const std::vector<double> &bnds,
    std::vector<double> &rts,
    std::vector<double> &errs,
    std::vector<double> &slows,
    RNG rng
  );

#endif //CPP_RUNDMCSIM_HPP
