#ifndef CPP_RUNDMCSIM_HPP
#define CPP_RUNDMCSIM_HPP

#include "inDMC.h"

void runDMCsim(
        Prms &p,
        std::map<std::string, std::vector<double> > &resSummary,
        std::map<std::string, std::vector<double> > &resDistribution,
        std::map<std::string, std::vector<double> > &resCAF,
        std::map<std::string, std::vector<double> > &simulation,
        std::map<std::string, std::vector<std::vector<double>>> &trials
);

void runDMCsim_t(
        Prms &p,
        std::map<std::string, std::vector<double> > &resSummary,
        std::map<std::string, std::vector<double> > &resDistribution,
        std::map<std::string, std::vector<double> > &resCAF,
        std::map<std::string, std::vector<double> > &simulation,
        std::map<std::string, std::vector<std::vector<double>>> &trials,
        std::string comp,
        int sign,
        std::vector<double> dr_mean,
        std::vector<double> sp_mean
);

void calculate_summary(
        std::vector<double> &rts,
        std::vector<double> &errs,
        unsigned long nTrl,
        std::map<std::string, std::vector<double> > &resSum,
        std::string cond
);

void calculate_percentile(
        int nDelta,
        std::vector<double> &rts,
        std::map<std::string,
                std::vector<double> > &resDistribution,
        std::string cond
);

void calculate_delta(std::map<std::string, std::vector<double> > &resDistribution);

void variable_drift_rate(Prms &p, std::vector<double> &dr, std::vector<double> &dr_mean);

void variable_starting_point(Prms &p, std::vector<double> &sp, std::vector<double> &sp_mean);

void calculate_caf(
        std::vector<double> &rts,
        std::vector<double> &errs,
        int nBins,
        std::map<std::string, std::vector<double> > &resCAF,
        std::string cond
);

void run_simulation(
        Prms &p,
        std::vector<double> &activation_sum,
        std::vector<std::vector<double>> &trial_matrix,
        std::vector<double> &mu_vec,
        std::vector<double> &sp,
        std::vector<double> &dr,
        std::vector<double> &rts,
        std::vector<double> &errs
);

void run_simulation(
        Prms &p,
        std::vector<double> &mu_vec,
        std::vector<double> &sp,
        std::vector<double> &dr,
        std::vector<double> &rts,
        std::vector<double> &errs
);

#endif //CPP_RUNDMCSIM_HPP
