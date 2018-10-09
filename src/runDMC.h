#ifndef CPP_RUNDMCSIM_HPP
#define CPP_RUNDMCSIM_HPP
#include "inDMC.h"

void runDMCsim(Prms &p,
               std::map<std::string, std::vector<double> > &resSummary,
               std::map<std::string, std::vector<double> > &resDistribution,
               std::map<std::string, std::vector<double> > &resCAF,
               std::map<std::string, std::vector<double> > &simulation,
               std::map<std::string, std::vector<std::vector<double>> > &trials);

void calculate_summary(std::vector<double> &rts,
                       std::vector<double> &errs,
                       unsigned long nTrl,
                       std::map<std::string, std::vector<double> > &resSum,
                       std::string cond);

void calculate_percentile(int nDelta,
                          std::vector<double> &rts, std::map<std::string, std::vector<double> > &resDistribution,
                          std::string cond);

void calculate_delta(std::map<std::string, std::vector<double> > &resDistribution);

void calculate_caf(std::vector<double> &rts,
                   std::vector<double> &errs,
                   int nBins,
                   std::map<std::string, std::vector<double> > &resCAF,
                   std::string cond);

#endif //CPP_RUNDMCSIM_HPP
