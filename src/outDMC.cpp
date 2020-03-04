#include <Rcpp.h>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <map>
#include <vector>

#include "inDMC.h"

void print_results(Prms &p, std::map<std::string, std::vector<double>> &rsum) {

    Rcpp::Rcout << "Results Summary:" << std::endl;
    Rcpp::Rcout << "\trtCor\tsdRtCor\tperErr\trtErr\tsdRtErr" << std::endl;
    Rcpp::Rcout << "comp\t"
                << std::fixed
                << std::setprecision(0) << rsum["resSum_comp"][0] << "\t"
                << std::setprecision(0) << rsum["resSum_comp"][1] << "\t"
                << std::setprecision(2) << rsum["resSum_comp"][2] << "\t"
                << std::setprecision(0) << rsum["resSum_comp"][3] << "\t"
                << std::setprecision(0) << rsum["resSum_comp"][4] << "\n";
    Rcpp::Rcout << "incomp\t"
                << std::fixed
                << std::setprecision(0) << rsum["resSum_incomp"][0] << "\t"
                << std::setprecision(0) << rsum["resSum_incomp"][1] << "\t"
                << std::setprecision(2) << rsum["resSum_incomp"][2] << "\t"
                << std::setprecision(0) << rsum["resSum_incomp"][3] << "\t"
                << std::setprecision(0) << rsum["resSum_incomp"][4] << "\n";
    Rcpp::Rcout << std::endl;

    // results delta distribution
    Rcpp::Rcout << "Delta Values:\n" << "\t";
    for (auto step = p.stepDelta; step < 100; step += p.stepDelta) 
        Rcpp::Rcout << step << "%\t";
    Rcpp::Rcout << std::endl;

    Rcpp::Rcout << "comp" << "\t" << std::fixed << std::setprecision(1);
    for (unsigned int i = 0; i < rsum["delta_pct_comp"].size(); i++) 
        Rcpp::Rcout << rsum["delta_pct_comp"][i] << "\t";
    Rcpp::Rcout << std::endl;

    Rcpp::Rcout << "incomp" << "\t" << std::fixed << std::setprecision(1);
    for (unsigned int i = 0; i < rsum["delta_pct_incomp"].size(); i++) 
        Rcpp::Rcout << rsum["delta_pct_incomp"][i] << "\t";
    Rcpp::Rcout << std::endl;

    Rcpp::Rcout << "mean" << "\t" << std::fixed << std::setprecision(1);
    for (unsigned int i = 0; i < rsum["delta_pct_mean"].size(); i++) 
        Rcpp::Rcout << rsum["delta_pct_mean"][i] << "\t";
    Rcpp::Rcout << std::endl;

    Rcpp::Rcout << "effect" << "\t" << std::fixed << std::setprecision(1);
    for (unsigned int i = 0; i < rsum["delta_pct_delta"].size(); i++) 
        Rcpp::Rcout << std::setw(5) << rsum["delta_pct_delta"][i] << "\t";
    Rcpp::Rcout << std::endl;
    Rcpp::Rcout << std::endl;

    // results caf
    Rcpp::Rcout << "CAF Values:\n" << "\t";
    for (auto step = 0; step < 100; step += p.stepCAF) 
        Rcpp::Rcout << std::setw(3) << step << "-" << (step + p.stepCAF) << "%\t";
    Rcpp::Rcout << std::endl;

    Rcpp::Rcout << "comp" << "\t" << std::fixed << std::setprecision(3);
    for (unsigned int i = 0; i < rsum["caf_comp"].size(); i++) 
        Rcpp::Rcout << std::setw(7) << rsum["caf_comp"][i] << "\t";
    Rcpp::Rcout << std::endl;
    Rcpp::Rcout << "incomp" << "\t" << std::fixed << std::setprecision(3);
    for (unsigned int i = 0; i < rsum["caf_incomp"].size(); i++) 
        Rcpp::Rcout << std::setw(7) << rsum["caf_incomp"][i] << "\t";
    Rcpp::Rcout << std::endl;

}
