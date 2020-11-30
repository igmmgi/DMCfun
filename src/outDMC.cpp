#include <Rcpp.h>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <map>
#include <vector>

#include "inDMC.h"

void print_results(Prms &p, std::map<std::string, std::vector<double>> &rsum) {

    Rcpp::Rcout << "Results Summary:" << "\n";
    Rcpp::Rcout << "\trtCor\tsdRtCor\tperErr\trtErr\tsdRtErr\tperSlow" << "\n";
    Rcpp::Rcout << "comp\t"
                << std::fixed
                << std::setprecision(0) << rsum["resSum_comp"][0] << "\t"
                << std::setprecision(0) << rsum["resSum_comp"][1] << "\t"
                << std::setprecision(2) << rsum["resSum_comp"][2] << "\t"
                << std::setprecision(0) << rsum["resSum_comp"][3] << "\t"
                << std::setprecision(0) << rsum["resSum_comp"][4] << "\t"
                << std::setprecision(1) << rsum["resSum_comp"][5] << "\n";
    Rcpp::Rcout << "incomp\t"
                << std::fixed
                << std::setprecision(0) << rsum["resSum_incomp"][0] << "\t"
                << std::setprecision(0) << rsum["resSum_incomp"][1] << "\t"
                << std::setprecision(2) << rsum["resSum_incomp"][2] << "\t"
                << std::setprecision(0) << rsum["resSum_incomp"][3] << "\t"
                << std::setprecision(0) << rsum["resSum_incomp"][4] << "\t"
                << std::setprecision(1) << rsum["resSum_incomp"][5] << "\n\n";

    // results delta distribution
    Rcpp::Rcout << "Delta Values:\n" << "\t";
    for (int i = 1; i <= p.nDelta; i++) 
        Rcpp::Rcout << p.vDelta[i] << "%\t";
    Rcpp::Rcout << "\n";

    Rcpp::Rcout << "comp" << "\t" << std::fixed << std::setprecision(1);
    for (int i = 0; i < p.nDelta; i++) 
        Rcpp::Rcout << rsum["delta_pct_comp"][i] << "\t";
    Rcpp::Rcout << "\n";

    Rcpp::Rcout << "incomp" << "\t" << std::fixed << std::setprecision(1);
    for (int i = 0; i < p.nDelta; i++) 
        Rcpp::Rcout << rsum["delta_pct_incomp"][i] << "\t";
    Rcpp::Rcout << "\n";

    Rcpp::Rcout << "mean" << "\t" << std::fixed << std::setprecision(1);
    for (int i = 0; i < p.nDelta; i++) 
        Rcpp::Rcout << rsum["delta_pct_mean"][i] << "\t";
    Rcpp::Rcout << "\n";

    Rcpp::Rcout << "effect" << "\t" << std::fixed << std::setprecision(1);
    std::setw(5);
    for (int i = 0; i < p.nDelta; i++) 
        Rcpp::Rcout << rsum["delta_pct_delta"][i] << "\t";
    Rcpp::Rcout << "\n\n";

    // results caf
    Rcpp::Rcout << "CAF Values:\n" << "\t";
    std::setw(3);
    for (int i = 0; i < p.nCAF; i++) 
        Rcpp::Rcout << static_cast<int>(p.vCAF[i]) << "-" << static_cast<int>(p.vCAF[i+1]) << "%\t";
    Rcpp::Rcout << "\n";

    Rcpp::Rcout << "comp" << "\t" << std::fixed << std::setprecision(3);
    std::setw(7); 
    for (int i = 0; i < p.nCAF; i++) 
        Rcpp::Rcout << rsum["caf_comp"][i] << "\t";
    Rcpp::Rcout << "\n";
    Rcpp::Rcout << "incomp" << "\t" << std::fixed << std::setprecision(3);
    std::setw(7);
    for (int i = 0; i < p.nCAF; i++) 
        Rcpp::Rcout << rsum["caf_incomp"][i] << "\t";
    Rcpp::Rcout << std::endl;

}
