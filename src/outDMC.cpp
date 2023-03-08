#include <Rcpp.h>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <map>
#include <vector>

#include "inDMC.h"

void print_results(Prms &p, std::map<std::string, std::vector<double>> &rsum) {

  Rcpp::Rcout << "Results Summary:"
              << "\n";
  Rcpp::Rcout << "\trtCor\tsdRtCor\tperErr\trtErr\tsdRtErr\tperSlow"
              << "\n";
  Rcpp::Rcout << "comp\t" << std::fixed << std::setprecision(0)
              << rsum["comp"][0] << "\t" << std::setprecision(0)
              << rsum["comp"][1] << "\t" << std::setprecision(2)
              << rsum["comp"][2] << "\t" << std::setprecision(0)
              << rsum["comp"][3] << "\t" << std::setprecision(0)
              << rsum["comp"][4] << "\t" << std::setprecision(1)
              << rsum["comp"][5] << "\n";
  Rcpp::Rcout << "incomp\t" << std::fixed << std::setprecision(0)
              << rsum["incomp"][0] << "\t" << std::setprecision(0)
              << rsum["incomp"][1] << "\t" << std::setprecision(2)
              << rsum["incomp"][2] << "\t" << std::setprecision(0)
              << rsum["incomp"][3] << "\t" << std::setprecision(0)
              << rsum["incomp"][4] << "\t" << std::setprecision(1)
              << rsum["incomp"][5] << "\n\n";

  // results delta distribution
  for (int ce = 0; ce < 2; ce++) {
    if (ce == 1 && !p.deltaErrors) {
      break;
    }
    std::string t;
    if (ce == 0) {
      Rcpp::Rcout << "Delta Values (Correct):\n"
                  << "\t";
      t = "correct";
    } else if (ce == 1) {
      Rcpp::Rcout << "Delta Values (Errors):\n"
                  << "\t";
      t = "errors";
    }
    if (p.tDelta == 1) {
      for (int i = 1; i <= p.nDelta; i++)
        Rcpp::Rcout << p.vDelta[i] << "%\t";
      Rcpp::Rcout << "\n";
    } else if (p.tDelta == 2) {
      for (int i = 1; i <= p.nDelta; i++)
        Rcpp::Rcout << "Bin " << i << "\t";
      Rcpp::Rcout << "\n";
    }

    Rcpp::Rcout << "comp"
                << "\t" << std::fixed << std::setprecision(1);
    for (int i = 0; i < p.nDelta; i++)
      Rcpp::Rcout << rsum["delta_" + t + "_comp"][i] << "\t";
    Rcpp::Rcout << "\n";

    Rcpp::Rcout << "incomp"
                << "\t" << std::fixed << std::setprecision(1);
    for (int i = 0; i < p.nDelta; i++)
      Rcpp::Rcout << rsum["delta_" + t + "_incomp"][i] << "\t";
    Rcpp::Rcout << "\n";

    Rcpp::Rcout << "mean"
                << "\t" << std::fixed << std::setprecision(1);
    for (int i = 0; i < p.nDelta; i++)
      Rcpp::Rcout << rsum["delta_" + t + "_mean"][i] << "\t";
    Rcpp::Rcout << "\n";

    Rcpp::Rcout << "effect"
                << "\t" << std::fixed << std::setprecision(1);
    std::setw(5);
    for (int i = 0; i < p.nDelta; i++)
      Rcpp::Rcout << rsum["delta_" + t + "_delta"][i] << "\t";
    Rcpp::Rcout << "\n\n";
  }

  // results caf
  Rcpp::Rcout << "CAF Values:\n"
              << "\t";
  std::setw(3);
  for (int i = 0; i < p.nCAF; i++)
    Rcpp::Rcout << static_cast<int>(p.vCAF[i]) << "-"
                << static_cast<int>(p.vCAF[i + 1]) << "%\t";
  Rcpp::Rcout << "\n";

  Rcpp::Rcout << "comp"
              << "\t" << std::fixed << std::setprecision(3);
  std::setw(7);
  for (int i = 0; i < p.nCAF; i++)
    Rcpp::Rcout << rsum["caf_comp"][i] << "\t";
  Rcpp::Rcout << "\n";
  Rcpp::Rcout << "incomp"
              << "\t" << std::fixed << std::setprecision(3);
  std::setw(7);
  for (int i = 0; i < p.nCAF; i++)
    Rcpp::Rcout << rsum["caf_incomp"][i] << "\t";
  Rcpp::Rcout << std::endl;
}
