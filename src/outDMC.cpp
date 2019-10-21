#include <iostream>
#include <iomanip>
#include <vector>
#include <map>
#include <cmath>
#include "inDMC.h"

void printResults(
        Prms &p,
        std::map<std::string, std::vector<double> > &resSum,
        std::map<std::string, std::vector<double> > &resDelta,
        std::map<std::string, std::vector<double> > &resCAF
) {

    std::cout << "Results Summary:" << std::endl;
    std::cout << "\trtCor\tsdRtCor\tperErr\trtErr\tsdRtErr" << std::endl;
    std::cout << "comp\t"
              << round(resSum["resSum_comp"][0]) << "\t"
              << round(resSum["resSum_comp"][1]) << "\t"
              << resSum["resSum_comp"][2] << "\t"
              << round(resSum["resSum_comp"][3]) << "\t"
              << round(resSum["resSum_comp"][4]) << std::endl;
    std::cout << "incomp\t"
              << round(resSum["resSum_incomp"][0]) << "\t"
              << round(resSum["resSum_incomp"][1]) << "\t"
              << resSum["resSum_incomp"][2] << "\t"
              << round(resSum["resSum_incomp"][3]) << "\t"
              << round(resSum["resSum_incomp"][4]);
    std::cout << std::endl;
    std::cout << std::endl;

    // results delta distribution
    std::cout << "Delta Values:\n" << "\t";
    for (auto step = p.stepDelta; step < 100; step += p.stepDelta) {
        std::cout << std::setw(4) << step << "%\t";
    }
    std::cout << std::endl;

    std::cout << "comp" << "\t";
    for (unsigned int i = 0; i < resDelta["delta_pct_comp"].size(); i++) {
        std::cout << std::fixed << std::setprecision(1) << resDelta["delta_pct_comp"][i] << "\t";
    }
    std::cout << std::endl;

    std::cout << "incomp" << "\t";
    for (unsigned int i = 0; i < resDelta["delta_pct_incomp"].size(); i++) {
        std::cout << std::fixed << std::setprecision(1) << resDelta["delta_pct_incomp"][i] << "\t";
    }
    std::cout << std::endl;

    std::cout << "mean" << "\t";
    for (unsigned int i = 0; i < resDelta["delta_pct_mean"].size(); i++) {
        std::cout << std::fixed << std::setprecision(1) << resDelta["delta_pct_mean"][i] << "\t";
    }
    std::cout << std::endl;

    std::cout << "effect" << "\t";
    for (unsigned int i = 0; i < resDelta["delta_pct_delta"].size(); i++) {
        std::cout << std::fixed << std::setprecision(1) << std::right << std::setw(5)
                  << resDelta["delta_pct_delta"][i] << "\t";
    }
    std::cout << std::endl;
    std::cout << std::endl;

    // results caf
    std::cout << "CAF Values:\n" << "\t";
    for (auto step = 0; step < 100; step += p.stepCAF) {
        std::cout << std::right << std::setw(3) << step << "-" << (step + p.stepCAF) << "%\t";
    }
    std::cout << std::endl;

    std::cout << "comp" << "\t";
    for (unsigned int i = 0; i < resCAF["caf_comp"].size(); i++) {
        std::cout << std::fixed << std::setprecision(3) << std::right << std::setw(7)
                  << resCAF["caf_comp"][i] << "\t";
    }
    std::cout << std::endl;
    std::cout << "incomp" << "\t";
    for (unsigned int i = 0; i < resCAF["caf_incomp"].size(); i++) {
        std::cout << std::fixed << std::setprecision(3) << std::right << std::setw(7)
                  << resCAF["caf_incomp"][i] << "\t";
    }
    std::cout << std::endl;

}
