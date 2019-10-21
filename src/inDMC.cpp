#include <iostream>
#include <getopt.h>
#include "inDMC.h"

void showHelp() {
    std::cout << "DMC model simulation\n"
                 "Ulrich, R., Schröter, H., Leuthold, H., & Birngruber, T. (2015).\n"
                 "Automatic and controlled stimulus processing in conflict tasks: Superimposed\n"
                 "diffusion processes and delta functions. Cognitive Psychology, 78, 148-174.\n"
                 "Code adapted from Appendix C. Basic Matlab Code\n\n"
                 "Inputs:\n"
                 "amp: amplitude of automatic activation\n"
                 "tau: time to peak of automatic activation\n"
                 "aaShape: shape parameter of automatic activation\n"
                 "mu: drift rate of controlled processes\n"
                 "bnds: +- response barrier\n"
                 "resMean: mean of non-decisional component\n"
                 "resSD: standard deviation of non-decisional component\n"
                 "nTrl: number of trials to simulate\n"
                 "tmax: the number of timepoints per trial\n"
                 "varSP: variable start point\n"
                 "spShape: shape parameter of starting point distribution\n"
                 "spLimLow: lower limit of starting point distribution\n"
                 "spLimHigh: higher limit of starting point distribution\n"
                 "varDR: variable drift rate\n"
                 "drLim: limit range of distribution of drift rate\n"
                 "drShape: shape parameter of drift rate\n"
                 "drLimLow: lower limit of drift rate distribution\n"
                 "drLimHigh: higher limit of drift rate point distribution\n"
                 "fullData: option to return all data required for plots when used within Rcpp\n"
                 "nTrlData: option to plot n trials when used within Rcpp\n"
                 "stepDelta: number of bins for rt distribution analysis\n"
                 "stepCAF: number of bins for conditional accuracy function analysis\n"
                 "printInputArgs: 0/1 print input arguments to console\n"
                 "printResults: 0/1 print results summary to console\n\n"
                 "Examples:\n"
                 "./dmcSim           // Results from Figure 3\n"
                 "./dmcSim --tau 150 // Results from Figure 4\n"
                 "./dmcSim --tau 90  // Results from Figure 5\n"
                 "./dmcSim --varDR 1 // Results from Figure 6\n"
                 "./dmcSim --varSP 1 // Results from Figure 7" << std::endl;
}

void processInputArgs(int argc, char **argv, Prms &p, bool &argProblem) {

    const struct option long_opts[] = {
            {"amp",            1, nullptr, 0},
            {"tau",            1, nullptr, 1},
            {"aaShape",        1, nullptr, 2},
            {"mu",             1, nullptr, 3},
            {"bnds",           1, nullptr, 4},
            {"resMean",        1, nullptr, 5},
            {"resSD",          1, nullptr, 6},
            {"nTrl",           1, nullptr, 7},
            {"tmax",           1, nullptr, 8},
            {"varSP",          1, nullptr, 9},
            {"spShape",        1, nullptr, 10},
            {"spLimLow",       1, nullptr, 11},
            {"spLimHigh",      1, nullptr, 12},
            {"varDR",          1, nullptr, 13},
            {"drLimLow",       1, nullptr, 14},
            {"drLimHigh",      1, nullptr, 15},
            {"drShape",        1, nullptr, 16},
            {"fullData",       1, nullptr, 17},  // when used from within Rcpp
            {"nTrlData",       1, nullptr, 18},  // when used from within Rcpp to plot individual trials (lower left plot)
            {"stepDelta",      1, nullptr, 19},
            {"stepCAF",        1, nullptr, 20},
            {"printInputArgs", 1, nullptr, 21},
            {"printResults",   1, nullptr, 22},
            {"setSeed",        1, nullptr, 23},
            {"help",           0, nullptr, 24},
            {nullptr,          0, nullptr, 0},
    };

    int option;
    int idxOption = 0;
    try {
        while ((option = getopt_long(argc, argv, "", long_opts, &idxOption)) != -1) {
            switch (option) {
                case 0:
                    p.amp = std::stod(optarg);
                    break;
                case 1:
                    p.tau = std::stod(optarg);
                    break;
                case 2:
                    p.aaShape = std::stod(optarg);
                    break;
                case 3:
                    p.mu = std::stod(optarg);
                    break;
                case 4:
                    p.bnds = std::stoi(optarg);
                    break;
                case 5:
                    p.resMean = std::stod(optarg);
                    break;
                case 6:
                    p.resSD = std::stod(optarg);
                    break;
                case 7:
                    p.nTrl = std::stoul(optarg);
                    break;
                case 8:
                    p.tmax = std::stoi(optarg);
                    break;
                case 9:
                    p.varSP = static_cast<bool>(std::stoi(optarg));
                    break;
                case 10:
                    p.spShape = std::stod(optarg);
                    break;
                case 11:
                    p.spLimLow = std::stod(optarg);
                    break;
                case 12:
                    p.spLimHigh = std::stod(optarg);
                    break;
                case 13:
                    p.varDR = static_cast<bool>(std::stoi(optarg));
                    break;
                case 14:
                    p.drLimLow = std::stod(optarg);
                    break;
                case 15:
                    p.drLimHigh = std::stod(optarg);
                    break;
                case 16:
                    p.drShape = std::stod(optarg);
                    break;
                case 17:
                    p.fullData = static_cast<bool>(std::stoi(optarg));
                    break;
                case 18:
                    p.nTrlData = std::stoi(optarg);
                    break;
                case 19:
                    p.stepDelta = std::stoi(optarg);
                    break;
                case 20:
                    p.stepCAF = std::stoi(optarg);
                    break;
                case 21:
                    p.printInputArgs = static_cast<bool>(std::stoi(optarg));
                    break;
                case 22:
                    p.printResults = static_cast<bool>(std::stoi(optarg));
                    break;
                case 23:
                    p.setSeed = static_cast<bool>(std::stoi(optarg));
                    break;
                case 24:
                    showHelp();
                    argProblem = true;
                    break;
                default:
                    std::cout << "Input option not recognized!:" << option << "\n";
                    argProblem = true;
            }
        }
    } catch (...) {
        std::cout << "Input option not recognized:" << long_opts[idxOption].name << ":" << optarg << std::endl;
        argProblem = true;
    }
}

void printInputArgs(Prms &p) {
    std::cout << "\nDMC Parameters:" << "\n";
    std::cout << "amp: " << p.amp << "\n";
    std::cout << "tau: " << p.tau << "\n";
    std::cout << "aaShape: " << p.aaShape << "\n";
    std::cout << "mu: " << p.mu << "\n";
    std::cout << "bnds: " << p.bnds << "\n";
    std::cout << "resMean: " << p.resMean << "\n";
    std::cout << "resSD: " << p.resSD << "\n";
    std::cout << "nTrl: " << p.nTrl << "\n";
    std::cout << "tmax: " << p.tmax << "\n";
    if (p.varSP) {
        std::cout << "varSP: " << p.varSP << "\n";
        std::cout << "spShape: " << p.spShape << "\n";
        std::cout << "spLims: " << p.spLimLow << " to " << p.spLimHigh << "\n";
    }
    if (p.varDR) {
        std::cout << "varDR: " << p.varDR << "\n";
        std::cout << "drShape: " << p.drShape << "\n";
        std::cout << "drLims: " << p.drLimLow << " to " << p.drLimHigh;
    }
    std::cout << std::endl;
}

