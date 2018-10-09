#ifndef CPP_INPUTS_HPP
#define CPP_INPUTS_HPP
#include <chrono>

struct Prms {
    double amp = 20;
    double tau = 30;
    double aaShape = 2;
    double mu = 0.5;
    double sigma = 4;
    double bnds = 75;
    double resMean = 300;
    double resSD = 30;
    unsigned long nTrl = 100000;
    unsigned int tmax = 1000;
    bool varSP = false;
    double spShape = 3;
    double spLimLow = -75;
    double spLimHigh = 75;
    bool varDR = false;
    double drShape = 3;
    double drLimLow = 0.1;
    double drLimHigh = 0.7;
    bool fullData = false;
    unsigned long nTrlData = 5;
    int stepDelta = 10;
    int stepCAF = 20;
    bool printInputArgs = true;
    bool printResults = true;
    bool setSeed = false; // if true, use RNGType rng(1) called

};

void showHelp();
void processInputArgs(int argc, char** argv, Prms &p, bool &argProblem);
void printInputArgs(Prms &p);

#endif //CPP_INPUTS_HPP
