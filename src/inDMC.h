#ifndef CPP_INPUTS_HPP
#define CPP_INPUTS_HPP
#include <vector>

struct Prms {
    double amp = 20;
    double tau = 30;
    double aaShape = 2;
    double drc = 0.5;
    double sigm = 4;
    double bnds = 75;
    int resDist = 1;
    double resMean = 300;
    double resSD = 30;
    double rtMax = 5000;
    unsigned long nTrl = 100000;
    unsigned int tmax = 1000;
    int spDist = 0;
    double spShape = 3;
    double spLimLow = -75;
    double spLimHigh = 75;
    double spBias = 0;
    int drDist = 0;
    double drShape = 3;
    double drLimLow = 0.1;
    double drLimHigh = 0.7;
    bool fullData = false;
    unsigned long nTrlData = 5;
    int nDelta = 10;
    unsigned int tDelta = 1;
    std::vector<double> pDelta;
    std::vector<double> vDelta;
    int nCAF = 5;
    std::vector<double> vCAF;
    bool printInputArgs = true;
    bool printResults = true;
    bool setSeed = false; // if true, use seed set to seed value
    unsigned int seedValue = 1;
};

void print_input_args(Prms &p);

std::vector<double> linspace(int start, int end, int n);

#endif //CPP_INPUTS_HPP
