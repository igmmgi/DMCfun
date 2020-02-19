#include <map>
#include <vector>
#include <string>
#include "inDMC.h"
#include "outDMC.h"
#include "runDMC.h"

int main(int argc, char *argv[]) {

    Prms p;
    bool argProb = false;
    processInputArgs(argc, argv, p, argProb);

    if (argProb) return 0;
    if (p.printInputArgs) printInputArgs(p);

    std::map<std::string, std::vector<double>> resSum;
    std::map<std::string, std::vector<double>> sim;
    std::map<std::string, std::vector<std::vector<double>>> trials;

    runDMCsim(p, resSum, sim, trials);

    if (p.printResults) printResults(p, resSum);

    return 0;

}
