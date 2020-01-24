#include <map>
#include <vector>
#include <boost/random.hpp>
#include "inDMC.h"
#include "outDMC.h"
#include "runDMC.h"

int main(int argc, char *argv[]) {

    Prms p;
    bool argProblem = false;
    processInputArgs(argc, argv, p, argProblem);

    if (argProblem) return 0;
    if (p.printInputArgs) printInputArgs(p);

    std::map<std::string, std::vector<double> > resSummary;
    std::map<std::string, std::vector<double> > resDelta;
    std::map<std::string, std::vector<double> > resCAF;
    std::map<std::string, std::vector<double> > simulation;
    std::map<std::string, std::vector<std::vector<double>>> trials;

    runDMCsim(p, resSummary, resDelta, resCAF, simulation, trials);

    if (p.printResults) printResults(p, resSummary, resDelta, resCAF);

    return 0;

}
