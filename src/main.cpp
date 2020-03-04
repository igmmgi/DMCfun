#include <map>
#include <vector>
#include <string>
#include "inDMC.h"
#include "outDMC.h"
#include "runDMC.h"

int main(int argc, char *argv[]) {

    Prms p;
    bool arg_problem = false;
    process_input_args(argc, argv, p, arg_problem);

    if (arg_problem) return 0;
    if (p.printInputArgs) print_input_args(p);
    
    std::map<std::string, std::vector<double>> rsum;                 // results summary
    std::map<std::string, std::vector<double>> rsim;                 // results simulation
    std::map<std::string, std::vector<std::vector<double>>> trials;  // individual trials

    run_dmc_sim(p, rsum, rsim, trials);

    if (p.printResults) print_results(p, rsum);

    return 0;

}
