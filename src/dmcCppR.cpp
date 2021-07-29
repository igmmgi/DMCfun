#include <Rcpp.h>
#include <map>
#include <vector>

#include "inDMC.h"
#include "outDMC.h"
#include "runDMC.h"

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(BH)]]
// [[Rcpp::export]]

List dmcCppR(List r_in) {

  Prms p; // default parameters
  if (r_in.containsElementNamed("amp"))            p.amp            = as<double>(r_in["amp"]);
  if (r_in.containsElementNamed("tau"))            p.tau            = as<double>(r_in["tau"]);
  if (r_in.containsElementNamed("drc"))            p.drc            = as<double>(r_in["drc"]);
  if (r_in.containsElementNamed("bnds"))           p.bnds           = as<double>(r_in["bnds"]);
  if (r_in.containsElementNamed("resDist"))        p.resDist        = as<double>(r_in["resDist"]);
  if (r_in.containsElementNamed("resMean"))        p.resMean        = as<double>(r_in["resMean"]);
  if (r_in.containsElementNamed("resSD"))          p.resSD          = as<double>(r_in["resSD"]);
  if (r_in.containsElementNamed("rtMax"))          p.rtMax          = as<double>(r_in["rtMax"]);
  if (r_in.containsElementNamed("aaShape"))        p.aaShape        = as<double>(r_in["aaShape"]);
  if (r_in.containsElementNamed("spShape"))        p.spShape        = as<double>(r_in["spShape"]);
  if (r_in.containsElementNamed("sigm"))           p.sigm           = as<double>(r_in["sigm"]);
  if (r_in.containsElementNamed("nTrl"))           p.nTrl           = as<unsigned long>(r_in["nTrl"]);
  if (r_in.containsElementNamed("tmax"))           p.tmax           = as<unsigned int>(r_in["tmax"]);
  if (r_in.containsElementNamed("spDist"))         p.spDist         = as<unsigned int>(r_in["spDist"]);
  if (r_in.containsElementNamed("spLimLow"))       p.spLimLow       = as<double>(r_in["spLimLow"]);
  if (r_in.containsElementNamed("spLimHigh"))      p.spLimHigh      = as<double>(r_in["spLimHigh"]);
  if (r_in.containsElementNamed("spBias"))         p.spBias         = as<double>(r_in["spBias"]);
  if (r_in.containsElementNamed("drDist"))         p.drDist         = as<unsigned int>(r_in["drDist"]);
  if (r_in.containsElementNamed("drShape"))        p.drShape        = as<double>(r_in["drShape"]);
  if (r_in.containsElementNamed("drLimLow"))       p.drLimLow       = as<double>(r_in["drLimLow"]);
  if (r_in.containsElementNamed("drLimHigh"))      p.drLimHigh      = as<double>(r_in["drLimHigh"]);
  if (r_in.containsElementNamed("fullData"))       p.fullData       = as<bool>(r_in["fullData"]);
  if (r_in.containsElementNamed("nTrlData"))       p.nTrlData       = as<double>(r_in["nTrlData"]);
  if (r_in.containsElementNamed("nDelta"))         p.nDelta         = as<double>(r_in["nDelta"]);
  if (r_in.containsElementNamed("pDelta"))         p.pDelta         = Rcpp::as< std::vector<double>> (r_in["pDelta"]);
  if (r_in.containsElementNamed("tDelta"))         p.tDelta         = as<unsigned int>(r_in["tDelta"]);
  if (r_in.containsElementNamed("nCAF"))           p.nCAF           = as<double>(r_in["nCAF"]);
  if (r_in.containsElementNamed("printInputArgs")) p.printInputArgs = as<bool>(r_in["printInputArgs"]);
  if (r_in.containsElementNamed("printResults"))   p.printResults   = as<bool>(r_in["printResults"]);
  if (r_in.containsElementNamed("setSeed"))        p.setSeed        = as<bool>(r_in["setSeed"]);
  if (r_in.containsElementNamed("seedValue"))      p.seedValue      = as<unsigned int>(r_in["seedValue"]);

  // values for delta/CAF
  if (!p.pDelta.empty()) {
    p.vDelta = p.pDelta; // take specific values
    p.vDelta.insert(p.vDelta.begin(), 0);
    p.vDelta.push_back(100);
  } else {
    p.vDelta = linspace(0, 100, p.nDelta + 2 - (p.tDelta - 1));
  }
  p.vCAF = linspace(0, 100, p.nCAF + 1);

  if (p.printInputArgs) print_input_args(p);

  std::map<std::string, std::vector<double>> rsum;                 // results summary
  std::map<std::string, std::vector<double>> rsim;                 // results simulation
  std::map<std::string, std::vector<std::vector<double>>> trials;  // individual trials

  run_dmc_sim(p, rsum, rsim, trials);
  if (p.printResults) print_results(p, rsum);

  List dmc;
  if (p.fullData) {
    dmc["summary"] = rsum;
    dmc["sim"]     = rsim;
    dmc["trials"]  = trials;
  } else {
    dmc["summary"] = rsum;
    dmc["sim"]     = rsim;
  }
  return (dmc);
}

std::vector<double> linspace(int start, int end, int n) {
  double step = (end - start) / double(n-1);
  std::vector<double> out(n);
  double val = start;
  for (int i = 0; i < n; i++) {
    out[i] = val;
    val += step;
  }
  return out;
}
