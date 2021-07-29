#include <Rcpp.h>
#include <vector>
#include "inDMC.h"

void print_input_args(Prms &p) {
    p.printResults ? Rprintf("\nDMC Parameters:\n") : Rprintf("\n");
    Rprintf("amp: %-5.1f tau: %-4.0f drc: %-5.2f bnds: %-3.0f resMean: %-4.0f resSD: %-3.0f aaShape: %-5.2f sigm: %-5.2f",
            p.amp, p.tau, p.drc, p.bnds, p.resMean, p.resSD, p.aaShape, p.sigm) ;
    if (p.spDist != 0) Rprintf("spShape: %-5.2f", p.spShape);
    if (p.spBias != 0) Rprintf("spBias: %-5.2f", p.spBias);
    if (p.drDist != 0) Rprintf("drShape: %-5.2f", p.drShape);
    if (p.printResults) Rprintf("\n\n");
}

