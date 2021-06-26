#include <Rcpp.h>
#include <vector>
#include "inDMC.h"

void print_input_args(Prms &p) {
    p.printResults ? Rprintf("\nDMC Parameters:\n") : Rprintf("\n");
    Rprintf("amp: %-5.1f tau: %-4.0f aaShape: %-5.2f drc: %-5.2f bnds: %-3.0f resMean: %-4.0f resSD: %-3.0f sigm: %-5.2f", 
            p.amp, p.tau, p.aaShape, p.drc, p.bnds, p.resMean, p.resSD, p.sigm) ;
    if (p.varSP) Rprintf("spShape: %-5.2f", p.spShape);
    if (p.varDR) Rprintf("drShape: %-5.2f", p.drShape);
    if (p.printResults) Rprintf("\n\n");
}

