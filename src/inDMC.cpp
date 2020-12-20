#include <Rcpp.h>
#include <vector>
#include "inDMC.h"

void print_input_args(Prms &p) {
    Rprintf("\nDMC Parameters:\n");
    Rprintf("amp: %-8.2f tau: %-8.2f aaShape: %-8.2f drc: %-8.2f bnds: %-8.2f resMean: %-8.2f resSD: %-8.2f sigm: %-8.2f", 
            p.amp, p.tau, p.aaShape, p.drc, p.bnds, p.resMean, p.resSD, p.sigm) ;
    if (p.varSP) Rprintf("spShape: %-8.2f", p.spShape);
    if (p.varDR) Rprintf("drShape: %-8.2f", p.drShape);
    Rprintf("\n\n");
}

