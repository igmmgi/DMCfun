#include <Rcpp.h>
// #include <vector>
#include "inDMC.h"

void print_input_args(Prms &p) {
    p.printResults ? Rprintf("\nDMC Parameters:\n") : Rprintf("\n");
    Rprintf("amp:%-5.1f tau:%-4.0f drc:%-5.2f bnds:%-3.0f resMean:%-4.0f resSD:%-3.0f aaShape:%-4.1f spShape:%-4.1f spBias:%-4.1f drShape:%-4.1f sigm:%-4.1f",
      p.amp, p.tau, p.drc, p.bnds, p.resMean, p.resSD, p.aaShape, p.spShape, p.spBias, p.drShape, p.sigm) ;
    if (p.printResults) Rprintf("\n\n");
}

