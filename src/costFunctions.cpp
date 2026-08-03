#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// [[Rcpp::export]]
double calculateCostValueRMSE_cpp(List resTh, List resOb) {
  DataFrame th_caf = as<DataFrame>(resTh["caf"]);
  DataFrame ob_caf = as<DataFrame>(resOb["caf"]);
  DataFrame th_delta = as<DataFrame>(resTh["delta"]);
  DataFrame ob_delta = as<DataFrame>(resOb["delta"]);

  NumericVector th_accPerComp = th_caf["accPerComp"];
  NumericVector th_accPerIncomp = th_caf["accPerIncomp"];
  NumericVector ob_accPerComp = ob_caf["accPerComp"];
  NumericVector ob_accPerIncomp = ob_caf["accPerIncomp"];

  NumericVector th_meanComp = th_delta["meanComp"];
  NumericVector th_meanIncomp = th_delta["meanIncomp"];
  NumericVector ob_meanComp = ob_delta["meanComp"];
  NumericVector ob_meanIncomp = ob_delta["meanIncomp"];

  int n_rt = th_delta.nrows() * 2;
  int n_err = th_caf.nrows() * 2;

  double sum_caf_sq = 0.0;
  for (int i = 0; i < th_caf.nrows(); ++i) {
    sum_caf_sq += std::pow(th_accPerComp[i] - ob_accPerComp[i], 2);
    sum_caf_sq += std::pow(th_accPerIncomp[i] - ob_accPerIncomp[i], 2);
  }
  double costCAF = std::sqrt((1.0 / n_err) * sum_caf_sq);

  double sum_rt_sq = 0.0;
  for (int i = 0; i < th_delta.nrows(); ++i) {
    sum_rt_sq += std::pow(th_meanComp[i] - ob_meanComp[i], 2);
    sum_rt_sq += std::pow(th_meanIncomp[i] - ob_meanIncomp[i], 2);
  }
  double costRT = std::sqrt((1.0 / n_rt) * sum_rt_sq);

  double weightRT = (double)n_rt / (n_rt + n_err);
  double weightCAF = (1.0 - weightRT) * 1500.0;

  return (weightCAF * costCAF) + (weightRT * costRT);
}

// [[Rcpp::export]]
double calculateCostValueSPE_cpp(List resTh, List resOb) {
  DataFrame th_caf = as<DataFrame>(resTh["caf"]);
  DataFrame ob_caf = as<DataFrame>(resOb["caf"]);
  DataFrame th_delta = as<DataFrame>(resTh["delta"]);
  DataFrame ob_delta = as<DataFrame>(resOb["delta"]);

  NumericVector th_accPerComp = th_caf["accPerComp"];
  NumericVector th_accPerIncomp = th_caf["accPerIncomp"];
  NumericVector ob_accPerComp = ob_caf["accPerComp"];
  NumericVector ob_accPerIncomp = ob_caf["accPerIncomp"];

  NumericVector th_meanComp = th_delta["meanComp"];
  NumericVector th_meanIncomp = th_delta["meanIncomp"];
  NumericVector th_meanBin = th_delta["meanBin"];
  NumericVector ob_meanComp = ob_delta["meanComp"];
  NumericVector ob_meanIncomp = ob_delta["meanIncomp"];
  NumericVector ob_meanBin = ob_delta["meanBin"];

  double costCAF = 0.0;
  for (int i = 0; i < th_caf.nrows(); ++i) {
    costCAF += std::pow((ob_accPerComp[i] - th_accPerComp[i]) / ob_accPerComp[i], 2);
    costCAF += std::pow((ob_accPerIncomp[i] - th_accPerIncomp[i]) / ob_accPerIncomp[i], 2);
  }

  double costRT = 0.0;
  for (int i = 0; i < th_delta.nrows(); ++i) {
    costRT += std::pow((ob_meanComp[i] - th_meanComp[i]) / ob_meanComp[i], 2);
    costRT += std::pow((ob_meanIncomp[i] - th_meanIncomp[i]) / ob_meanIncomp[i], 2);
    costRT += std::pow((ob_meanBin[i] - th_meanBin[i]) / ob_meanBin[i], 2);
  }

  return costRT + costCAF;
}

double calculate_cs_cpp_helper(const NumericVector& th, const NumericVector& boundary, double trialPropOb, double trialPropTh) {
  if (th.size() == 0 && boundary.size() > 0) return trialPropOb;
  if (th.size() > 0 && boundary.size() == 0) return trialPropTh;
  if (th.size() == 0 && boundary.size() == 0) return 0.0;
  
  std::vector<double> probE = {0.1, 0.2, 0.2, 0.2, 0.2, 0.1};
  for (int i = 0; i < 6; ++i) {
    probE[i] *= trialPropOb;
  }
  
  std::vector<int> counts(6, 0);
  const double* th_ptr = REAL(th);
  int n = th.size();
  for (int i = 0; i < n; ++i) {
    double val = th_ptr[i];
    if (std::isnan(val)) continue;
    int bin = 0;
    while (bin < boundary.size() && val > boundary[bin]) {
      bin++;
    }
    if (bin < 6) {
      counts[bin]++;
    }
  }
  
  int sum_counts = 0;
  for (int c : counts) sum_counts += c;
  
  double cs_sum = 0.0;
  for (int i = 0; i < 6; ++i) {
    double pBin = 0.0;
    if (sum_counts > 0) {
      pBin = ((double)counts[i] / sum_counts) * trialPropTh;
    }
    if (pBin == 0.0) pBin = 0.0001;
    
    cs_sum += std::pow(probE[i] - pBin, 2) / pBin;
  }
  return cs_sum;
}

double calculate_gs_cpp_helper(const NumericVector& th, const NumericVector& boundary, double trialPropOb, double trialPropTh) {
  if (th.size() == 0 && boundary.size() > 0) return trialPropOb;
  if (th.size() > 0 && boundary.size() == 0) return trialPropTh;
  if (th.size() == 0 && boundary.size() == 0) return 0.0;
  
  std::vector<double> probE = {0.1, 0.2, 0.2, 0.2, 0.2, 0.1};
  for (int i = 0; i < 6; ++i) {
    probE[i] *= trialPropOb;
  }
  
  std::vector<int> counts(6, 0);
  const double* th_ptr = REAL(th);
  int n = th.size();
  for (int i = 0; i < n; ++i) {
    double val = th_ptr[i];
    if (std::isnan(val)) continue;
    int bin = 0;
    while (bin < boundary.size() && val > boundary[bin]) {
      bin++;
    }
    if (bin < 6) {
      counts[bin]++;
    }
  }
  
  int sum_counts = 0;
  for (int c : counts) sum_counts += c;
  
  double gs_sum = 0.0;
  for (int i = 0; i < 6; ++i) {
    double pBin = 0.0;
    if (sum_counts > 0) {
      pBin = ((double)counts[i] / sum_counts) * trialPropTh;
    }
    if (pBin == 0.0) pBin = 0.0001;
    
    gs_sum += std::abs(probE[i] * std::log(probE[i] / pBin));
  }
  return gs_sum;
}

void extract_boundaries_and_ntrials(const DataFrame& prob_df, 
                                    NumericVector& bound_comp_cor, NumericVector& bound_comp_err,
                                    NumericVector& bound_incomp_cor, NumericVector& bound_incomp_err,
                                    double& nCompCorrectOb, double& nCompErrorOb,
                                    double& nIncompCorrectOb, double& nIncompErrorOb) {
  CharacterVector prob_comp = prob_df["Comp"];
  IntegerVector prob_error = prob_df["Error"];
  NumericVector prob_nTrials = prob_df["nTrials"];
  NumericVector prob_boundary = prob_df["boundary"];
  
  std::vector<double> v_bound_comp_cor, v_bound_comp_err, v_bound_incomp_cor, v_bound_incomp_err;
  bool found_comp_cor = false, found_comp_err = false, found_incomp_cor = false, found_incomp_err = false;
  
  nCompCorrectOb = 0; nCompErrorOb = 0; nIncompCorrectOb = 0; nIncompErrorOb = 0;
  
  for (int i = 0; i < prob_df.nrows(); ++i) {
    std::string comp_str = as<std::string>(prob_comp[i]);
    int err = prob_error[i];
    if (comp_str == "comp" && err == 0) {
      if (!found_comp_cor && !NumericVector::is_na(prob_nTrials[i])) { nCompCorrectOb = prob_nTrials[i]; found_comp_cor = true; }
      if (!NumericVector::is_na(prob_boundary[i])) v_bound_comp_cor.push_back(prob_boundary[i]);
    } else if (comp_str == "comp" && err == 1) {
      if (!found_comp_err && !NumericVector::is_na(prob_nTrials[i])) { nCompErrorOb = prob_nTrials[i]; found_comp_err = true; }
      if (!NumericVector::is_na(prob_boundary[i])) v_bound_comp_err.push_back(prob_boundary[i]);
    } else if (comp_str == "incomp" && err == 0) {
      if (!found_incomp_cor && !NumericVector::is_na(prob_nTrials[i])) { nIncompCorrectOb = prob_nTrials[i]; found_incomp_cor = true; }
      if (!NumericVector::is_na(prob_boundary[i])) v_bound_incomp_cor.push_back(prob_boundary[i]);
    } else if (comp_str == "incomp" && err == 1) {
      if (!found_incomp_err && !NumericVector::is_na(prob_nTrials[i])) { nIncompErrorOb = prob_nTrials[i]; found_incomp_err = true; }
      if (!NumericVector::is_na(prob_boundary[i])) v_bound_incomp_err.push_back(prob_boundary[i]);
    }
  }
  
  bound_comp_cor = wrap(v_bound_comp_cor);
  bound_comp_err = wrap(v_bound_comp_err);
  bound_incomp_cor = wrap(v_bound_incomp_cor);
  bound_incomp_err = wrap(v_bound_incomp_err);
}

// [[Rcpp::export]]
double calculateCostValueCS_cpp(List resTh, List resOb) {
  List sim = as<List>(resTh["sim"]);
  NumericVector rts_comp = sim["rts_comp"];
  NumericVector rts_incomp = sim["rts_incomp"];
  NumericVector errs_comp = sim["errs_comp"];
  NumericVector errs_incomp = sim["errs_incomp"];
  DataFrame prob_df = as<DataFrame>(resOb["prob"]);
  
  NumericVector bound_comp_cor, bound_comp_err, bound_incomp_cor, bound_incomp_err;
  double nCompCorrectOb, nCompErrorOb, nIncompCorrectOb, nIncompErrorOb;
  extract_boundaries_and_ntrials(prob_df, bound_comp_cor, bound_comp_err, bound_incomp_cor, bound_incomp_err, 
                                 nCompCorrectOb, nCompErrorOb, nIncompCorrectOb, nIncompErrorOb);
  
  double nCompCorrectTh = rts_comp.size();
  double nCompErrorTh = errs_comp.size();
  double nIncompCorrectTh = rts_incomp.size();
  double nIncompErrorTh = errs_incomp.size();
  
  double nCompTotalOb = nCompCorrectOb + nCompErrorOb;
  double nCompTotalTh = nCompCorrectTh + nCompErrorTh;
  double nIncompTotalOb = nIncompCorrectOb + nIncompErrorOb;
  double nIncompTotalTh = nIncompCorrectTh + nIncompErrorTh;
  
  double cs_comp_correct = calculate_cs_cpp_helper(rts_comp, bound_comp_cor, 
                            nCompTotalOb > 0 ? nCompCorrectOb/nCompTotalOb : 0, 
                            nCompTotalTh > 0 ? nCompCorrectTh/nCompTotalTh : 0);
  if (nCompCorrectOb != 0) cs_comp_correct *= nCompCorrectOb;
  
  double cs_comp_error = calculate_cs_cpp_helper(errs_comp, bound_comp_err, 
                            nCompTotalOb > 0 ? nCompErrorOb/nCompTotalOb : 0, 
                            nCompTotalTh > 0 ? nCompErrorTh/nCompTotalTh : 0);
  if (nCompErrorOb != 0) cs_comp_error *= nCompErrorOb;
  
  double cs_incomp_correct = calculate_cs_cpp_helper(rts_incomp, bound_incomp_cor, 
                            nIncompTotalOb > 0 ? nIncompCorrectOb/nIncompTotalOb : 0, 
                            nIncompTotalTh > 0 ? nIncompCorrectTh/nIncompTotalTh : 0);
  if (nIncompCorrectOb != 0) cs_incomp_correct *= nIncompCorrectOb;
  
  double cs_incomp_error = calculate_cs_cpp_helper(errs_incomp, bound_incomp_err, 
                            nIncompTotalOb > 0 ? nIncompErrorOb/nIncompTotalOb : 0, 
                            nIncompTotalTh > 0 ? nIncompErrorTh/nIncompTotalTh : 0);
  if (nIncompErrorOb != 0) cs_incomp_error *= nIncompErrorOb;
  
  double total_cs = 0;
  if (!std::isnan(cs_comp_correct)) total_cs += cs_comp_correct;
  if (!std::isnan(cs_comp_error)) total_cs += cs_comp_error;
  if (!std::isnan(cs_incomp_correct)) total_cs += cs_incomp_correct;
  if (!std::isnan(cs_incomp_error)) total_cs += cs_incomp_error;
  
  return total_cs;
}

// [[Rcpp::export]]
double calculateCostValueGS_cpp(List resTh, List resOb) {
  List sim = as<List>(resTh["sim"]);
  NumericVector rts_comp = sim["rts_comp"];
  NumericVector rts_incomp = sim["rts_incomp"];
  NumericVector errs_comp = sim["errs_comp"];
  NumericVector errs_incomp = sim["errs_incomp"];
  DataFrame prob_df = as<DataFrame>(resOb["prob"]);
  
  NumericVector bound_comp_cor, bound_comp_err, bound_incomp_cor, bound_incomp_err;
  double nCompCorrectOb, nCompErrorOb, nIncompCorrectOb, nIncompErrorOb;
  extract_boundaries_and_ntrials(prob_df, bound_comp_cor, bound_comp_err, bound_incomp_cor, bound_incomp_err, 
                                 nCompCorrectOb, nCompErrorOb, nIncompCorrectOb, nIncompErrorOb);
  
  double nCompCorrectTh = rts_comp.size();
  double nCompErrorTh = errs_comp.size();
  double nIncompCorrectTh = rts_incomp.size();
  double nIncompErrorTh = errs_incomp.size();
  
  double nCompTotalOb = nCompCorrectOb + nCompErrorOb;
  double nCompTotalTh = nCompCorrectTh + nCompErrorTh;
  double nIncompTotalOb = nIncompCorrectOb + nIncompErrorOb;
  double nIncompTotalTh = nIncompCorrectTh + nIncompErrorTh;
  
  double gs_comp_correct = calculate_gs_cpp_helper(rts_comp, bound_comp_cor, 
                            nCompTotalOb > 0 ? nCompCorrectOb/nCompTotalOb : 0, 
                            nCompTotalTh > 0 ? nCompCorrectTh/nCompTotalTh : 0);
  double gs_comp_error = calculate_gs_cpp_helper(errs_comp, bound_comp_err, 
                            nCompTotalOb > 0 ? nCompErrorOb/nCompTotalOb : 0, 
                            nCompTotalTh > 0 ? nCompErrorTh/nCompTotalTh : 0);
  
  double gs_incomp_correct = calculate_gs_cpp_helper(rts_incomp, bound_incomp_cor, 
                            nIncompTotalOb > 0 ? nIncompCorrectOb/nIncompTotalOb : 0, 
                            nIncompTotalTh > 0 ? nIncompCorrectTh/nIncompTotalTh : 0);
  double gs_incomp_error = calculate_gs_cpp_helper(errs_incomp, bound_incomp_err, 
                            nIncompTotalOb > 0 ? nIncompErrorOb/nIncompTotalOb : 0, 
                            nIncompTotalTh > 0 ? nIncompErrorTh/nIncompTotalTh : 0);
  
  double gs_comp_total = nCompTotalOb * (gs_comp_correct + gs_comp_error);
  double gs_incomp_total = nIncompTotalOb * (gs_incomp_correct + gs_incomp_error);
  
  return 2.0 * (gs_comp_total + gs_incomp_total);
}
