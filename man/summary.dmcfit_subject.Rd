% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary.dmcfit_subject}
\alias{summary.dmcfit_subject}
\title{summary.dmcfit_subject: dmcfit individual subject}
\usage{
\method{summary}{dmcfit_subject}(object, digits = 2, ...)
}
\arguments{
\item{object}{Output from dmcFitSubject}

\item{digits}{Number of digits in the output}

\item{...}{pars}
}
\value{
DataFrame
}
\description{
Summary of the simulation results from dmcFitSubjectX
}
\examples{
\donttest{
# Example 1
fitSubject <- dmcFitSubject(flankerData, nTrl = 1000, subjects = c(1:3))
summary(fitSubject)
}

}
