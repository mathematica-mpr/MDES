% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MDES_MCG_CL.R
\name{MDES_MCG_CL}
\alias{MDES_MCG_CL}
\title{MDES Matched Comparison Group with clustering}
\usage{
MDES_MCG_CL(sig.lvl = 0.05, power = 0.8, N, g, p = 0.5,
  outcome = c("binary", "continuous"), mean_y, sigma_y, rho = 0.02,
  R_sq_WG = 0, R_sq_BG = 0, R_sq_xt = 0.15)
}
\arguments{
\item{sig.lvl}{Level of significance (default 0.05).}

\item{power}{Power (default 0.80).}

\item{N}{Total number of individuals in the sample.}

\item{g}{Number of clusters.}

\item{p}{Probability of assignment to the treatment group (default 0.5).}

\item{outcome}{\code{"binary"} or \code{"continuous"}.}

\item{mean_y}{Mean of the outcome variable (required for binary outcomes)}

\item{sigma_y}{standard deviation of the outcome (required for continious outcomes).}

\item{rho}{Intraclass correlation coefficient (default 0.02)}

\item{R_sq_WG}{Proportion of the within-group variance of outcome y explained by covariates x (default 0).}

\item{R_sq_BG}{Proportion of the group-level variance of outcome y explained by covariates x (default 0).}

\item{R_sq_xt}{Proportion of the variance of the treatment indicator explained by covariates x (default 0.15).}
}
\value{
A dataframe with \code{MDES} and \code{MDE}.
}
\examples{
MDES_MCG_CL(N=5000, g = 20,  outcome = "continuous", sigma_y = 0.1, rho = 0.02, R_sq_xt = 0.15)
MDES_MCG_CL(N=5000, g = 20, outcome = "binary", mean_y = 0.5, rho = 0.02, R_sq_xt = 0.15)
}

