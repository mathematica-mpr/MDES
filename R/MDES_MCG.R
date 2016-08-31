#' @title MDES Matched Comparison Group
#' @param sig.lvl Level of significance (default 0.05).
#' @param power Power (default 0.80).
#' @param N Total number of individuals in the sample.
#' @param p Probability of assignment to the treatment group (default 0.5).
#' @param outcome \code{"binary"} or \code{"continuous"}.
#' @param mean_y Mean of the outcome variable (required for binary outcomes)
#' @param sigma_y standard deviation of the outcome (required for continious outcomes).
#' @param R_sq_xyt Proportion of the individual-level variance of outcome y explained by covariates x (default 0).
#' @param R_sq_xt Proportion of the variance of the treatment indicator explained by covariates x (default 0.15).
#' @return A dataframe with \code{MDES} and \code{MDE}.
#' @examples 
#' MDES_MCG(N=5000, outcome = "continuous", sigma_y = 0.1, R_sq_xt = 0.15)
#' MDES_MCG(N=5000, outcome = "binary", mean_y = 0.5, R_sq_xt = 0.15)
#' @export
#' 
MDES_MCG = function (sig.lvl = 0.05, power = 0.80, N, p = 0.5, 
                     outcome = c("binary", "continuous"),  mean_y, sigma_y, 
                     R_sq_xyt = 0, R_sq_xt = 0.15){
  if(outcome=="continuous" & missing(sigma_y)){
    stop("When outcome is continuous you have to specify sigma_y")
  }
  
  if(outcome=="binary" & missing(mean_y)){
    stop("When outcome is binary you have to specify mean_y")
  }  
  df = N-2
  Factor = qt(p = 1-sig.lvl/2, df = df) + 
    qt(p = power, df = df)
  if(outcome=="binary"){
    sigma_y=sqrt(mean_y*(1-mean_y))
  }  
  sigmasqy = sigma_y*sigma_y
  Numerator = sigmasqy*(1-R_sq_xyt)
  Denominator = (1-R_sq_xt)*N*p*(1-p)
  MDE = Factor * sqrt(Numerator/Denominator)
  MDES = MDE / sigma_y
  df1 = data.frame(MDES, MDE)
  return(df1)
}