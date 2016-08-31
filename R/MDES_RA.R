#' @title MDES Random Assignment
#' @param sig.lvl Level of significance (default 0.05).
#' @param power Power (default 0.80).
#' @param N Total number of individuals in the sample.
#' @param p Probability of assignment to the treatment group (default 0.5).
#' @param outcome \code{"binary"} or \code{"continuous"}.
#' @param mean_y Mean of the outcome variable (required for binary outcomes)
#' @param sigma_y standard deviation of the outcome (required for continious outcomes).
#' @param R_sq_xy The proportion of the variance of outcome y explained by covariates x (default 0).
#' @return A dataframe with \code{MDES} and \code{MDE}.
#' @examples 
#' MDES_RA(N=5000, outcome = "continuous", sigma_y = 0.1)
#' MDES_RA(N=5000, outcome = "binary", mean_y = 0.5)
#' @export
#' 
MDES_RA = function (sig.lvl = 0.05, power = 0.80, N, p=0.5, 
                    outcome = c("binary", "continuous"), mean_y, sigma_y, 
                    R_sq_xy = 0){
  
  if(outcome=="continuous" & missing(sigma_y)){
    stop("When outcome is continuous you have to specify sigma_y")
  }
  if(outcome=="binary" & missing(mean_y)){
    stop("When outcome is binary you have to specify mean_y")
  }
  df = N-2
  Factor = qt(p = 1-sig.lvl/2, df = df) + qt(p = power, df = df)
  
  if(outcome=="continuous"){
    sigmasqy = sigma_y*sigma_y
    sigma_lambdaRA_hat =sqrt( sigmasqy*(1-R_sq_xy)/(N*p*(1-p)))
    MDE = Factor * sigma_lambdaRA_hat
    MDES = MDE / sigma_y    
  }else{
    std.dev=sqrt(mean_y*(1-mean_y))
    sigmasqy = std.dev*std.dev
    sigma_lambdaRA_hat =sqrt( sigmasqy*(1-R_sq_xy)/(N*p*(1-p)))
    MDE = Factor * sigma_lambdaRA_hat
    MDES = MDE / std.dev    
  }
  df1 = data.frame(MDES, MDE)
  return(df1)
} 