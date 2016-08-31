#' @title MDES Random Assignment with clustering
#' @param sig.lvl Level of significance (default 0.05).
#' @param power Power (default 0.80).
#' @param N Total number of individuals in the sample.
#' @param g Number of clusters.
#' @param p Probability of assignment to the treatment group (default 0.5).
#' @param outcome \code{"binary"} or \code{"continuous"}.
#' @param mean_y Mean of the outcome variable (required for binary outcomes)
#' @param sigma_y standard deviation of the outcome (required for continious outcomes).
#' @param rho Intraclass correlation coefficient (default 0.02)
#' @param R_sq_WG Proportion of the within-group variance of outcome y explained by covariates x (default 0).
#' @param R_sq_BG Proportion of the group-level variance of outcome y explained by covariates x (default 0).
#' @return A dataframe with \code{MDES} and \code{MDE}.
#' @examples 
#' MDES_CL(N=5000, g = 20, outcome = "continuous", sigma_y = 0.1, rho = 0.01)
#' MDES_CL(N=5000, g = 20, outcome = "binary", mean_y = 0.5, rho = 0.01)
#' @export
#' 
MDES_CL = function (sig.lvl = 0.05, power = 0.80, N, g, p = 0.5, 
                    outcome = c("binary", "continuous"),  mean_y, sigma_y, 
                    rho = 0.02, R_sq_WG = 0, R_sq_BG = 0){
  if(outcome=="continuous" & missing(sigma_y)){
    stop("When outcome is continuous you have to specify sigma_y")
  }
  
  if(outcome=="binary" & missing(mean_y)){
    stop("When outcome is binary you have to specify mean_y")
  }
  df = g-2
  Factor = qt(p = 1-sig.lvl/2, df = df) + 
    qt(p = power, df = df)
  m = N/g
  
  if(outcome=="binary"){
    sigma_y=sqrt(mean_y*(1-mean_y))
  }  
  A = 1/(p*(1-p))
  B = ((1-rho)*(1-R_sq_WG))/(g*m)
  C = (rho*(1-R_sq_BG))/g
  MDES = Factor * sqrt(A*(B+C)) 
  MDE = MDES*sigma_y
  
  df1 = data.frame(MDES, MDE)
  return(df1)
} 