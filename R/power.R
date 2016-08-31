#' @title MDES and MDE Calculator
#' @param type \code{c("Random Assigment", "MCG", "DID")}.
#' @param clusters \code{c(TRUE, FALSE)}
#' @param outcome \code{c("binary", "continuous")}
#' @param sig.lvl Level of significance (default 0.05).
#' @param power Power (default 0.80).
#' @param N Total number of individuals in the sample.
#' @param g Number of clusters.
#' @param p Probability of assignment to the treatment group (default 0.5).
#' @param mean_y Mean of the outcome variable (required for binary outcomes)
#' @param sigma_y standard deviation of the outcome (required for continious outcomes).
#' @param rho Intraclass correlation coefficient (default 0.02).
#' @param R_sq_WG Proportion of the within-group variance of outcome y explained by covariates x (default 0).
#' @param R_sq_BG Proportion of the group-level variance of outcome y explained by covariates x (default 0).
#' @param R_sq_xy The proportion of the variance of outcome y explained by covariates x (default 0).
#' @param R_sq_xyt Proportion of the individual-level variance of outcome y explained by covariates x (default 0).
#' @param R_sq_xt Proportion of the variance of the treatment indicator explained by covariates x (default 0.15).
#' @param theta The proportion of the outcome y variance due to persistent factors (0 <= theta <= 1). If the pre- and pos-observations are independent, then theta = 0.
#' @return A dataframe with \code{MDES} and \code{MDE}.
#' @examples 
#' power(type = 'Random Assigment', clusters = FALSE, outcome = 'continuous', 
#' sig.lvl = .05, power = .8, N = 5000, p = 0.5, sigma_y = 0.1, R_sq_xy = 0)
#' 
#' power(type = 'Random Assigment', clusters = TRUE, outcome = 'continuous',
#' sig.lvl = .05, power = .8, N = 5000, g = 20, p = .5,
#' sigma_y = 0.1, rho = 0.02, R_sq_WG = 0, R_sq_BG = 0)
#' 
#' power(type = 'MCG', clusters = FALSE, outcome = 'continuous', sig.lvl = .05, 
#' power = .8, N = 5000, p = .5, sigma_y = 0.1, rho = 0.02, R_sq_xyt = 0, 
#' R_sq_xt = 0)
#' 
#' power(type = 'MCG', clusters = TRUE, outcome = 'continuous', sig.lvl = .05,
#' power = .8, N = 5000, g = 20, p = .5, sigma_y = 0.1, rho = 0.02,
#' R_sq_WG = 0, R_sq_xt = 0, R_sq_BG = 0)
#' 
#' power(type = 'DID', clusters = FALSE, outcome = 'continuous', sig.lvl = .05,
#' power = .8, N = 5000, p = .5, sigma_y = 0.1, rho = 0.02, R_sq_WG = 0, 
#' R_sq_xt = 0, theta = 0)
#' 
#' power(type = 'DID', clusters = TRUE, outcome = 'continuous', sig.lvl = .05, 
#' power = .8, N = 5000, g = 20, p = .5, sigma_y = 0.1, rho = 0.02, 
#' R_sq_WG = 0, R_sq_xt = 0, theta = 0, R_sq_BG = 0)
#' @export
#' 

power <-
  function(type = c("Random Assigment", "MCG", "DID"),
           clusters = c(TRUE, FALSE),
           outcome = c("binary", "continuous"),
           sig.lvl = 0.05, power = 0.8, N, g, p = 0.5, mean_y, sigma_y,
           rho = 0.02, R_sq_WG = 0, R_sq_BG = 0, R_sq_xy = 0, R_sq_xyt = 0,	
           R_sq_xt = 0.15, theta) {
    if (missing(type)) {
      stop("You must set the type variable")
    }
    ifelse(
      test = (type == "Random Assigment" & clusters == FALSE),
      yes = x <-
        MDES_RA(sig.lvl, power, N, p, outcome,  mean_y, sigma_y, R_sq_xy),
      no = ifelse(
        test = (type == "Random Assigment" & clusters == TRUE),
        yes = x <-
          MDES_CL(
            sig.lvl, power, N, g, p, outcome,  mean_y, sigma_y, rho, R_sq_WG, R_sq_BG
          ),
        no = ifelse(
          test = (type == "MCG" & clusters == FALSE),
          yes = x <-
            MDES_MCG(
              sig.lvl, power, N, p, outcome, mean_y, sigma_y, R_sq_xyt, R_sq_xt
            ),
          no = ifelse(
            test = (type == "MCG" & clusters == TRUE),
            yes = x <- MDES_MCG_CL(
              sig.lvl, power, N, g, p, outcome,  mean_y, sigma_y, rho, R_sq_WG, R_sq_BG, R_sq_xt
            ),
            no = ifelse(
              test = (type == "DID" & clusters == FALSE),
              yes = x <-
                MDES_DID(
                  sig.lvl, power, N, p, outcome,  mean_y, sigma_y, R_sq_WG, R_sq_xt, theta
                ),
              no = ifelse(
                test = (type == "DID" & clusters == TRUE),
                yes = x <-
                  MDES_DID_CL(
                    sig.lvl, power, N, g, p, outcome,  mean_y, sigma_y, rho, R_sq_WG, R_sq_BG, R_sq_xt, theta
                  ),
                no = x <- NA
              )
            )
          )
        )
      )
    )
    cat('The MDE is', round(x$MDE,3), '. That is, ', round(x$MDES,3), 'SD.', '\n')
    return(x)
  }



