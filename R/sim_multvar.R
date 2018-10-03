#' Construct simulated multivariate data sets
#'
#' @description Constructs multivariate data sets with complex structure.  This function allows for a combination of uncorrelated variables, variables that covary, but are not related to group membership, and variables that contribute to discrimination among groups.
#' 
#'
#' @param p_uncorvar The number of uncorrelated variables to create.
#' @param p_corvar The number of correlated, but not associated with group membership, variables to create.
#' @param p_discr The number of discriminating variables to create.  The means of these will differ among groups by `diff_discr`.
#' @param cov_corvar The covariance for the correlated variables
#' @param cov_discr The covariance for discriminating variables *within each group* (see details)
#' @param diff_discr The difference in means among groups
#' @param group_variance A vector the same length as number of groups (currently only 2) that specifies variances for each group.
#' @param N Number of observations or rows to simulate
#' @param seed A seed for the random number generator to use.  If `NA` is supplied, a random seed will be generated.
#'
#' @details This function builds a multivariate dataset with up to three different types of variable group---variables with no covariance (actually cov = 0.01 to help avoid making non-positive-definite matrices), variables with covariance (adjusted using `cov_corvar`) but that do not contribute to group separation, and variables that are constructed with means that differ between groups by a total of `diff_discr`.  `group_variance` applies only to the last two sets of variables and allows specification of different variances (diagonals in the variance-covariance matrix) between groups.
#' 
#' **NOTE:** While `cov_discr` supplies a covariance to `mvrnorm`, the actual covarince of these variables also depends on `diff_discr`.  It is reccomended that the correlation matrix of the final dataset is inspected to ensure you're getting the correlation structure you want.  For example, you may need to reduce `cov_discr` relative `cov_corvar` to get these two sets of variables to have similar correlations.
#'
#' @return A dataframe with one character column for group membership and columns for each data type.
#' @import dplyr
#' @import tibble
#' @importFrom MASS mvrnorm
#' @export
#'
#' @examples
#' 
#' df <- sim_multvar(p_uncorvar = 6, p_corvar = 6, p_discr = 6, N = 40)
#' library(dplyr)
#' df %>% select(-group) %>% as.matrix() %>% cor() %>% heatmap(Rowv = NA, Colv = NA)
sim_multvar <- function(p_uncorvar,
                        p_corvar,
                        p_discr,
                        cov_corvar = 0.5,
                        cov_discr = 0.5,
                        diff_discr = 1,
                        group_variance = c(1, 1),
                        N,
                        seed = NA){
  .Deprecated("sim_*()")
  # Choose a random number for seed if none is supplied
  if(is.na(seed)){
    seed = as.integer(runif(1) * 10e6)
  }
  # make uncorrelated variables
  if(p_uncorvar > 0){
    S_uncor <- matrix(rep(0.001, p_uncorvar^2), ncol = p_uncorvar) #variance-covariance matrix
    diag(S_uncor) <- 1 #variance = 1 for now.  Might change later to make this an argument.
    set.seed(seed)
    data_uncor <- MASS::mvrnorm(n = N, Sigma = S_uncor, mu = rep(0, p_uncorvar)) %>%
      as.data.frame()
    colnames(data_uncor) <- paste0("uncor_", 1:p_uncorvar)
  } else {data_uncor <- list()}
  
  # make covarying variables
  if(p_corvar > 0){
    S_corr <- matrix(rep(cov_corvar, p_corvar^2), ncol = p_corvar)
    S_corr_B <- S_corr_A <- S_corr
    diag(S_corr_A) <- group_variance[1]
    diag(S_corr_B) <- group_variance[2]
    
    set.seed(seed+1)
    corr_A <- MASS::mvrnorm(n = N/2, Sigma = S_corr_A, mu = rep(0, p_corvar)) %>% 
      as.data.frame()
    corr_B <- MASS::mvrnorm(n = N/2, Sigma = S_corr_B, mu = rep(0, p_corvar)) %>% 
      as.data.frame()
    
    data_corr <- dplyr::bind_rows(corr_A, corr_B)
    colnames(data_corr) <- paste0("corr_", 1:p_corvar)
  } else {data_corr <- list()}
  
  # make discriminating data
  if(p_discr > 0){
    S_disc <- matrix(rep(cov_discr, p_discr^2), ncol = p_discr)
    
    # make data for group A
    diag(S_disc) <- group_variance[1]
    set.seed(seed+2)
    means_A <- rep((diff_discr/2), p_discr)
    data_A <- MASS::mvrnorm(n = N/2, mu = means_A, Sigma = S_disc) %>%
      as.data.frame()
    
    # make data for group B
    diag(S_disc) <- group_variance[2]
    set.seed(seed+3)
    means_B <- rep(-1*(diff_discr/2), p_discr)
    data_B <- MASS::mvrnorm(n = N/2, mu = means_B, Sigma = S_disc) %>%
      as.data.frame()
    
    data_disc <- dplyr::bind_rows(data_A, data_B)
    colnames(data_disc) <- paste0("disc_", 1:p_discr)
  } else{data_disc <- list()}
  
  # column bind it all together
  sim.data <- dplyr::bind_cols(data_disc, data_corr, data_uncor) %>%
    tibble::add_column(group = rep(c("a","b"), each = N/2), .before = 1)
  
  return(sim.data)
}
# TO DO: Implement a n_groups argument.  Warn if fewer than 5 samples per group. Warn if group size is uneven due to rounding. Error if N/n_groups < 3
