#' Simulate a dataset with a given number of observations and groups
#'
#' @description This is a simple wrapper that creates a tibble of length `N` with a single column `groups`.  It will warn if there are fewer than three replicates per group.
#' @param N Total number of observations/rows
#' @param n_groups How many groups or treatments to simulate
#'
#' @return a tibble
#' 
#' @import tibble
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' df <- sim_df(30, 3)
sim_df <- function(N, n_groups) {
  #stopifnots for group size.  Groups should have at least 3 obserations, warn if less than 5.
  if(N/n_groups < 3){
    stop("Not enough replicates per group")
  }
  if(N/n_groups < 5){
    warning("Fewer than 5 replicates per group")
  }
  df <- tibble(group = rep(letters[1:n_groups], length.out = N)) %>% arrange(group)
  return(df)
}



set_diag <- function(x, value){
  diag(x) <- value
  return(x)
}



#' Simulate co-varying variables
#'
#' @param df a dataframe or tibble
#' @param p number of variables to simulate
#' @param var variance used to construct variance-covarinace matrix.
#' @param cov covariance used to construct variance-covarinace matrix.
#' @param name an optional name to be appended to the column names in the output
#' @param seed an optional seed for random number generation.  If `NA` a random seed will be used.
#'
#' @return a tibble
#' 
#' @importFrom MASS mvrnorm
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' df <- sim_df(30, 3)
#' sim_covar(df, p = 5, var = 1, cov = 0.5, name = "correlated")

sim_covar <- function(df, p, var, cov, name = NA, seed = NA) {
  stopifnot(is.data.frame(df))
  if(length(cov) != 1){
    stop("Error: length(cov) not equal to 1")
  }
  if(is.na(seed)){
    seed = as.integer(runif(1) * 10e6)
  }
  N <- nrow(df)
  S <- matrix(rep(cov, p^2), ncol = p) %>% set_diag(var)
  
  set.seed(seed)
  sim_data <- MASS::mvrnorm(n = N, Sigma = S, mu = rep(0, p)) %>%
    as.data.frame()
  
  if(!is.na(name)){
    colnames(sim_data) <- paste0(name, "_", 1:p)
  }
  new_df <- bind_cols(df, sim_data)
  return(new_df)
}



#' Simulate co-varying variables with different means by group.
#'
#' To-do: make this work with `dplyr::group_by()` instead of `group =`
#'
#' @param df a dataframe containing a grouping variable column
#' @param p number of variables to simulate
#' @param var variance used to construct variance-covarinace matrix.
#' @param cov covariance used to construct variance-covarinace matrix.
#' @param group_means a vector of the same length as the number of grouping variables
#' @param group specify the name of the column containing grouping variables
#' @param name an optional name to be appended to the column names in the output
#' @param seed an optional seed for random number generation.  If `NA` a random seed will be used.
#' 
#' @return a tibble
#' 
#' @importFrom MASS mvrnorm
#' @import dplyr
#' @import purrr
#' 
#' @export
#'
#' @examples
#' df <- sim_df(30, 3)
#' sim_discr(df, p = 5, var = 1, cov = 0.5, group_means = c(-1, 0, 1), name = "descr")
sim_discr <- function(df, p, var, cov, group_means, group = "group", name = NA, seed = NA){
  if(is.na(seed)){
    seed = as.integer(runif(1) * 10e6)
  }
  N <- nrow(df)
  group_var <- sym(group)
  n_groups <- select(df, !!group_var) %>% unique() %>% nrow()
  df <- arrange(df, !!group_var)
  len_groups <- df %>% 
    group_by(!!group_var) %>% 
    summarise(lens = n()) %>% 
    .$lens
  if(length(group_means) != n_groups){
    stop(paste("Please supply means for all", n_groups, "groups."))
  }
  if(length(var) != 1 & length(var) != n_groups){
    stop("length(var) should be equal to 1 or n_groups")
  }
  if(length(cov) !=1 & length(cov) != n_groups){
    stop("length(cov) should be equal to 1 or n_groups")
  }
  if(length(var) == 1){
    Var <- rep(var, n_groups)
  }
  if(length(cov) == 1){
    Cov <- rep(cov, n_groups)
  }
  
  S.list <- purrr::map2(Cov, Var,
                        ~matrix(rep(.x, p^2), ncol = p) %>%
                          set_diag(.y))
  
  l <- list(S = S.list, n = len_groups, mu = group_means)
  set.seed(seed)
  sim_data <- pmap_df(l, .f = function(S, n, mu) as.data.frame(MASS::mvrnorm(n = n, Sigma = S, mu = rep(mu, p))))
  
  if(!is.na(name)){
    colnames(sim_data) <- paste0(name, "_", 1:p)
  }
  new_df <- bind_cols(df, sim_data)
  return(new_df)
}

