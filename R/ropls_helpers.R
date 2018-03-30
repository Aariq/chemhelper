#' Extract VIP scores from pls object
#' 
#' Provides a wrapper for `ropls::getVipVn()` that returns a tibble rather than a named numeric vector.
#'
#' @param pls a pls object created by `ropls::opls()`
#'
#' @return a tibble
#' 
#' @import ropls
#' @import tibble
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' pls.model <- opls(X, Y)
#' get_VIP(pls.model)
#' }
get_VIP <- function(pls){
  getVipVn(pls) %>%
    as.data.frame() %>% 
    rownames_to_column() %>% 
    rename(Variable = "rowname", VIP = ".")
}