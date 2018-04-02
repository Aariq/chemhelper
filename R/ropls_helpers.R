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


#' Format numbers correctly as CAS numbers
#'
#'
#' @param x an integer or character vector
#' @importFrom webchem is.cas
#' @import stringr
#' @import purrr
#' @return 
#' @export
#'
#' @examples
#' x <- c(58082, 83341, 12345678, "hexanol")
#' format_CAS(x)

format_CAS <- function(x){
  if(any(!str_detect(x, "^\\d+$"))){
    warning("Some elements of x cannot be converted to CAS numbers")
  }
  match <- str_match(x, pattern = "([0-9]+)([0-9]{2})([0-9]{1})")
  parsed <- paste0(match[,2], "-", match[,3], "-", match[,4])
  pass <- map_lgl(parsed, is.cas)
  out <- ifelse(pass, parsed, NA)
  return(out)
}

