#' Parse IonAnalytics CSV files
#'
#'
#' @param file a path to a csv file exported by IonAnalytics
#'
#' @return a string.
#'
#' @import dplyr
#' @import stringr
#' @export
#'
#' @examples
#' \dontrun{
#' parse_IA("report.csv")
#' }
#' 
parse_IA <- function(file){
  parsed_IA <-file %>%
    str_replace_all("^.+\r\n", "") %>% #remove first line
    str_replace_all("(?<!\r)\n", "") %>% #remove line breaks within headers (\n but now \r\n)
    str_replace_all("\r\n", "\n") #convert weird windows linebreaks (\r\n) to regular \n
  return(parsed_IA)
}

#' Read IonAnalytics CSV files
#'
#' Reads csv files exported from IonAnalytics methods or integration reports.
#'
#' @param file a path to a csv file exported by IonAnalytics
#'
#' @return a dataframe
#' @import dplyr
#' @import readr
#' @export
#'
#' @examples
#' \dontrun{
#' read_IA("report.csv")
#' }
read_IA <- function(file){
  output <- readr::read_file(file) %>%
    parse_IA() %>%
    readr::read_csv()
  return(output)
}
