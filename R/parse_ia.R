#' Parse IonAnalytics CSV files
#'
#' @param file raw text
#'
#' @return a string.
#'
#' @import dplyr
#' @import stringr
#' 
#'
#' @examples
#' \dontrun{
#' parse_IA("report.csv")
#' }
#' 
parse_IA <- function(file){
  parsed_IA <- file %>%
    str_replace_all("^.+\r\n", "") %>% #remove first line
    str_replace_all("(?<!\r)\n", "") %>% #remove line breaks within headers (\n but now \r\n)
    str_replace_all("\r\n", "\n") #convert weird windows linebreaks (\r\n) to regular \n
  return(parsed_IA)
}

#' Read IonAnalytics CSV files
#'
#' @description Reads csv files exported from IonAnalytics methods or integration reports.
#' These csv files are poorly formatted and include line breaks within headers so `read_csv()` doesn't work
#'
#' @param file a path to a csv file exported by IonAnalytics
#'
#' @return A dataframe
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
