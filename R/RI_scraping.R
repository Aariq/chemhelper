#' Read HTML slowly
#' @description Just adds a 5 second rest after `read_html()`.  Useful for web-scraping.
#' @importFrom xml2 read_html
#' @param x a URL
#' @param ... currently unused
#'
#' @return html
#'
#' @examples
#' read_html_slow("https://cran.r-project.org/")
read_html_slow <- function(x, ...){
  output <- xml2::read_html(x)
  Sys.sleep(5)
  return(output)
}



#' Scrape Retention Indices from NIST
#'
#' @param cas cas number
#' @param type what kind of RI
#' @param polarity polar or non-polar
#' @param temp_prog what kind of temperature program
#' 
#' @import rvest
#' @return a table
#'
#' @examples
#' get_RI_tables("78-70-6", "alkane", "non-polar", "custom")
get_RI_tables <- function(cas, type = c("kovats", "linear", "alkane"), polarity = c("polar", "non-polar"), temp_prog = c("isothermal", "ramp", "custom")){
  type_str <- toupper(paste(type, "RI", polarity, temp_prog, sep = "-"))
  URL_detail <- paste0("https://webbook.nist.gov/cgi/cbook.cgi?ID=C",
                       str_remove_all(cas, "-"),
                       "&Units=SI&Mask=2000&Type=",
                       type_str)
  page <- read_html_slow(URL_detail)
  #put in some sort of error cheking here to make sure the page exists!
  tables <- rvest::html_nodes(page, ".data")
  attr(tables, "type") <- type
  attr(tables, "polarity") <- polarity
  attr(tables, "temp_prog") <- temp_prog
  return(tables)
}



#' custom tidier for RI tables with custom temp programs
#'
#' @param table 
#'
#' @return table
#'
columns_custom <- function(table){
  table %>% rename(type = "Column type",
                   phase = "Active phase",
                   length = "Column length (m)",
                   gas = "Carrier gas",
                   substrate = "Substrate",
                   diameter = "Column diameter (mm)",
                   thickness = "Phase thickness (m)",
                   program = "Program",
                   RI = "I",
                   reference = "Reference",
                   comment = "Comment") %>% 
    # fix column types and make uniform contents of some columns
    mutate_at(vars(length, diameter, thickness, RI), as.numeric)
}

#' custom tideir for RI tables with ramp temperature programs
#'
#' @param table 
#'
#' @return a table
#'
columns_ramp <- function(table){
  table %>% rename(type = "Column type",
                   phase = "Active phase",
                   length = "Column length (m)",
                   gas = "Carrier gas",
                   substrate = "Substrate",
                   diameter = "Column diameter (mm)",
                   thickness = "Phase thickness (m)",
                   temp_start = "Tstart (C)",
                   temp_end = "Tend (C)",
                   temp_rate = "Heat rate (K/min)",
                   hold_start = "Initial hold (min)",
                   hold_end = "Final hold (min)",
                   RI = "I",
                   reference = "Reference",
                   comment = "Comment") %>% 
    # fix column types and make uniform contents of some columns
    mutate_at(vars(length, diameter, thickness, temp_start, temp_end, temp_rate, hold_start, hold_end,  RI), as.numeric)
}

#' Custom tider for RI tables with isothermal temperature programs
#'
#' @param table 
#'
#' @return a table
#'
columns_iso <- function(table){
  table %>% rename(type = "Column type",
                   phase = "Active phase",
                   length = "Column length (m)",
                   gas = "Carrier gas",
                   substrate = "Substrate",
                   diameter = "Column diameter (mm)",
                   thickness = "Phase thickness (m)",
                   temp = "Temperature (C)",
                   RI = "I",
                   reference = "Reference",
                   comment = "Comment") %>% 
    # fix column types and make uniform contents of some columns
    mutate_at(vars(length, diameter, thickness, temp,  RI), as.numeric)
}


#' Tidier for webscraped RI tables
#'
#' @param tables captured by `get_RI_tables`
#'
#' @return a single table
#' 
#'
#' @examples
#' \dontrun{
#' tidy_RItable(test2)
#' }
tidy_RItable <- function(tables){
  
  temp_prog <- attr(tables, "temp_prog")
  
  tidy1 <- tables %>%
    #transpose tables and fix column names
    map({
      . %>% 
        html_table() %>% 
        t() %>%
        as.data.frame(stringsAsFactors = FALSE) %>% 
        setNames(.[1, ]) %>% 
        slice(-1)
    }) %>%
    # bind into one table
    bind_rows()
  if(temp_prog == "custom"){
    tidy2 <- columns_custom(tidy1)
  } else if(temp_prog == "ramp"){
    tidy2 <- columns_ramp(tidy1)
  } else if(temp_prog == "isothermal"){
    tidy2 <- columns_iso(tidy1)
  }
  
  # fix column names %>% 
  output <- tidy2 %>%
    mutate_all(~na_if(., "")) %>%
    mutate(gas = case_when(
      str_detect(gas, "He") ~ "Helium",
      str_detect(gas, "H2") ~ "Hydrogen",
      str_detect(gas, "N2") ~ "Nitrogen",
      TRUE                  ~ as.character(NA)
    )) %>%
    # reorder columns
    select(type, phase, RI, everything())
  return(output)
}