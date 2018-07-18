#' Extract VIP scores from pls object
#' 
#' Provides a wrapper for \code{\link{getVipVn}} from the \link{ropls} package that returns a tibble rather than a named numeric vector.
#'
#' @param pls a pls object created by \code{\link{opls}}
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

#' Extract axis loadings from pls object
#'
#' Provides a wrapper for \code{\link{getLoadingMN}} from the \link{ropls} package that returns a tibble rather than a matrix
#'
#' @param pls a pls object created by \code{\link{opls}}
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
#' get_loadings(pls.model)
#' }
get_loadings <- function(pls){
  getLoadingMN(pls) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    rename(Variable = "rowname")
}



#' Format numbers correctly as CAS numbers
#'
#'
#' @param x an integer or character vector
#' @importFrom webchem is.cas
#' @import stringr
#' @import purrr
#' @return a character vector of correctly formatted CAS numbers
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

#' Extract data for plotting (O)PLS(-DA) data with ggplot2
#' 
#' Extracts relevant data from an "opls" object for making annotated score plots with ggplot2 or other plotting packages.
#'
#' @param model An object created by \code{\link{opls}}
#' @import dplyr
#' @import ropls
#'
#' @return For **PCA**, a list containing: principal component scores (`plot_data`, a data frame) and variance explained (`var_explained`), a named vector) for PC axes
#' 
#' For **PLS** and **PLS-DA**, a list containing: A dataframe of scores along predictive axes and y-variable values (`plot_data`), descriptive statistics for each axis (`axis_stats`), and descriptive statistics for the model including R^2, Q^2, and p-value (`model_stats`)
#' 
#' For **OPLS** and **OPLS-DA**, a list containing: A dataframe of scores along one predictive axis, orthogonal axes, and y-variable values (`plot_data`), descriptive statistics for each axis (`axis_stats`), and descriptive statistics for the model including R^2, Q^2, and p-value (`model_stats`)
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' library(ropls)
#' data(sacurine)
#' sacurine.oplsda <- opls(sacurine$dataMatrix, sacurine$sampleMetadata[, "gender"],
#'                         predI = 1,
#'                         orthoI = NA)
#' df <- get_plotdata(sacurine.oplsda)
#' }

get_plotdata <- function(model){
  #check object type
  if(class(model) != "opls"){
    stop(paste("Expected a model object created by ropls::opls(), but was passed an object of class", class(model)[1]))
  }
  if(model@typeC == "PCA"){
    scores <- model@scoreMN %>% as.data.frame() %>% rownames_to_column(var = "sample")
    plot_data <- scores
    
    var_explained <- model@modelDF[ , 1:2]
    
    return(list("plot_data" = plot_data,
                "var_explained" = var_explained))
    
  } else if(model@typeC == "PLS-DA"){
    y <- model@suppLs$yMCN %>% as.data.frame() %>% rownames_to_column(var = "sample")
    scores <- model@scoreMN %>% as.data.frame() %>% rownames_to_column(var = "sample")
    plot_data <- full_join(y, scores)
    
    return(list("plot_data" = plot_data,
                "axis_stats" = model@modelDF,
                "model_stats" = model@summaryDF)) 
    
  } else if(model@typeC == "OPLS-DA"){
    #make a OPLS-DA data frame
    pred.scores <- model@scoreMN %>% as.data.frame() %>% rownames_to_column(var = "sample")
    ortho.scores <- model@orthoScoreMN %>% as.data.frame() %>% rownames_to_column(var = "sample")
    scores <- full_join(pred.scores, ortho.scores)
    y <- model@suppLs$yMCN %>% as.data.frame() %>% rownames_to_column(var = "sample")
    plot_data <- full_join(y, scores)
    
    return(list("plot_data" = plot_data,
                "axis_stats" = model@modelDF,
                "model_stats" = model@summaryDF))
    
  } else if(model@typeC == "PLS"){
    #make a PLS data frame
    scores <- model@scoreMN %>% as.data.frame() %>% rownames_to_column(var = "sample")
    y <- model@suppLs$yMCN %>% as.data.frame() %>% rownames_to_column(var = "sample")
    plot_data <- full_join(y ,scores)
    
    return(list("plot_data" = plot_data,
                "axis_stats" = model@modelDF,
                "model_stats" = model@summaryDF)) 
    
  } else if(model@typeC == "OPLS"){
    #make an OPLS data frame
    pred.scores <- model@scoreMN %>% as.data.frame() %>% rownames_to_column(var = "sample")
    ortho.scores <- model@orthoScoreMN %>% as.data.frame() %>% rownames_to_column(var = "sample")
    scores <- full_join(pred.scores, ortho.scores)
    y <- model@suppLs$yMCN %>% as.data.frame() %>% rownames_to_column(var = "sample")
    plot_data <- full_join(y, scores)
    
    return(list("plot_data" = plot_data,
                "axis_stats" = model@modelDF,
                "model_stats" = model@summaryDF))
  }
}