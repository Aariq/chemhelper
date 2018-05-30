#' Scaling Functions for Metabolomics
#'
#' Provides additional scaling functions besides autoscaling.  Reviewed in van den [Berg et al. 2006](https://doi.org/10.1186/1471-2164-7-142).
#'
#' @param x a vector
#' @param center logical. Do you want to apply centering?
#' @param scale choice of scaling functions.  Defaults to autoscaling (dividing by standard deviation). See details for more.
#' 
#' @details Currently the choices for `scale = ` allow for all of the scaling methods reviewed in 
#' Berg et al. 2006. *Centering, scaling, and transformations: improving the biological information content of metabolomics data.*
#'  BMC Genomics 7:142. Autoscaling divides each number by the column standard deviation. 
#'  Pareto scaling divides each number by the square root of the column standard deviation.  
#'  Compared to autoscaling, this stays closer to the original measurments, but is highly sensitive to large fold changes.
#'  Range scaling divides the numbers by the column range, which may be useful in cases when scaling relative to a biologically possible 
#'  range is desired, however this method is highly sensitive to outliers. Vast scaling multiplies the autoscaled results 
#'  by the ratio of the column mean or some group mean to the column/group standard deviation. With this method, one could take knowledge 
#'  of groups into account, although this isn't currently implemented in this function.  Level scaling simply divides by the column mean, 
#'  transforming values into relative responses.
#' 
#' @return Scaled vector with attributes showing the scaling and centering parameters
#' @importFrom stats sd
#' @export
#'
#' @examples
#' x = c(0, 0.1, 0.2, 10)
#' y = c(1000, 1232, 2022, 4000)
#'
#' chem_scale(x, center = TRUE, scale = "auto")
#' chem_scale(y, center = TRUE, scale = "pareto")
#'
chem_scale <-
  function(x, center = TRUE, scale = c("auto", "pareto", "range", "vast", "level", "none")) {
    if(center){
      c = x - mean(x)
    }else{
      c = x
    }
    if(missing(scale)){
        method = "auto"
    }else{
      method = scale
    }
    out <- switch(method,
                  none = c,
                  auto = c / sd(x),
                  pareto = c / sqrt(sd(x)),
                  range = c / (max(x) - min(x)),
                  vast = c / sd(x) * (mean(x) / sd(x)),
                  level = c / center
    )
    attributes(out) <- switch(method,
                              none = list(center = mean(x)),
                              auto = list(center = mean(x), scale = sd(x)),
                              pareto = list(center = mean(x), scale = sqrt(sd(x))),
                              range = list(center = mean(x), scale = max(x) - min(x)),
                              vast = list(center = mean(x), scale = sd(x) * (mean(x) / sd(x))),
                              level = list(center = mean(x), scale = center)
    )
    return(out)
  }
