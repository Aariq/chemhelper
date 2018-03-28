#' Scaling Functions for Metabolomics
#'
#' Provides additional scaling functions besides autoscaling.  Reviewed in van den Berg et al. 2006.
#'
#' @param x a vector
#' @param center logical. Do you want to apply centering?
#' @param scale choice of scaling functions.  Defaults to autoscaling (dividing by standard deviation). See details for more.
#'
#' @return scaled vector with attributes showing the scaling and centering parameters
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
