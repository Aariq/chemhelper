
# To-do
# - Use quasiquotation to simplify code
# - Make into a geom_ or stat_ function for ggplot2

#' Calculate convex hulls by group.
#' taken from https://gist.github.com/mbedward/26296da610fd747a9b9f62672f57e9c1.js 
#' Author: Michael Bedward
#' @param dat a data frame 
#' @param xcol column of x data
#' @param ycol column of y data
#' @param groupcol grouping columns
#' @param hullcol I'm not sure yet??
#' @author Michael Bedward
#'
#' @return Returns a new data frame with rows reduced to those for convex hull vertices and a new hull column giving the order in which to plot (e.g. with geom_polygon).
#' @import dplyr
#' @export 
#'
#' @examples
#' \dontrun{
#' hulls <- hullfn(data, x, y, groupcol = "grp")
#' }
#
# Convex hull function. Takes a data frame with:
#
#   - x,y columns
#   - an optional group column 
#   - any additional columns
#
# Returns a new data frame with rows reduced to those
# for convex hull vertices and a new hull column giving
# the order in which to plot (e.g. with geom_polygon).
#
# taken from https://gist.github.com/mbedward/26296da610fd747a9b9f62672f57e9c1.js
# Author: Michael Bedward

hullfn <- function(dat, 
                   xcol,
                   ycol, 
                   groupcol = NULL,
                   hullcol = "hull") {
  
  do_hull <- function(xs, ys) {
    ii <- 1:length(xs)
    hi <- chull(xs, ys)
    match(ii, hi)
  }
  
  #could use quasiquotation instead of doing this next chunk I think
  cols <- colnames(dat)
  ix <- match(xcol, cols)
  iy <- match(ycol, cols)
  colnames(dat)[c(ix, iy)] <- c("x", "y")
  
  grps <- !is.null(groupcol)
  if (grps) {
    dat <- group_by_(dat, .dots = groupcol)
  }
  
  dat <- dat %>%
    mutate(hull = do_hull(x, y)) %>%
    arrange(hull) %>%
    filter(!is.na(hull))
  
  if (grps) dat <- ungroup(dat)
  
  # restore original column names
  colnames(dat) <- c(cols, hullcol)
  
  dat
}
