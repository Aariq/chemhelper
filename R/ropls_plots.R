#' Plot PCA models created by `ropls::opls()`
#'
#' @param ropls_pca 
#' @param group_var 
#'
#' @return a ggplot object
#' 
#' @import latex2exp
#' @import ggplot2
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' plot_pca(pca, data$treatment)
#' }
plot_pca <- function(ropls_pca, group_var){
  plotdata <- chemhelper::get_plotdata(ropls_pca)
  ggplot(plotdata$plot_data, aes(x = p1, y = p2, color = group_var)) +
    geom_point() +
    stat_ellipse() +
    labs(x = paste0("PC1 (", plotdata$var_explained$R2X[1] * 100, "%)"),
         y = paste0("PC2 (", plotdata$var_explained$R2X[2] * 100, "%)")) +
    scale_colour_discrete("Group Membership") +
    theme_bw() +
    ggtitle("PCA") +
    labs(caption = latex2exp::TeX(
      paste0(nrow(plotdata$var_explained), " principal components;",
             "$R^2(cumulative) = ", max(plotdata$var_explained$`R2X(cum)`, "$"))))
}



#' Plot PLS-DA models produced by `ropls::opls()`
#'
#' @param ropls_plsda 
#'
#' @return a ggplot object
#' 
#' @import latex2exp
#' @import ggplot2
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' plot_plsda(plsda)
#' }
plot_plsda <- function(ropls_plsda){
  plotdata <- chemhelper::get_plotdata(ropls_plsda)
  ggplot(plotdata$plot_data, aes(x = p1, y = p2, color = y1)) +
    geom_point() +
    stat_ellipse() +
    labs(x = paste0("P1 (", plotdata$axis_stats$R2X[1] * 100, "%)"),
         y = paste0("P2 (", plotdata$axis_stats$R2X[2] * 100, "%)")) +
    scale_color_discrete("Group Membership") +
    theme_bw() +
    ggtitle("PLS-DA") +
    labs(caption = TeX(
      paste0("$R^{2}_{Y} = ", plotdata$model_stats$`R2Y(cum)`, "$; ",
             "$Q^{2} = ", plotdata$model_stats$`Q2(cum)`, "$; ",
             "$p_{Q^{2}} = ", plotdata$model_stats$pQ2, "$")))
}