#' Propensity Score Histogram
#'
#' @param data data a data.frame or tibble with t and prop
#'
#' @return
#' @export
propensity_histogram <- function(data){
  ggplot(data, aes(x = prop, y=..density.., fill = t)) +
    geom_histogram(alpha = 0.4, position = "identity") +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    theme(legend.position = "none") +
    xlab(expression(paste("Propensity, ", phi, "(x)", sep = ""))) +
    ylab("Density")
}
