#' Assignment-Control Filter Plot
#'
#' Shows an AC plot of a matched dataset, with matched individuals opaque and
#' unmatched individuals translucent
#'
#' @inheritParams AC_match_plot
#'
#' @return
#' @export
AC_filter_plot <- function(data, match, title = "", is_RAC = FALSE){
  n_pairs <- sum(!is.na(match))/2

  plt_data <- data %>%
    mutate(m = match) %>%
    mutate(a = !is.na(m)) %>%
    dplyr::select(c(t, prog, prop, m, a))

  plt <- ggplot(data = plt_data, aes( x = prop, y = prog, group = t, color = t)) +
    geom_point(aes(alpha = a), size = 1) +
    scale_color_brewer(palette="Set1", direction = -1) +
    ggtitle(title)+
    theme(legend.position = "none", aspect.ratio = 1,
          plot.title = element_text(hjust = 0.5, size = 9))+
    ylab(expression(paste("Prognosis, ", Psi, "(x)", sep = ""))) +
    xlab(expression(paste("Propensity, ", phi, "(x)", sep = "")))

  if(is_RAC){
    plt <- plt +
      xlab(expression(paste("Propensity, ", tilde(phi), "(x)", sep = "")))
  }

  return(plt)
}

#' Control-Randomization Filter Plot
#'
#' Shows a CR plot of a matched dataset, with matched individuals opaque and
#' unmatched individuals translucent
#'
#' @inheritParams AC_filter_plot
#'
#' @return
#' @export
CR_filter_plot <- function(data, match, title = ""){
  n_pairs <- sum(!is.na(match))/2

  plt_data <- data %>%
    mutate(m = match) %>%
    mutate(a = !is.na(m)) %>%
    dplyr::select(c(t, prog, IV, m, a))

  plt <- ggplot(data = plt_data, aes(x = prog, y = IV, group = t, color = t)) +
    geom_point(aes(alpha = as.factor(a)), size = 1)+
    scale_color_brewer(palette="Set1", direction = -1) +
    ggtitle(title)+
    theme(legend.position = "none", aspect.ratio=1, plot.title = element_text(hjust = 0.5, size = 9))+
    ylab(expression(paste("Prognosis, ", Psi, "(x)", sep = ""))) +
    xlab(expression(paste("IV", sep = "")))

  return(plt)
}

#' Assignment-Randomization Filter Plot
#'
#' Shows an AR plot of a matched dataset, with matched individuals opaque and
#' unmatched individuals translucent
#'
#' @inheritParams AC_filter_plot
#'
#' @return
#' @export
AR_filter_plot <- function(data, match, k = 1, title = ""){
  n_pairs <- sum(!is.na(match))/2

  plt_data <- data %>%
    mutate(m = match) %>%
    mutate(a = !is.na(m)) %>%
    dplyr::select(c(t, prop, IV, m, a))

  plt <- ggplot(data = plt_data, aes(x = prop, y = IV, group = t, color = t)) +
    geom_point(aes(alpha = a), size = 1)+
    scale_color_brewer(palette="Set1", direction = -1) +
    ggtitle(title)+
    theme(legend.position = "none", aspect.ratio=1, plot.title = element_text(hjust = 0.5, size = 9))+
    xlab(expression(paste("Propensity, ", phi, "(x)", sep = "")))
    ylab(expression(paste("IV", sep = "")))

  return(plt)
}
