#' Assignment-Control Filter Plot
#'
#' Shows an AC plot of a matched dataset, with matched individuals opaque and
#' unmatched individuals translucent
#'
#' @inheritParams AC_match_plot
#'
#' @return
#' @export
AC_filter_plot <- function(data, match, title = ""){
  n_pairs <- sum(!is.na(match))/2

  plt_data <- data %>%
    mutate(m = match) %>%
    mutate(a = ifelse (is.na(m), 0.9, 1),
           t = as.factor(abs(1-t))) %>%
    dplyr::select(c(t, prog, prop, m, a))

  plt <- ggplot(data = plt_data, aes( x = prop, y = prog, group = t, color = t)) +
    geom_point(aes(alpha = a), size = 1) +
    scale_color_brewer(palette="Set1") +
    ggtitle( title)+
    theme(legend.position = "none", aspect.ratio=1, plot.title = element_text(hjust = 0.5, size = 9))+
    ylab(expression(paste("Prognosis, ", Psi, "(x)", sep = ""))) +
    xlab(expression(paste("Propensity, ", phi, "(x)", sep = "")))

  return(plt)
}

#' Randomization-Control Filter Plot
#'
#' Shows an RC plot of a matched dataset, with matched individuals opaque and
#' unmatched individuals translucent
#'
#' @inheritParams RC_match_plot
#'
#' @return
#' @export
RC_filter_plot <- function(data, match, k = 1, title = ""){
  n_pairs <- sum(!is.na(match))/(k + 1)

  plt_data <- data %>%
    mutate(m = match) %>%
    mutate(a = ifelse (is.na(m), 0.9, 1),
           t = as.factor(abs(1-t))) %>%
    dplyr::select(c(t, prog, IV, m, a))

  plt <- ggplot(data = plt_data, aes( x = IV, y = prog, group = t, color = t)) +
    geom_point(aes(alpha = as.factor(a)), size = 1)+
    scale_color_brewer(palette="Set1") +
    ggtitle( title)+
    theme(legend.position = "none", aspect.ratio=1, plot.title = element_text(hjust = 0.5, size = 9))+
    ylab(expression(paste("Prognosis, ", Psi, "(x)", sep = ""))) +
    xlab(expression(paste("IV", sep = "")))

  return(plt)
}

#' Randomization-Assignment Filter Plot
#'
#' Shows an RA plot of a matched dataset, with matched individuals opaque and
#' unmatched individuals translucent
#'
#' @inheritParams RA_match_plot
#'
#' @return
#' @export
RA_filter_plot <- function(data, match, k = 1, title = ""){
  n_pairs <- sum(!is.na(match))/(k + 1)

  plt_data <- data %>%
    mutate(m = match) %>%
    mutate(a = ifelse (is.na(m), 0.9, 1),
           t = as.factor(abs(1-t))) %>%
    dplyr::select(c(t, prop, IV, m, a))

  plt <- ggplot(data = plt_data, aes( x = IV, y = prop, group = t, color = t)) +
    geom_point(aes(alpha = a), size = 1)+
    scale_color_brewer(palette="Set1") +
    ggtitle( title)+
    theme(legend.position = "none", aspect.ratio=1, plot.title = element_text(hjust = 0.5, size = 9))+
    ylab(expression(paste("Propensity, ", phi, "(x)", sep = "")))
  xlab(expression(paste("IV", sep = "")))

  return(plt)
}
