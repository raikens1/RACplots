#' Assignment-Control Plot
#'
#' Makes an AC plot of the data, with treated individuals in red and opaque,
#' control individuals translucent blue.  Assumes propensity and prognostic
#' scores are already in the data as \code{prop} and \code{prog}
#'
#' @param data a data.frame or tibble with t, prop, and prog
#' @param title a title for the plot
#' @param opaque_class 1, 0 or "none".  Specifies which class should be opaque
#'   and which should be translucent
#'
#' @return a ggplot object
#' @export
AC_plot <- function(data, title = "", opaque_class = 1){
  if(opaque_class == "none"){
    plt_data <- data %>%
      mutate(a = FALSE) %>%
      dplyr::select(c(t, prog, prop, a))
  } else{
    plt_data <- data %>%
      mutate(a = (t == opaque_class)) %>%
      dplyr::select(c(t, prog, prop, a))
  }

  plt <- ggplot(data = plt_data, aes( x = prop, y = prog, group = t, color = t)) +
    geom_point(size = 1, aes(alpha = a)) +
    scale_color_brewer(palette="Set1", direction = -1) +
    ggtitle(title) +
    theme(legend.position = "none", aspect.ratio=1,
          plot.title = element_text(hjust = 0.5, size = 9))+
    ylab(expression(paste("Prognosis, ", Psi, "(x)", sep = ""))) +
    xlab(expression(paste("Propensity, ", phi, "(x)", sep = "")))

  return(plt)
}


#' Randomization-Control plot
#'
#' Makes an RC plot of the data, with treated individuals in red and opaque,
#' control individuals translucent blue.  Assumes prognostic
#' scores and IV are already in the data as \code{prog} and \code{IV}
#'
#' @inheritParams AC_plot
#' @param data a data.frame or tibble with t, prog, and IV
#'
#' @return a ggplot object
#' @export
RC_plot <- function(data, title = "", opaque_class = 1){
  if(opaque_class == "none"){
    plt_data <- data %>%
      mutate(a = FALSE) %>%
      dplyr::select(c(t, prog, IV, a))
  } else{
    plt_data <- data %>%
      mutate(a = (t == opaque_class)) %>%
      dplyr::select(c(t, prog, IV, a))
  }
  plt <- ggplot(data = plt_data, aes( x = IV, y = prog, group = t, color = t)) +
    geom_point(size = 1, aes(alpha = a)) +
    scale_color_brewer(palette="Set1", direction = -1) +
    ggtitle(title) +
    theme(legend.position = "none", aspect.ratio=1, plot.title = element_text(hjust = 0.5, size = 9))+
    ylab(expression(paste("Prognosis, ", Psi, "(x)", sep = ""))) +
    xlab(expression(paste("IV", sep = "")))

  return(plt)
}


#' Randomization-Assignment plot
#'
#' Makes an RA plot of the data, with treated individuals in red and opaque,
#' control individuals translucent blue.  Assumes propensity scores and IV are
#' already in the data as \code{prop} and \code{IV}
#'
#' @inheritParams AC_plot
#' @param data a data.frame or tibble with t, prop, and IV
#'
#' @return a ggplot object
#' @export
RA_plot <- function(data, title = "", opaque_class = 1){
  if(opaque_class == "none"){
    plt_data <- data %>%
      mutate(a = FALSE) %>%
      dplyr::select(c(t, prop, IV, a))
  } else{
    plt_data <- data %>%
      mutate(a = (t == opaque_class)) %>%
      dplyr::select(c(t, prop, IV, a))
  }

  plt <- ggplot(data = plt_data, aes( x = IV, y = prop, group = t, color = t)) +
    geom_point(size = 1, aes(alpha = a)) +
    scale_color_brewer(palette="Set1", direction = -1) +
    ggtitle(title) +
    theme(legend.position = "none", aspect.ratio=1, plot.title = element_text(hjust = 0.5, size = 9))+
    ylab(expression(paste("Propensity, ", phi, "(x)", sep = ""))) +
    xlab(expression(paste("IV", sep = "")))

  return(plt)
}
