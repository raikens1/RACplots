#' Assignment-Control plot with matching overlayed
#'
#' @inheritParams AC_plot
#' @param match an optmatch object with a 1:1 matching of subjects
#'
#' @return
#' @export
AC_match_plot <- function(data, match, title = ""){
  n_pairs <- sum(!is.na(match))/2

  plt_data <- data %>%
    mutate(m = match) %>%
    mutate(a = !is.na(m)) %>%
    dplyr::select(c(t, prog, prop, m, a))

  m_data <- plt_data %>%
    filter(!is.na(m)) %>%
    arrange(m, desc(t)) %>%
    mutate(id = rep(1:2, n_pairs)) %>%
    dplyr::select(-c(t, a)) %>%
    group_by(m) %>%
    summarize(prop1 = first(prop), prop2 = last(prop),
              prog1 = first(prog), prog2 = last(prog)) %>%
    dplyr::select(prog1, prog2, prop1, prop2)

  plt <- ggplot(data = plt_data, aes(x = prop, y = prog, group = t, color = t)) +
    geom_point(aes(alpha = a), size = 1) +
    scale_color_brewer(palette="Set1", direction = -1) +
    geom_segment(data = m_data,
                 aes(x = prop1, y = prog1,
                     xend = prop2, yend = prog2),
                 color =  "black", group = NA, linetype = "dashed") +
    ggtitle( title)+
    theme(legend.position = "none", aspect.ratio=1,
          plot.title = element_text(hjust = 0.5, size = 9))+
    ylab(expression(paste("Prognosis, ", Psi, "(x)", sep = ""))) +
    xlab(expression(paste("Propensity, ", phi, "(x)", sep = "")))

  return(plt)
}

#' Control-Randomization plot with matching overlayed
#'
#' @inheritParams AC_match_plot
#' @param data a data.frame or tibble with t, prog, and IV
#'
#' @return
#' @export
CR_match_plot <- function(data, match, title = ""){
  n_pairs <- sum(!is.na(match))/2

  plt_data <- data %>%
    mutate(m = match) %>%
    mutate(a = !is.na(m)) %>%
    dplyr::select(c(t, prog, IV, m, a))

  m_data <- plt_data %>%
    filter(!is.na(m)) %>%
    arrange(m, desc(t)) %>%
    mutate(id = rep(1:2, n_pairs)) %>%
    dplyr::select(-c(t, a)) %>%
    group_by(m) %>%
    summarize(IV1 = first(IV), IV2 = last(IV),
              prog1 = first(prog), prog2 = last(prog)) %>%
    dplyr::select(prog1, prog2, IV1, IV2)

  plt <- ggplot(data = plt_data, aes(x = prog, y = IV, group = t, color = t)) +
    geom_point(aes(alpha = a), size = 1)+
    scale_color_brewer(palette="Set1", direction = -1) +
    geom_segment(data = m_data,
                 aes(y = IV1, x = prog1,
                     yend = IV2, xend = prog2),
                 color =  "black", group = NA, linetype = "dashed") +
    ggtitle( title)+
    theme(legend.position = "none", aspect.ratio=1, plot.title = element_text(hjust = 0.5, size = 9))+
    xlab(expression(paste("Prognosis, ", Psi, "(x)", sep = ""))) +
    ylab(expression(paste("IV", sep = "")))

  return(plt)
}

#' Assignment-Randomization plot with matching overlayed
#'
#' @inheritParams AC_match_plot
#' @param data a data.frame or tibble with t, prop, and IV
#'
#' @return
#' @export
AR_match_plot <- function(data, match, title = ""){
  n_pairs <- sum(!is.na(match))/2

  plt_data <- data %>%
    mutate(m = match) %>%
    mutate(a = !is.na(m)) %>%
    dplyr::select(c(t, prop, IV, m, a))

  m_data <- plt_data %>%
    filter(!is.na(m)) %>%
    arrange(m, desc(t)) %>%
    mutate(id = rep(1:2, n_pairs)) %>%
    dplyr::select(-c(t, a)) %>%
    group_by(m) %>%
    summarize(IV1 = first(IV), IV2 = last(IV),
              prop1 = first(prop), prop2 = last(prop)) %>%
    dplyr::select(prop1, prop2, IV1, IV2)

  plt <- ggplot(data = plt_data, aes( x = prop, y = IV, group = t, color = t)) +
    geom_point(aes(alpha = a), size = 1)+
    scale_color_brewer(palette="Set1", direction = -1) +
    geom_segment(data = m_data,
                 aes(y = IV1, x = prop1,
                     yend = IV2, xend = prop2),
                 color =  "black", group = NA, linetype = "dashed") +
    ggtitle(title)+
    theme(legend.position = "none", aspect.ratio=1, plot.title = element_text(hjust = 0.5, size = 9))+
    xlab(expression(paste("Propensity, ", phi, "(x)", sep = "")))
    ylab(expression(paste("IV", sep = "")))

  return(plt)
}
