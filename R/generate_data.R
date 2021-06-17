#' @title Generate simulated data
#' @description Data set contains measured covariates X, outcome Y, treatment
#'   assignment t, and logit(propensity), mu.
#'
#'   covariate data ~ normal(0,1); mu = true_mu; t ~ binom(p = 1 / (1 +
#'   exp(-mu))); y ~ rho * X1 + sqrt(1-rho^2) * X2 + tau * t + epsilon epsilon ~
#'   normal(0, 1)
#'
#' @param N numeric, sample size
#' @param p numeric, number of features
#' @param true_mu string formula giving true propensity score linear model
#' @param rho numeric between 0 and 1.  0 => prog orthogonal to prop, 1=> prog
#'   || prop
#' @param sigma numeric noise to be added to y. y += sigma*rnorm(0,1)
#' @param tau numeric additive treatment effect
#' @return data.frame of covariates, y, t, and true prop (logit scale) and prog
#' @export
generate_data <- function(N = 2000,
                          p = 10,
                          true_mu = "X1/3-3",
                          rho = 0,
                          sigma = 1,
                          tau = 1) {

  df <- data.frame(matrix(rnorm(p*N), ncol = p)) %>%
    dplyr::mutate(prop = !!rlang::parse_quosure(true_mu),
                  prog = rho * X1 + sqrt(1 - rho ^ 2) * X2,
           t = as.logical(rbinom(n = N, size = 1, prob = 1 / (1 + exp(-prop)))),
           y = tau * t + prog + rnorm(N, sd = sigma))

  return(df)
}

#' @title Generate simulated data with a SITA violation
#' @description Data set contains measured covariates X, unmeasured covariate U,
#' outcome Y, treatment assignment t, and logit(propensity), mu.
#'
#' covariate data ~ normal(0,1);
#' mu = true_mu;
#' t ~ binom(p = 1 / (1 + exp(-mu)));
#' y ~ rho * X1 + sqrt(1-rho^2) * X2 + 0.2 * U + t + epsilon
#' epsilon ~ normal(0, 1)
#'
#' @inheritParams generate_data
#' @param nu coefficient of unmeasured confounder in propensity and prognosis
#' @return data.frame of covariates, y, t, and true prop (logit scale) and prog
#' @export
generate_xSITA_data <- function(N = 2000,
                                p = 10,
                                true_mu = "X1/3-3 + nu * U",
                                rho = 0.1,
                                nu = 0.2,
                                sigma = 1,
                                tau = 1) {
  # set up covariates
  df <- data.frame(matrix(rnorm(p * N), ncol = p)) %>%
    mutate(U = rnorm(N),
           prop = !!rlang::parse_quosure(true_mu),
           prog = rho * X1 + sqrt(1 - rho ^ 2)*X2 + nu * U,
           t = as.logical(rbinom(n = N, size = 1, prob = 1 / (1 + exp(-prop)))),
           y = tau * t + prog + rnorm(N, sd = sigma))

  return(df)
}


#' @title Generate simulated data for RD design
#' @description Data set contains measured covariates X, outcome Y, treatment
#'   assignment t, and logit(propensity), mu.
#'
#'   covariate data ~ normal(0,1);
#'   t = 0 if X1 < c, 1 otherwise;
#'   y ~ rho * X1 + sqrt(1-rho^2) * X2 + tau * t + epsilon;
#'   epsilon ~ normal(0, 1)
#'
#' @inheritParams generate_data
#' @param c numeric, cutoff for forcing variable
#' @return data.frame of covariates, y, t, and mu
#' @export
generate_rd_data <- function(N = 1000,
                          p = 10,
                          c = qnorm(0.75),
                          rho = 0,
                          sigma = 1,
                          tau = 1) {

  df <- data.frame(matrix(rnorm(p*N), ncol = p)) %>%
    dplyr::mutate(t = X1 > c,
                  y = tau * t + rho * X1 + sqrt(1 - rho ^ 2) * X2 + rnorm(N, sd = sigma),
                  prog = rho * X1 + sqrt(1 - rho ^ 2) * X2)

  return(df)
}

#' @title Generate Simulated IV data
#' @description Data set contains measured covariates X, outcome Y, treatment
#'   assignment t, and logit(propensity), mu.
#'
#'   covariate data ~ normal(0,1); mu = true_mu; t ~ binom(p = 1 / (1 +
#'   exp(-mu))); y ~ rho * X1 + sqrt(1-rho^2) * X2 + tau * t + epsilon epsilon ~
#'   normal(0, 1); Z instrument correllated with X1 by rho_z
#'
#' @param N numeric, sample size
#' @param p numeric, number of features
#' @param true_mu string formula giving true propensity score linear model
#' @param rho numeric between 0 and 1.  0 => prog orthogonal to prop, 1=> prog
#'   || prop
#' @param rho_z numeric between 0 and 1.  correllation between Z and X1
#' @param sigma numeric noise to be added to y. y += sigma*rnorm(0,1)
#' @param tau numeric additive treatment effect
#' @return data.frame of covariates, y, t, and true prop (logit scale) and prog
#' @export
generate_data_IV <- function(N = 2000,
                          p = 10,
                          true_mu = "X1/2 + Z/2 - 3.25",
                          rho = 0,
                          rho_z = 0.2,
                          sigma = 1,
                          tau = 1) {

  # a little hacky. To find assignment part of mu, extract and subtract the Z term
  # will fail if there are spaces in unexpected places, or if Z term is negative
  split_mu <- str_split(true_mu, " ")[[1]]
  Z_term <- split_mu[grepl("Z", split_mu)]

  df <- data.frame(matrix(rnorm(p*N), ncol = p)) %>%
    dplyr::mutate(zeta = rnorm(N),
                  Z = rho_z*X1 + sqrt(1 - rho ^ 2) * zeta,
                  mu = !!rlang::parse_quosure(true_mu),
                  prog = rho * X1 + sqrt(1 - rho ^ 2) * X2,
                  prop = mu - !!rlang::parse_quosure(Z_term),
                  t = as.logical(rbinom(n = N, size = 1, prob = 1 / (1 + exp(-mu)))),
                  y = tau * t + prog + rnorm(N, sd = sigma))

  return(df)
}


#' @title Generate Simulated IV data - with a SITA violation
#' @description Data set contains measured covariates X, outcome Y, treatment
#'   assignment t, and logit(propensity), mu.
#'
#'   covariate data ~ normal(0,1); mu = true_mu; t ~ binom(p = 1 / (1 +
#'   exp(-mu))); y ~ rho * X1 + sqrt(1-rho^2) * X2 + tau * t + epsilon epsilon ~
#'   normal(0, 1); Z instrument correllated with X1 by rho_z
#'
#' @param N numeric, sample size
#' @param p numeric, number of features
#' @param true_mu string formula giving true propensity score linear model
#' @param rho numeric between 0 and 1.  0 => prog orthogonal to prop, 1=> prog
#'   || prop
#' @param rho_z numeric between 0 and 1.  correllation between Z and X1
#' @param sigma numeric noise to be added to y. y += sigma*rnorm(0,1)
#' @param tau numeric additive treatment effect
#' @return data.frame of covariates, y, t, and true prop (logit scale) and prog
#' @export
generate_data_IV_xSITA <- function(N = 2000,
                             p = 10,
                             true_mu = "X1/2 + Z/2 - 3.25",
                             rho = 0,
                             rho_z = 0.2,
                             nu = 0.2,
                             sigma = 1,
                             tau = 1) {

  # a little hacky. To find assignment part of mu, extract and subtract the Z term
  # will fail if there are spaces in unexpected places, or if Z term is negative
  split_mu <- str_split(true_mu, " ")[[1]]
  Z_term <- split_mu[grepl("Z", split_mu)]

  df <- data.frame(matrix(rnorm(p*N), ncol = p)) %>%
    dplyr::mutate(U = rnorm(N),
                  zeta = rnorm(N),
                  Z = rho_z*X1 + sqrt(1 - rho ^ 2) * zeta,
                  mu = !!rlang::parse_quosure(true_mu),
                  prog = rho * X1 + sqrt(1 - rho ^ 2) * X2 + nu * U,
                  prop = mu - !!rlang::parse_quosure(Z_term),
                  t = as.logical(rbinom(n = N, size = 1, prob = 1 / (1 + exp(-mu)))),
                  y = tau * t + prog + rnorm(N, sd = sigma))

  return(df)
}
