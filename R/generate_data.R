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
#' @return data.frame of covariates, y, t, and mu
#' @export
generate_data <- function(N = 2000,
                          p = 10,
                          true_mu = "X1/3-3",
                          rho = 0,
                          sigma = 1,
                          tau = 1) {

  df <- data.frame(matrix(rnorm(p*N), ncol = p)) %>%
    dplyr::mutate(mu = !!rlang::parse_quosure(true_mu),
           t = rbinom(n = N, size = 1, prob = 1 / (1 + exp(-mu))),
           y = tau * t + rho * X1 + sqrt(1 - rho ^ 2) * X2 + rnorm(N, sd = sigma))

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
#' @param N numeric, sample size
#' @param p numeric, number of features
#' @param true_mu string formula giving true propensity score linear model
#' @param rho numeric between 0 and 1.  0 => prog orthogonal to prop, 1=> prog || prop
#' @param nu coefficient of unmeasured confounder in propensity and prognosis
#' @param sigma numeric noise to be added to y. y += sigma*rnorm(0,1)
#' @param tau numeric additive treatment effect
#' @return data.frame of covariates, y, t, and mu
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
           mu = !!rlang::parse_quosure(true_mu),
           t = rbinom(n = N, size = 1, prob = 1 / (1 + exp(-mu))),
           y = tau * t + rho * X1 + sqrt(1 - rho ^ 2)*X2 + nu * U + rnorm(N, sd = sigma))

  return(df)
}
