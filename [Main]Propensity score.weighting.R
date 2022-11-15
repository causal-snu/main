
## propensityml, paper : Improving propensity score weighting using machine learning.

## (1) Installation
library(stats)
library(magrittr)
library(data.table)
library(Matrix)
library(tibble)
library(dplyr)
library(randomForest)
library(rpart)
library(e1071)
library(stringr)
library(mvtnorm)
library(rlang)
library(ggplot2)
library(foreach)
library(parallel)
library(remotes)
library(covr)
library(testthat)
library(readr)
library(usethis)
library(propensityml)
remotes::install_github("ygeunkim/propensityml", force=TRUE)


## (2) read_me.... 참고용
(x <- sim_outcome(1000, covmat = build_covariate()))

(fit_rf <- 
    x %>% 
    ps_rf(exposure ~ . - y - exposure_prob, data = .))

##  #defined the class named propmod for some usage
class(fit_rf)

## Estimating propensity score:
estimate_ps(fit_rf) %>% head()



## (3) data_set 
devtools::install_github("r-lib/usethis")
chemical <-
  readr::read_table(
    "C:/Users/sarah/Desktop/대학원 2학기/응용통계세미나/프로젝트/poisox.txt",
    col_names = c("age", "sex", "prior", "poisox", "after", "mortal")
  ) %>%
  dplyr::mutate(
    sex = factor(sex),
    poisox = factor(poisox),
    blood = after - prior,
    mortal = factor(mortal)
  ) %>%
  dplyr::select(-after, -prior)

chemical
## ------------------------------------------------------------------##
# R코드 - github

##1. Evaluate.R

##1-1. Covariate Balance : gives covariate balance summary
##1-1-1. compute_balance
compute_balance <- function(data, col_name = "balance", treatment, trt_indicator = 1, outcome, exclude = NULL) {
  if (is.data.table(data)) {
    data <- copy(data)
  } else {
    data <- copy(data %>% data.table())
  }
  old <- names(data)
  setnames(data, old, str_remove_all(old, pattern = "\\."))
  data %>%
    tidy_moment(treatment = treatment, col_exclude = c(outcome, exclude)) %>%
    .[,
      .(balance = diff(value[moment == "mean"]) / ( sqrt(sum(value[moment == "var"]) / 2) ) ),
      by = variable] %>%
    # .[,
    #   .(balance = diff(value[moment == "mean"]) / sqrt(value[moment == "var" & get(treatment) == trt_indicator])),
    #   by = variable] %>%
    setNames(c("variable", col_name))
}

##1-1-2. compute_moment
compute_moment <- function(x) {
  if (is.factor(x)) x <- as.numeric(levels(x))[x]
  list(mean = mean(x), var = var(x))
}

##1-1-3. tidy_moment
tidy_moment <- function(data, treatment, with_melt = NULL, col_exclude) {
  data[,
       unlist(lapply(.SD, compute_moment)) %>% as.list(),
       by = treatment,
       .SDcols = -col_exclude] %>%
    melt(id.vars = c(treatment, with_melt)) %>%
    .[,
      c("variable", "moment") := tstrsplit(variable, ".", fixed = TRUE)]
}

##1-2. Average standardized absolute mean distance(ASAM) : computes ASAM
## : Lower ASAM means that treatment and control groups are more similar w.r.t. the given covariates.

##1-2-1.COMPUTE ASAM
compute_asam <- function(data, treatment, trt_indicator = 1, outcome, exclude = NULL,
                         object = NULL, formula = NULL, method = c("logit", "rf", "cart"), 
                         weighting = c("IPW", "SIPW"), mc_col = NULL,sc_col = NULL, parallel = FALSE, ...){
  if (is.data.table(data)){
    data <- copy(data)
  } else {
    data <- copy(data %>% data.table())
  }
  # IPW or SIPW------------------------
  weighting <- match.arg(weighting)
  if (weighting == "IPW") { # ipw
    wt <-
      data %>%
      compute_ipw(
        treatment = treatment, trt_indicator = trt_indicator, outcome = outcome,
        object = object, formula = formula, method = method, mc_col = mc_col, sc_col = sc_col, parallel = parallel, ...
      )
  } else { # sipw
    wt <-
      data %>%
      compute_sipw(
        treatment = treatment, trt_indicator = trt_indicator, outcome = outcome,
        object = object, formula = formula, method = method, mc_col = mc_col, sc_col = sc_col, parallel = parallel, ...
      )
  }
  # data[,
  #      (weighting) := wt]
  if (is.null(c(mc_col, sc_col))) {
    data[,
         (weighting) := wt]
  } else {
    data <- merge(data, wt, by = c(mc_col, sc_col))
  }
  # Balancing--------------------------------------------------------------
  wt_vars <- paste0(weighting, c(".mean", ".var"))
  left_formula <- paste0(c(treatment, "variable", wt_vars), collapse = "+")
  formul <- paste0(c(left_formula, "moment"), collapse = "~")
  formul <- as.formula(formul)
  # covariate column names--------------------------
  covariate_name <- names(data)
  covariate_name <- setdiff(covariate_name, c(outcome, treatment, exclude, weighting, mc_col, sc_col))
  # covariate columns that are factor---------------
  cols_fct <- sapply(data, class)[covariate_name]
  cols_fct <- names(cols_fct[cols_fct == "factor"])
  # change to numeric-------------------------------
  for (col in cols_fct) set(data, j = col, value = as.numeric(levels(data[[col]]))[data[[col]]])
  # weighing----------------------------------------
  for (col in covariate_name) set(data, j = col, value = data[[col]] * data[[weighting]])
  # balance dt--------------------------------------
  data %>%
    .[,
      .(
        asam = mean(
          compute_balance(
            .SD,
            treatment = treatment,
            trt_indicator = trt_indicator,
            outcome = outcome,
            exclude = c(exclude, weighting)
          )$balance %>% abs()
        )
      ),
      by = sc_col]
  # data %>%
  #   compute_balance(
  #     treatment = treatment,
  #     trt_indicator = trt_indicator,
  #     outcome = outcome,
  #     exclude = c(exclude, weighting)
  #   ) %>%
  #   .[,
  #     .(asam = mean(abs(balance))),
  #     by = sc_col]
}

##2. propensity.R : propensity socre fitting (function)

  ### (1)-1 Fitting Logistic Regression for Propensity Score
ps_glm <- function(formula, data, ...) {
  result <-
    data %>%
    glm(formula, data = ., family = binomial, ...)
  res <- structure(list(
    model = result,
    name = "glm",
    data = data
  ))
  class(res) <- "propmod"
  # result
  # return(invisible(res))
  res
}
  ### (1)-2 Fitting Random Forests for Propensity Score
ps_rf <- function(formula, data, ...) {
  result <-
    data %>%
    randomForest(formula, data = ., ...)
  res <- structure(list(
    model = result,
    name = "rf",
    data = data
  ))
  class(res) <- "propmod"
  # result
  # return(invisible(res))
  res
}
  ### (1)-3 Fitting CART for Propensity Score
ps_cart <- function(formula, data, ...) {
  result <-
    data %>%
    rpart(formula, data = ., method = "class", ...)
  res <- structure(list(
    model = result,
    name = "cart",
    data = data
  ))
  class(res) <- "propmod"
  # result
  # return(invisible(res))
  res
}
  ### (1)-4 Estimation of Propensity Score
estimate_ps <- function(object, ...) {
  if (object$name == "glm") {
    return(predict(object$model, type = "response"))
  } else if (object$name == "rf") {
    trt_lev <-
      ifelse(
        which(c("1", "TRUE") %in% object$model$classes) == 1,
        1,
        TRUE
      )
    pred <- predict(object$model, type = "prob")
    return(pred[, colnames(pred) == trt_lev])
  } else if (object$name == "cart") {
    pred <- predict(object$model, type = "prob")
    trt_lev <-
      ifelse(
        which(c("1", "TRUE") %in% colnames(pred)) == 1,
        1,
        TRUE
      )
    return(pred[, colnames(pred) == trt_lev])
  }
}
  ### (1)-5 Propensity score model class : 'propmod' class 
      ### The \code{propmod} class ready for computing propensity score
setOldClass("propmod")

#' @rdname propmod-class
#' @param x `propmod` object
#' @param digits digit
#' @param ... additional arguments
#' @export

print.propmod <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  print(x$model)
  invisible(x$name)
  invisible(x$data)
}

##3. propensityml-package.R : package description

#' @aliases propensityml propensityml-package
#' @details
#' The propensityml package provides various ML functions to estimate propensity score.
#' @references Lee, B. K., Lessler, J., & Stuart, E. A. (2010). \emph{Improving propensity score weighting using machine learning. Statistics in Medicine}. Statistics in Medicine, 29(3), 337-346.
#' @keywords internal
'_PACKAGE'

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

##3. simdata.R : Potential outcome framework , Toy dataset
  ##3-1. 
#' @format A data frame with 8 rows and 5 variables:
#' \describe{
#'   \item{y0}{outcome when the unit is untreated}
#'   \item{y1}{outcome when the unit is treated}
#'   \item{z}{binary treatment}
#'   \item{y}{outcome}
#'   \item{n}{observed count}
#' }
outcome_frame
  
  ##3-2.
  ## Cohort Study Data about POISOX : 
    ### Cohort study interesting in the effect of exposure to an industrial chemical, POISOX, on subsequent blood pressure and mortality.
#' @docType data
#'
#' @format A data frame with 5000 rows and 5 variables:
#' \describe{
#'   \item{age}{age of each subject}
#'   \item{sex}{sex of each subject, 0 if male, 1 if female}
#'   \item{poisox}{0 if unexposed, 1 if exposed}
#'   \item{mortal}{0 if alive, 1 if dead}
#'   \item{blood}{subsequent blood pressure, after - prior}
#' }
chemical

  ##3-3.
  ## Setoguchi paper(논문)
    ##3-3-1. build_covariate

#' Build covariance matrix of the covariates
#'
#' @description
#' builds covariance matrix of the onfounders, exposure predictors, and outcome predictors
#' @param sig variance of each covariate
#' @param confounder_exposure covariance between confounder and exposure predictors, vector in order of column
#' @param confounder_outcome covariance between confounder and outcome predictors, vector in order of column
#' @param exposure_outcome covariance between exposure predictors and outcome predictors, vector in order of column
#' @param value_cor are given values (except `sig`) are correlation? (default = TRUE)
#' @references Setoguchi, S., Schneeweiss, S., Brookhart, M. A., Glynn, R. J., & Cook, E. F. (2008). \emph{Evaluating uses of data mining techniques in propensity score estimation: a simulation study}. Pharmacoepidemiology and Drug Safety, 17(6), 546–555 \url{https://doi.org/10.1002/pds.1555}
#' @details
#' This function builds covariance matrix of the covariates, which are
#' confounders (w1, w2, w3, w4), exposure predictors (w5, w6, w7), and outcome predictors(w8, w9, w10).
#' In each group, there is no correlation.
#' On the other hand, you can specify picewise correlation between e.g. confounder-exposure predictor.
#' @seealso \code{\link{sim_outcome}}
#' @importFrom Matrix bdiag symmpart
#' @export

build_covariate <- function(sig = rep(1L, 10),
                            confounder_exposure = c(.2, 0, 0, 0, .9, 0, 0, 0, 0, 0, 0, 0),
                            confounder_outcome = c(0, 0, 0, 0, 0, 0, .2, 0, 0, 0, .9, 0),
                            exposure_outcome = rep(0L, 9),
                            value_cor = TRUE) {
  confounder <- diag(sig[1:4])
  exposure <- diag(sig[5:7])
  outcome <- diag(sig[8:10])
  S <- bdiag(confounder, exposure, outcome)
  x <- S
  x[5:7, 1:4] <- confounder_exposure * 2
  x[8:10, 1:4] <- confounder_outcome * 2
  x[8:10, 5:7] <- exposure_outcome * 2
  x <- symmpart(x)
  if (value_cor) {
    sqrt(S) %*% x %*% sqrt(S) %>% as.matrix()
  } else {
    x %>% as.matrix()
  }
}

    ##3-3-2. sim_covariate
#' Simulating covariates
#'
#' @description
#' generates covariates in the references
#' @param n sample size
#' @param covmat Covariance matrix of the covariates
#' @references Setoguchi, S., Schneeweiss, S., Brookhart, M. A., Glynn, R. J., & Cook, E. F. (2008). \emph{Evaluating uses of data mining techniques in propensity score estimation: a simulation study}. Pharmacoepidemiology and Drug Safety, 17(6), 546–555 \url{https://doi.org/10.1002/pds.1555}
#' @references Lee, B. K., Lessler, J., & Stuart, E. A. (2010). \emph{Improving propensity score weighting using machine learning. Statistics in Medicine}. Statistics in Medicine, 29(3), 337-346. \url{https://doi.org/10.1002/sim.3782}
#' @details
#' This function reproduces the setting in the paper in Setoguchi et al. and Lee et al.
#' First generate binary covariates (w1, w3, w5, w6, w8, w9), and continuous covariates (w2, w4, w7, w10).
#' \enumerate{
#'  \item Generate 10-dim multivariate normal v1, v3, v5, v6, v8, v9 (corresponding to binary), and w7, w10
#'  \item Dichotomize w1, w3, w5, w6, w8, w9
#' }
#' @seealso \code{\link{build_covariate}}
#' @importFrom mvtnorm rmvnorm
#' @import data.table
#' @export

sim_covariate <- function(n, covmat = build_covariate()) {
  x <-
    rmvnorm(n, sigma = covmat) %>%
    data.table()
  new_col <- paste0("w", 1:10)
  setnames(
    x,
    old = paste0("V", 1:10),
    new = new_col
  )
  # dichotomize w1, w3, w5, w6, w8, w9------------------
  binary <- paste0("w", c(1, 3, 5, 6, 8, 9))
  x[,
    (binary) := lapply(.SD, function(x) {
      ifelse(x > mean(x), 1, 0)
    }),
    .SDcols = binary] %>%
    .[,
      w0 := 1]
  setcolorder(x, c("w0", new_col))
  x[]
}

    ## 3-3-3. Simulating dataset for various scenarios
#' Simulating Dataset for Various Scenarios
#'
#' @description
#' generates a dataset for various scenarios
#' @param n sample size
#' @param covmat Covariance matrix of the covariates
#' @param scenario scenarios
#' @param b coefficients for confounder and exposure predictors
#' @param a coefficients in outcome model
#' @param gam coefficient of exposure
#' @references Setoguchi, S., Schneeweiss, S., Brookhart, M. A., Glynn, R. J., & Cook, E. F. (2008). \emph{Evaluating uses of data mining techniques in propensity score estimation: a simulation study}. Pharmacoepidemiology and Drug Safety, 17(6), 546–555 \url{https://doi.org/10.1002/pds.1555}
#' @references Lee, B. K., Lessler, J., & Stuart, E. A. (2010). \emph{Improving propensity score weighting using machine learning. Statistics in Medicine}. Statistics in Medicine, 29(3), 337-346. \url{https://doi.org/10.1002/sim.3782}
#' @details
#' About scenarios:
#' \itemize{
#'  \item A: additivity and linearity
#'  \item B: mild non-linearity
#'  \item C: moderate non-linearity
#'  \item D: mild non-additivity
#'  \item E: mild non-additivity and non-linearity
#'  \item F: moderate non-linearity
#'  \item F: moderate non-additivity and non-linearity
#' }
#' See Appendix of Setoguchi et al.
#' @import data.table
#' @importFrom stats runif
#' @export

sim_outcome <- function(n, covmat = build_covariate(), scenario = LETTERS[1:7],
                        b = c(0, .8, -.25, .6, -.4, -.8, -.5, .7),
                        a = c(-3.85, .3, -.36, -73, -.2, .71, -.19, .26),
                        gam = -.4) {
  scenario <- match.arg(scenario)
  x <- sim_covariate(n, covmat)
  covariate <- paste0("w", 0:7)
  confounder <- paste0("w", 1:4)
  out_cov <- paste0("w", 8:10)
  if (scenario == "A") {
    x[,
      exposure_prob := Reduce("+", b * .SD[, .SD, .SDcols = covariate])]
  } else if (scenario == "B") {
    b <- c(b, b[2])
    x[,
      w2w2 := w2 * w2] %>%
      .[,
        exposure_prob := Reduce("+", b * .SD[, .SD, .SDcols = c(covariate, "w2w2")])]
  } else if (scenario == "C") {
    b <- c(b, b[2], b[4], b[7])
    x[,
      `:=` (
        w2w2 = w2 * w2,
        w4w4 = w4 * w4,
        w7w7 = w7 * w7
      )] %>%
      .[,
        exposure_prob := Reduce("+", b * .SD[, .SD, .SDcols = c(covariate, "w2w2", "w4w4", "w7w7")])]
  } else if (scenario == "D") {
    b <- c(b, .5 * b[1], .7 * b[2], .5 * b[4], .5 * b[5])
    x[,
      `:=` (
        w1w3 = w1 * w3,
        w2w4 = w2 * w4,
        w4w5 = w4 * w5,
        w5w6 = w5 * w6
      )] %>%
      .[,
        exposure_prob := Reduce("+", b * .SD[, .SD, .SDcols = c(covariate, "w1w3", "w2w4", "w4w5", "w5w6")])]
  } else if (scenario == "E") {
    b <- c(b, b[2], .5 * b[1], .7 * b[2], .5 * b[4], .5 * b[5])
    x[,
      `:=` (
        w2w2 = w2 * w2,
        w1w3 = w1 * w3,
        w2w4 = w2 * w4,
        w4w5 = w4 * w5,
        w5w6 = w5 * w6
      )] %>%
      .[,
        exposure_prob := Reduce("+", b * .SD[, .SD, .SDcols = c(covariate, "w2w2", "w1w3", "w2w4", "w4w5", "w5w6")])]
  } else if (scenario == "F") {
    b <- c(b, .5 * b[1], .7 * b[2], .5 * b[3], .7 * b[4], .5 * b[5], .5 * b[1], .7 * b[2], .5 * b[3], .5 * b[4], .5 * b[5])
    x[,
      `:=` (
        w1w3 = w1 * w3,
        w2w4 = w2 * w4,
        w3w5 = w3 * w5,
        w4w6 = w4 * w6,
        w5w7 = w5 * w7,
        w1w6 = w1 * w6,
        w2w3 = w2 * w3,
        w3w4 = w3 * w4,
        w4w5 = w4 * w5,
        w5w6 = w5 * w6
      )] %>%
      .[,
        exposure_prob := Reduce("+", b * .SD[, .SD, .SDcols = c(covariate, "w1w3", "w2w4", "w3w5", "w4w6", "w5w7", "w1w6", "w2w3", "w3w4", "w4w5", "w5w6")])]
  } else if (scenario == "G") {
    b <- c(
      b,
      b[2], b[4], b[7], # C
      .5 * b[1], .7 * b[2], .5 * b[4], .5 * b[5], # D
      .5 * b[3], .7 * b[4], .5 * b[5], .5 * b[1], .7 * b[2], .5 * b[3] # F
    )
    x[,
      `:=` (
        # C-----------
        w2w2 = w2 * w2,
        w4w4 = w4 * w4,
        w7w7 = w7 * w7,
        # D-----------
        w1w3 = w1 * w3,
        w2w4 = w2 * w4,
        w4w5 = w4 * w5,
        w5w6 = w5 * w6,
        # F-----------
        w3w5 = w3 * w5,
        w4w6 = w4 * w6,
        w5w7 = w5 * w7,
        w1w6 = w1 * w6,
        w2w3 = w2 * w3,
        w3w4 = w3 * w4
      )] %>%
      .[,
        exposure_prob := Reduce("+", b * .SD[, .SD, .SDcols = c(covariate, "w2w2", "w4w4", "w7w7", "w1w3", "w2w4", "w4w5", "w5w6", "w3w5", "w4w6", "w5w7", "w1w6", "w2w3", "w3w4")])]
  }
  x[,
    exposure_prob := (1 + exp(-exposure_prob))^(-1)] %>%
    .[,
      exposure := ifelse(exposure_prob > runif(n), 1, 0)] %>%
    .[,
      y := Reduce("+", c(a, gam) * .SD[, .SD, .SDcols = c(confounder, out_cov, "exposure")])]
  bin_cols <- c("exposure", paste0("w", c(1, 3, 5, 6, 8, 9)))
  x[,
    (bin_cols) := lapply(.SD, factor),
    .SDcols = bin_cols]
  x[, .SD, .SDcols = c(paste0("w", 1:10), "exposure", "y", "exposure_prob")]
}

    ## 3-3-4. Simulation Setting

#' Simulation Setting
#'
#' @description
#' generates multiple sets of data-set
#' @param N number of data set generation
#' @param n_dat sample size
#' @param covmat Covariance matrix of the covariates
#' @param scenario scenarios
#' @param b coefficients for confounder and exposure predictors
#' @param a coefficients in outcome model
#' @param gam coefficient of exposure
#' @param parallel parallelize? By default, `FALSE`.
#' @param mc_core The number of cores to use for MC simulation
#' @references Setoguchi, S., Schneeweiss, S., Brookhart, M. A., Glynn, R. J., & Cook, E. F. (2008). \emph{Evaluating uses of data mining techniques in propensity score estimation: a simulation study}. Pharmacoepidemiology and Drug Safety, 17(6), 546–555 \url{https://doi.org/10.1002/pds.1555}
#' @references Lee, B. K., Lessler, J., & Stuart, E. A. (2010). \emph{Improving propensity score weighting using machine learning. Statistics in Medicine}. Statistics in Medicine, 29(3), 337-346. \url{https://doi.org/10.1002/sim.3782}
#' @details
#' This function constructs \link[data.table]{data.table-class} for simulation using \code{\link{sim_outcome}}.
#' \code{mcname} represents the index of the replicate.
#' @import data.table
#' @import foreach
#' @importFrom parallel mclapply
#' @export
mc_setoguchi <- function(N, n_dat, covmat = build_covariate(), scenario = LETTERS[1:7],
                         b = c(0, .8, -.25, .6, -.4, -.8, -.5, .7),
                         a = c(-3.85, .3, -.36, -73, -.2, .71, -.19, .26),
                         gam = -.4, parallel = FALSE, mc_core = 1) {
  if (parallel) {
    mc_list <- foreach(s = scenario, .combine = rbind) %dopar% {
      mclapply(
        1:N,
        function(id) {
          sim_outcome(n_dat, covmat, s, b, a, gam) %>%
            .[,
              mcname := id]
        },
        mc.cores = mc_core
      ) %>%
        rbindlist() %>%
        .[,
          scenario := s]
    }
    # mc_list <- foreach(i = 1:N, .combine = rbind) %dopar% {
    #   sim_outcome(n_dat, covmat, scenario, b, a, gam)[, mcname := i]
    # }
  } else {
    mc_list <- foreach(s = scenario, .combine = rbind) %do% {
      mclapply(
        1:N,
        function(id) {
          sim_outcome(n_dat, covmat, s, b, a, gam) %>%
            .[,
              mcname := id]
        },
        mc.cores = mc_core
      ) %>%
        rbindlist() %>%
        .[,
          scenario := s]
    }
    # mc_list <- foreach(i = 1:N, .combine = rbind) %do% {
    #   sim_outcome(n_dat, covmat, scenario, b, a, gam)[, mcname := i]
    # }
  }
  mc_list
}

##4. weighting.R : add_propensity, add_ipw_wt, compute_sipw, add_weighting

  ##4-1. Add estimated propensity score to a data frame. 

#' @description
#' adds propensity score to a data frame
#' @param data A data frame to be used.
#' @param object A \code{propmod} object if already fitted.
#' @param formula If not, write a \link[stats]{formula} to be fitted. Remember that you don't have to worry about group variable. \link[data.table]{.SD} do exclude `by`.
#' @param method Estimating methods
#' \itemize{
#'  \item "logit" - \code{\link{ps_glm}}
#'  \item "rf" - \code{\link{ps_rf}}
#'  \item "cart" - \code{\link{ps_cart}}
#' }
#' @param var The name of the propensity score column.
#' @param mc_col Indicator column name for MC simulation if exists
#' @param sc_col Indicator column name for various scenarios if exists
#' @param parallel parallelize some operation
#' @param ... Additional arguments of fitting functions
#' @import data.table foreach
#' @export

add_propensity <- function(data, object = NULL, formula = NULL, method = c("logit", "rf", "cart"), var = "propensity", mc_col = NULL, sc_col = NULL, parallel = FALSE, ...) {
  method <- match.arg(method)
  if (is.data.table(data)) {
    data <- copy(data)
  } else {
    data <- copy(data)
    setDT(data)
  }
  if (!is.null(object)) {
    data[[var]] <- estimate_ps(object, ...)
    data
  }
  # for each method-------------------------------------
  switch(
    method,
    "logit" = {
      # glm------------------------------------
      if (!is.null(mc_col) & !is.null(sc_col)) {
        if (parallel) {
          foreach(sc_id = data[,get(sc_col)] %>% unique(), .combine = rbind) %:%
            foreach(mc_id = data[get(sc_col) == sc_id, get(mc_col)] %>% unique(), .combine = rbind) %dopar% {
              # sc_dt <- copy(data[get(sc_col) == sc_id & get(mc_col) == mc_id, .SD, .SDcols = -c(sc_col, mc_col)])
              data[get(sc_col) == sc_id & get(mc_col) == mc_id, .SD, .SDcols = -c(sc_col, mc_col)] %>%
                .[,
                  (var) := ps_glm(formula, data = .SD, ...) %>%
                    estimate_ps()] %>%
                .[,
                  (mc_col) := mc_id] %>%
                .[,
                  (sc_col) := sc_id]
            }
        } else {
          foreach(sc_id = data[,get(sc_col)] %>% unique(), .combine = rbind) %:%
            foreach(mc_id = data[get(sc_col) == sc_id, .SD, .SDcols = -sc_col] %>% .[,get(mc_col)] %>% unique(), .combine = rbind) %do% {
              # sc_dt <- copy(data[get(sc_col) == sc_id & get(mc_col) == mc_id, .SD, .SDcols = -c(sc_col, mc_col)])
              data[get(sc_col) == sc_id & get(mc_col) == mc_id, .SD, .SDcols = -c(sc_col, mc_col)] %>%
                .[,
                  (var) := ps_glm(formula, data = .SD, ...) %>%
                    estimate_ps()] %>%
                .[,
                  (mc_col) := mc_id] %>%
                .[,
                  (sc_col) := sc_id]
            }
        }
      } else {
        data[,
             (var) := ps_glm(formula, data = .SD, ...) %>%
               estimate_ps(),
             by = mc_col]
        data[]
      }
      #---------------------------------------
    },
    "rf" = {
      # randomForest---------------------------
      if (!is.null(mc_col) & !is.null(sc_col)) {
        if (parallel) {
          foreach(sc_id = data[,get(sc_col)] %>% unique(), .combine = rbind) %:%
            foreach(mc_id = data[get(sc_col) == sc_id, get(mc_col)] %>% unique(), .combine = rbind) %dopar% {
              # sc_dt <- copy(data[get(sc_col) == sc_id & get(mc_col) == mc_id, .SD, .SDcols = -c(sc_col, mc_col)])
              data[get(sc_col) == sc_id & get(mc_col) == mc_id, .SD, .SDcols = -c(sc_col, mc_col)] %>%
                .[,
                  (var) := ps_rf(formula, data = .SD, ...) %>%
                    estimate_ps()] %>%
                .[,
                  (mc_col) := mc_id] %>%
                .[,
                  (sc_col) := sc_id]
            }
        } else {
          foreach(sc_id = data[,get(sc_col)] %>% unique(), .combine = rbind) %:%
            foreach(mc_id = data[get(sc_col) == sc_id, .SD, .SDcols = -sc_col] %>% .[,get(mc_col)] %>% unique(), .combine = rbind) %do% {
              # sc_dt <- copy(data[get(sc_col) == sc_id, .SD, .SDcols = -(sc_col, mc_col)])
              data[get(sc_col) == sc_id & get(mc_col) == mc_id, .SD, .SDcols = -sc_col] %>%
                .[,
                  (var) := ps_rf(formula, data = .SD, ...) %>%
                    estimate_ps()] %>%
                .[,
                  (mc_col) := mc_id] %>%
                .[,
                  (sc_col) := sc_id]
            }
        }
      } else {
        data[,
             (var) := ps_rf(formula, data = .SD, ...) %>%
               estimate_ps(),
             by = mc_col]
        data[]
      }
      #---------------------------------------
    },
    "cart" = {
      # rpart--------------------------------------
      if (!is.null(mc_col) & !is.null(sc_col)) {
        if (parallel) {
          foreach(sc_id = data[,get(sc_col)] %>% unique(), .combine = rbind) %:%
            foreach(mc_id = data[get(sc_col) == sc_id, get(mc_col)] %>% unique(), .combine = rbind) %dopar% {
              data[get(sc_col) == sc_id & get(mc_col) == mc_id, .SD, .SDcols = -c(sc_col, mc_col)] %>%
                .[,
                  (var) := ps_cart(formula, data = .SD, ...) %>%
                    estimate_ps()] %>%
                .[,
                  (mc_col) := mc_id] %>%
                .[,
                  (sc_col) := sc_id]
            }
        } else {
          foreach(sc_id = data[,get(sc_col)] %>% unique(), .combine = rbind) %:%
            foreach(mc_id = data[get(sc_col) == sc_id, .SD, .SDcols = -sc_col] %>% .[,get(mc_col)] %>% unique(), .combine = rbind) %do% {
              data[get(sc_col) == sc_id & get(mc_col) == mc_id, .SD, .SDcols = -c(sc_col, mc_col)] %>%
                .[,
                  (var) := ps_cart(formula, data = .SD, ...) %>%
                    estimate_ps()] %>%
                .[,
                  (mc_col) := mc_id] %>%
                .[,
                  (sc_col) := sc_id]
            }
        }
      } else {
        data[,
             (var) := ps_cart(formula, data = .SD, ...) %>%
               estimate_ps(),
             by = mc_col]
        data[]
      }
      #---------------------------------------
    }
  )
}

  ## 4-2. weighting

add_ipw_wt <- function(data, treatment, trt_indicator = 1, object = NULL, formula = NULL, method = c("logit", "rf", "cart"), mc_col = NULL, sc_col = NULL, parallel = FALSE, ...) {
  if (is.data.table(data)) {
    data <- copy(data)
  } else {
    data <- copy(data %>% data.table())
  }
  data %>%
    add_propensity(object = object, formula = formula, method = method, mc_col = mc_col, sc_col = sc_col, parallel = parallel, ...) %>%
    .[,
      treatment := ifelse(get(treatment) == trt_indicator, 1, 0)] %>%
    .[,
      ipw_wt := treatment / propensity - (1 - treatment) / (1 - propensity)] %>%
    .[]
}

  ## 4-3. Estimation of IPW
#' @description
#' estimates inverse probability weighting (IPW) based on propensity score estimates
#' @param data A data frame to be used
#' @param treatment Treatment variable name
#' @param trt_indicator Value that indicates the unit is treated
#' @param outcome Outcome variable name
#' @param weight If weighting column exists, it can be specified for computing efficiency
#' @param object A \code{propmod} object if already fitted.
#' @param formula If not, write a \link[stats]{formula} to be fitted. Remember that you don't have to worry about group variable. \link[data.table]{.SD} do exclude `by`.
#' @param method Estimating methods
#' \itemize{
#'  \item "logit" - \code{\link{ps_glm}}
#'  \item "rf" - \code{\link{ps_rf}}
#'  \item "cart" - \code{\link{ps_cart}}
#' }
#' @param mc_col Indicator column name for MC simulation if exists
#' @param sc_col Indicator column name for various scenarios if exists
#' @param parallel parallelize some operation
#' @param ... Additional arguments of fitting functions
#' @import data.table
#' @export

compute_ipw <- function(data, treatment, trt_indicator = 1, outcome, weight = NULL,
                        object = NULL, formula = NULL, method = c("logit", "rf", "cart"),
                        mc_col = NULL, sc_col = NULL, parallel = FALSE, ...) {
  if (is.data.table(data)) {
    data <- copy(data)
  } else {
    data <- copy(data %>% data.table())
  }
  # for single group--------------
  mc <- c(mc_col, sc_col)
  if (any(is.null(mc))) mc <- NULL
  #-------------------------------
  if (is.null(weight)) {
    data <-
      data %>%
      add_ipw_wt(treatment = treatment, trt_indicator = trt_indicator, object = object, formula = formula, method = method, mc_col = mc_col, sc_col = sc_col, parallel = parallel, ...) %>%
      .[,
        .(IPW = mean(ipw_wt * get(outcome))),
        by = mc]
    return(data)
  }
  data %>%
    .[,
      .(IPW = mean(get(weight) * get(outcome))),
      by = mc]
}

  ##4-4. Estimation of Stabilized Inverse Probability Weighting(SIPW)

#' @description
#' estimates stabilized inverse probability weighting (SIPW) based on propensity score estimates
#' @param data A data frame to be used
#' @param treatment Treatment variable name
#' @param trt_indicator Value that indicates the unit is treated
#' @param outcome Outcome variable name
#' @param weight If weighting column exists, it can be specified for computing efficiency
#' @param object A \code{propmod} object if already fitted.
#' @param formula If not, write a \link[stats]{formula} to be fitted. Remember that you don't have to worry about group variable. \link[data.table]{.SD} do exclude `by`.
#' @param method Estimating methods
#' \itemize{
#'  \item "logit" - \code{\link{ps_glm}}
#'  \item "rf" - \code{\link{ps_rf}}
#'  \item "cart" - \code{\link{ps_cart}}
#' }
#' @param mc_col Indicator column name for MC simulation if exists
#' @param sc_col Indicator column name for various scenarios if exists
#' @param parallel parallelize some operation
#' @param ... Additional arguments of fitting functions
#' @import data.table
#' @export
compute_sipw <- function(data, treatment, trt_indicator = 1, outcome, weight = NULL,
                         object = NULL, formula = NULL, method = c("logit", "rf", "cart"),
                         mc_col = NULL, sc_col = NULL, parallel = FALSE, ...) {
  if (is.data.table(data)) {
    data <- copy(data)
  } else {
    data <- copy(data %>% data.table())
  }
  # for single group--------------
  mc <- c(mc_col, sc_col)
  if (any(is.null(mc))) mc <- NULL
  #-------------------------------
  if (is.null(weight)) {
    data <-
      data %>%
      add_ipw_wt(treatment = treatment, trt_indicator = trt_indicator, object = object, formula = formula, method = method, mc_col = mc_col, sc_col = sc_col, parallel = parallel, ...) %>%
      .[,
        sipw_wt := ipw_wt / sum(ipw_wt),
        by = treatment] %>%
      .[,
        .(SIPW = sum(sipw_wt * get(outcome))),
        by = mc]
    return(data)
  }
  data %>%
    .[,
      sipw_wt := get(weight) / sum(get(weight)),
      by = treatment] %>%
    .[,
      .(SIPW = sum(sipw_wt * get(outcome))),
      by = mc]
}

  ##4-5 Treatment Effect Estimation Using Propensity Scores

#' @description
#' estimates treatment effect based on ps estimation (e.g. inverse probability treatment weighting)
#' @param data A data frame to be used
#' @param treatment Treatment variable name
#' @param trt_indicator Value that indicates the unit is treated
#' @param object A \code{propmod} object if already fitted.
#' @param formula If not, write a \link[stats]{formula} to be fitted. Remember that you don't have to worry about group variable. \link[data.table]{.SD} do exclude `by`.
#' @param method Estimating methods
#' \itemize{
#'  \item "logit" - \code{\link{ps_glm}}
#'  \item "rf" - \code{\link{ps_rf}}
#'  \item "cart" - \code{\link{ps_cart}}
#' }
#' @param mc_col Indicator column name for MC simulation if exists
#' @param sc_col Indicator column name for various scenarios if exists
#' @param parallel parallelize some operation
#' @param ... Additional arguments of fitting functions
#' @details
#' This functions add columns by
#' \deqn{\frac{trt_i}{\hat{e}_i} - \frac{1- trt_i}{1 - \hat{e}_i}}
#' and
#' \deqn{trt_i - (1 - trt_i) \frac{\hat{e}_i}{1 - \hat{e}_i}}
#' @references Lee, B. K., Lessler, J., & Stuart, E. A. (2010). \emph{Improving propensity score weighting using machine learning. Statistics in Medicine}. Statistics in Medicine, 29(3), 337-346.
#' @references Pirracchio, R., Petersen, M. L., & Laan, M. van der. (2015). \emph{Improving Propensity Score Estimators’ Robustness to Model Misspecification Using Super Learner}. American Journal of Epidemiology, 181(2), 108–119. \url{https://doi.org/10.1093/aje/kwu253}
#' @seealso
#' \code{\link{add_propensity}}
#' @import data.table
#' @export

add_weighting <- function(data, treatment, trt_indicator = 1, object = NULL, formula = NULL, method = c("logit", "rf", "cart"), mc_col = NULL, sc_col = NULL, parallel = FALSE, ...) {
  if (is.data.table(data)) {
    data <- copy(data)
  } else {
    data <- copy(data %>% data.table())
  }
  data %>%
    add_propensity(object = object, formula = formula, method = method, mc_col = mc_col, sc_col = sc_col, parallel = parallel, ...) %>%
    .[,
      treatment := ifelse(get(treatment) == trt_indicator, 1, 0)] %>%
    .[,
      `:=`(
        iptw = treatment / propensity - (1 - treatment) / (1 - propensity),
        propwt = treatment - (1 - treatment) * propensity / (1 - propensity)
      )] %>%
    # .[,
    #   iptw := treatment / propensity + (1 - treatment) / (1 - propensity)] %>%
    # .[,
    #   `:=`(treatment = NULL, propensity = NULL)] %>%
    .[,
      treatment := NULL] %>%
    .[]
}


## 5. test_code

  ##5-1. testthat.R

  ##5-2 test-propensity.R
test_that(
  "Fitting GLM for propensity",
  {
    fit <-
      chemical %>%
      ps_glm(poisox ~ age + sex, data = .)
    expect_s3_class(fit, "propmod")
    expect_s3_class(fit$model, "glm")
    expect_length(fit, 3)
  }
)

test_that(
  "Fitting random forests for propensity",
  {
    fit <-
      chemical %>%
      ps_rf(poisox ~ age + sex, data = .)
    expect_s3_class(fit, "propmod")
    expect_s3_class(fit$model, "randomForest")
    expect_length(fit, 3)
  }
)
test_package("propensityml")
