#' Fitting Logistic Regression for Propensity Score
#'
#' @description
#' fits logistic regression model before estimating propensity scores
#' @param formula an object \link[stats]{formula} to be fitted. Response should be treatment.
#' @param data optional data frame.
#' @param ... For additional options of \link[stats]{glm}.
#' @return \code{\link{propmod}} class, a list with model and its name
#' \itemize{
#'  \item model - \link[stats]{glm} model
#'  \item name - "glm"
#'  \item data - dataset
#' }
#' @references Rosenbaum, P. R., & Rubin, D. B. (1983). \emph{The central role of the propensity score in observational studies for causal effects}. Biometrika, 70(1), 41-55. \url{https://doi.org/10.1093/biomet/70.1.41}
#' @details
#' Propensity score is
#' \deqn{e(X) = P(Z_i = 1 \mid X_i = x)},
#' which is the conditional probability of receiving treatment.
#' Naturally, logit model is the easiest way to estimate the score.
#' @importFrom stats glm binomial
#' @examples
#' fit <- chemical %>% ps_glm(poisox ~ age + sex, data = .)
#' @export
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

#' Fitting Random Forests for Propensity Score
#'
#' @description
#' fits random forests before estimating propensity scores
#' @param formula an object \link[stats]{formula} to be fitted. Response should be treatment.
#' @param data optional data frame. REMEMBER that treatment should be `TRUE` or `1`.
#' @param ... For additional options of \link[randomForest]{randomForest}.
#' @return \code{\link{propmod}} class, a list with model and its name
#' \itemize{
#'  \item model - \link[randomForest]{randomForest}
#'  \item name - "rf"
#'  \item data - dataset
#' }
#' @references Lee, B. K., Lessler, J., & Stuart, E. A. (2010). \emph{Improving propensity score weighting using machine learning. Statistics in Medicine}. Statistics in Medicine, 29(3), 337-346. \url{https://doi.org/10.1002/sim.3782}
#' @importFrom randomForest randomForest
#' @examples
#' fit <- chemical %>% ps_rf(poisox ~ age + sex, data = .)
#' @export
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

#' Fitting CART for Propensity Score
#'
#' @description
#' fits classification tree before estimating propensity scores
#' @param formula an object \link[stats]{formula} to be fitted. Response should be treatment.
#' @param data optional data frame.
#' @param ... For additional options of \link[rpart]{rpart}.
#' @return \code{\link{propmod}} class, a list with model and its name
#' \itemize{
#'  \item model - \link[rpart]{rpart}
#'  \item name - "cart"
#'  \item data - dataset
#' }
#' @references Lee, B. K., Lessler, J., & Stuart, E. A. (2010). \emph{Improving propensity score weighting using machine learning. Statistics in Medicine}. Statistics in Medicine, 29(3), 337-346. \url{https://doi.org/10.1002/sim.3782}
#' @importFrom rpart rpart
#' @export
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

#' Plot a Complexity Parameter Table
#'
#' @description
#' Before pruning, plot complexity parameter from CART.
#' @param object fitted \code{\link{propmod}} object. Its `name` should be "cart"
#' @param ... Additional arguments for \link[ggplot2]{geom_pointrange}
#' @importFrom ggplot2 ggplot aes geom_path geom_pointrange labs
#' @export
plot_cp <- function(object, ...) {
  if (object$name != "cart") stop("Model should be cart")
  object$model$cptable %>%
    as.data.frame() %>%
    ggplot(aes(x = nsplit + 1, y = xerror)) +
    geom_path() +
    geom_pointrange(aes(ymin = xerror - xstd, ymax = xerror + xstd), ...) +
    labs(
      x = "Size of Tree",
      y = "X-val Relative Error"
    )
}

#' Pruning CART
#'
#' @description
#' prunes the result of \code{\link{ps_cart}}.
#' @param object fitted \code{\link{propmod}} object. Its `name` should be "cart"
#' @param cp Complexity parameter for pruning the tree
#' @param ... Additional arguments for \link[rpart]{prune}.
#' @importFrom rpart prune
#' @export
ps_prune <- function(object, cp, ...) {
  if (object$name != "cart") stop("Model should be cart")
  result <- prune(object$model, cp, ...)
  res <- structure(list(
    model = result,
    name = "pruned",
    data = object$data
  ))
  class(res) <- "propmod"
  res
}

# Estimate-------------------------------------

#' Estimation of Propensity Score
#'
#' @description
#' estimates propensity score based on the given model
#' @param object fitted \code{\link{propmod}} object
#' @param ... additional arguments for \link[stats]{predict}
#' @seealso
#' \code{\link{ps_glm}}
#' \code{\link{ps_rf}}
#' \code{\link{ps_cart}}
#' \code{\link{ps_svm}}
#' @importFrom stats predict
#' @export
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
  } else if (object$name == "SVM") {
    pred <-
      predict(object$model, object$data, probability = TRUE) %>%
      attr("probabilities")
    trt_lev <-
      ifelse(
        which(c("1", "TRUE") %in% colnames(pred)) == 1,
        1,
        TRUE
      )
    return(pred[, colnames(pred) == trt_lev])
  }
}

# Propensity score model class-----------------

#' `propmod` class
#'
#' @description
#' The \code{propmod} class ready for computing propensity score
#' @name propmod-class
#' @rdname propmod-class
#' @aliases propmod cvar-class
#' @importFrom methods setOldClass
#' @exportClass propmod
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
