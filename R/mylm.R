
#' the same function as the R function lm()
#'
#' @param formula
#' @param data
#' @param subset
#' @param weights
#' @param na.action
#' @param method
#' @param model
#' @param x
#' @param y
#' @param qr
#' @param singular.ok
#' @param contrasts
#' @param offset
#' @param ...
#'
#' @return lm object
#' @export
#'
#' @examples
#' mylm.basic(y~x1+x2,data)
mylm.basic <- function (formula, data, subset, weights, na.action, method = "qr",
                                  model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,                                 contrasts = NULL, offset, ...)
{
  ret.x <- x
  ret.y <- y
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action",
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  if (method == "model.frame")
    return(mf)
  else if (method != "qr")
    warning(gettextf("method = '%s' is not supported. Using 'qr'",
                     method), domain = NA)
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  w <- as.vector(model.weights(mf))
  if (!is.null(w) && !is.numeric(w))
    stop("'weights' must be a numeric vector")
  offset <- model.offset(mf)
  mlm <- is.matrix(y)
  ny <- if (mlm)
    nrow(y)
  else length(y)
  if (!is.null(offset)) {
    if (!mlm)
      offset <- as.vector(offset)
    if (NROW(offset) != ny)
      stop(gettextf("number of offsets is %d, should equal %d (number of observations)",
                    NROW(offset), ny), domain = NA)
  }
  if (is.empty.model(mt)) {
    x <- NULL
    z <- list(coefficients = if (mlm) matrix(NA_real_, 0,
                                             ncol(y)) else numeric(), residuals = y, fitted.values = 0 *
                y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w !=
                                                                                0) else ny)
    if (!is.null(offset)) {
      z$fitted.values <- offset
      z$residuals <- y - offset
    }
  }
  else {
    x <- model.matrix(mt, mf, contrasts)
    z <- if (is.null(w))
      lm.fit(x, y, offset = offset, singular.ok = singular.ok,
             ...)
    else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok,
                 ...)
  }
  class(z) <- c(if (mlm) "mlm", "lm")
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  if (model)
    z$model <- mf
  if (ret.x)
    z$x <- x
  if (ret.y)
    z$y <- y
  if (!qr)
    z$qr <- NULL
  z
}


#' Linear regression with summary and plots
#'
#' @param formula
#' @param data
#'
#' @return lm model
#' @export
#'
#' @examples
#' mylm.detailed(Y~x1+x2,data)
mylm.detailed <- function(formula,data) {
  model <- lm(formula,data)
  summary(model)
  plot(model)
  return(model)
}

#' Plot histograms of data and calculate common statistics
#'
#' @param data
#'
#' @return None
#' @export
#'
#' @examples describe.data(data)
describe.data <- function(data){
  for (i in colnames(data)){
    hist(data[,i],main = i)
  }
  summary(data)
}


#' Make predictions and plot graphs using the incoming model
#'
#' @param model
#' @param x
#'
#' @return list
#' @export
#'
#' @examples mylm.predict(model, x)
mylm.predict <- function(model,x){
  y.pred <- predict(model,x)
  plot(x,y)
  return(y.pred)
}



