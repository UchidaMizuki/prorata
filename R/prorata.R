#' @export
prorata <- function(y, x,
                    type = c("squared", "absolute"),
                    f = NULL,
                    grad = NULL,
                    control = control_prorata(),
                    ...) {
  data <- prepare_y_x_prorata(y, x)
  y <- data$y
  x <- data$x

  get_weights <- \(par) c(1 - sum(par), par)

  if (is.null(f)) {
    type <- rlang::arg_match(type, c("squared", "absolute"))

    f <- switch(
      type,
      squared = \(par) {
        y_pred <- predict_prorata(x = x,
                                  weights = get_weights(par))
        sum((y - y_pred)^2)
      },
      absolute = \(par) {
        y_pred <- predict_prorata(x = x,
                                  weights = get_weights(par))
        sum(abs(y - y_pred))
      }
    )

    grad <- switch(
      type,
      squared = \(par) {
        y_pred <- predict_prorata(x = x,
                                  weights = get_weights(par))
        as.double(2 * (y - y_pred) %*% -x)[-1]
      },
      absolute = \(par) {
        y_pred <- predict_prorata(x = x,
                                  weights = get_weights(par))
        as.double(sign(y - y_pred) %*% -x)[-1]
      }
    )
  }

  K <- dim(x)[[2]]
  opt <- stats::constrOptim(theta = rep(1 / K, K - 1),
                            f = f,
                            grad = grad,
                            ui = rbind(-rep(1, K - 1),
                                       diag(K - 1)),
                            ci = c(-1, rep(0, K - 1)),
                            control = control,
                            ...)
  structure(get_weights(opt[["par"]]),
            class = "prorata")
}

#' @export
control_prorata <- function(verbose = FALSE,
                            max_iter = 1e3,
                            ...) {
  structure(rlang::list2(trace = verbose,
                         maxit = max_iter,
                         ...),
            class = "control_prorata")
}

#' @export
predict.prorata <- function(object, new_data, ...) {
  check_new_data_prorata(new_data)

  dm <- dim(new_data)
  dim(x) <- c(prod(dm[1:2]), dm[[3]])
  pred <- predict_prorata(x = x,
                          weights = object)
  dim(pred) <- dim(new_data)[1:2]

  out <- new_data[, , 1]
  out[] <- pred
  out
}

prepare_y_x_prorata <- function(y, x) {
  dm_y <- dim(y)
  dm_x <- dim(x)

  stopifnot(
    length(dm_y) == 2,
    length(dm_x) == 3,
    dm_y[[1]] == dm_x[[1]],
    dm_y[[2]] == dm_x[[2]],
    all(y >= 0),
    all(x >= 0)
  )

  y_total <- rowSums(y)
  x_total <- apply(x, c(1, 3), sum)

  y <- as.double(y)

  x <- sweep(x, c(1, 3), x_total, `/`)
  x <- sweep(x, 1, y_total, `*`)
  dim(x) <- c(prod(dm_x[1:2]), dm_x[[3]])

  list(y = y,
       x = x)
}

predict_prorata <- function(x, weights) {
  as.double(x %*% weights)
}

check_new_data_prorata <- function(new_data) {
  new_data_total <- apply(new_data, c(1, 3), sum)

  if (!all(apply(new_data_total, 1, near_all))) {
    rlang::warn("!all(apply(new_data_total, 1, near_all))")
  }
}
