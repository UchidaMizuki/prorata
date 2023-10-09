#' @export
prorata <- function(y, x,
                    type = c("squared", "absolute"),
                    f = NULL,
                    grad = NULL,
                    control = control_prorata(),
                    ...) {
  check_y_x_prorata(y, x)

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
        apply(sweep(-x, 1:2, 2 * (y - y_pred), `*`), 3, sum)[-1]
      },
      absolute = \(par) {
        y_pred <- predict_prorata(x = x,
                                  weights = get_weights(par))
        apply(sweep(-x, 1:2, sign(y - y_pred), `*`), 3, sum)[-1]
      }
    )
  }

  K <- dim(x)[[3]]
  opt <- stats::constrOptim(theta = rep(1 / K, K - 1),
                            f = f,
                            grad = grad,
                            ui = rbind(-rep(1, K - 1),
                                       diag(K - 1)),
                            ci = c(-1, rep(0, K - 1)),
                            control = control,
                            ...)
  structure(list(weights = get_weights(opt[["par"]])),
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
  out <- new_data[, , 1]
  out[] <- predict_prorata(x = new_data,
                           weights = object[["weights"]])
  out
}

#' @export
print.prorata <- function(x, ...) {
  weights <- big_mark(x$weights)
  cli::cli_inform(c("prorata",
                    "*" = "{length(weights)} cluster{?s}",
                    "*" = "weights: {weights}"))
  invisible(x)
}

check_y_x_prorata <- function(y, x,
                              frequency = FALSE) {
  dm_y <- dim(y)
  dm_x <- dim(x)

  stopifnot(
    !frequency || rlang::is_integerish(y),
    length(dm_y) == 2,
    length(dm_x) == 3,
    dm_y[[1]] == dm_x[[1]],
    dm_y[[2]] == dm_x[[2]],
    all(y >= 0),
    all(x >= 0)
  )
}

predict_prorata <- function(x, weights) {
  apply(sweep(x, 3, weights, `*`), 1:2, sum)
}

check_new_data_prorata <- function(new_data) {
  new_data_total <- apply(new_data, c(1, 3), sum)
  stopifnot(
    apply(new_data_total, 1, near_all)
  )
}
