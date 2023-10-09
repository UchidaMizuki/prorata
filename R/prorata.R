#' @export
prorata <- function(y, x,
                    type = c("squared", "absolute"),
                    control = control_prorata()) {
  check_y_x_prorata(y, x)
  x <- x_prorata(x)

  if (is.function(type)) {
    fn <- type
  } else {
    type <- rlang::arg_match(type, c("squared", "absolute"))

    fn <- switch(
      type,
      squared = \(truth, estimate) {
        error <- truth - estimate
        objective <- sum(error^2)
        gradient <- apply(sweep(-x, 1:2, 2 * error, `*`), 3, sum)

        list(objective = objective,
             gradient  = gradient)
      },
      absolute = \(truth, estimate) {
        error <- truth - estimate
        objective <- sum(abs(error))
        gradient <- apply(sweep(-x, 1:2, error / abs(error), `*`), 3, sum)

        list(objective = objective,
             gradient  = gradient)
      }
    )
  }

  K <- dim(x)[[3]]
  nloptr <- nloptr::nloptr(x0 = rep(1 / K, K),
                           eval_f = \(weights) fn(y, predict_prorata(weights, x)),
                           lb = rep(0, K),
                           ub = rep(1, K),
                           eval_g_eq = \(weights) list(constraints = sum(weights) - 1,
                                                       jacobian = rep(1, K)),
                           opts = control)
  structure(list(weights = nloptr[["solution"]],
                 nloptr = nloptr),
            class = "prorata")
}

#' @export
control_prorata <- function(algorithm = "NLOPT_LD_SLSQP",
                            xtol_rel = 1e-10,
                            maxeval = 1e6,
                            ...) {
  structure(rlang::list2(algorithm = algorithm,
                         xtol_rel = xtol_rel,
                         maxeval = maxeval,
                         ...),
            class = "control_prorata")
}

#' @export
predict.prorata <- function(object, new_data, ...) {
  out <- new_data[, , 1]
  out[] <- predict_prorata(object[["weights"]], new_data)
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

x_prorata <- function(x) {
  x_total <- apply(x, c(1, 3), sum)
  sweep(x, c(1, 3), x_total, `/`)
}

predict_prorata <- function(weights, x) {
  apply(sweep(x, 3, weights, `*`), 1:2, sum)
}

check_new_data_prorata <- function(new_data) {
  new_data_total <- apply(new_data, c(1, 3), sum)
  stopifnot(
    apply(new_data_total, 1, near_all)
  )
}
