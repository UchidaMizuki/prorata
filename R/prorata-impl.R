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

prorata_impl <- function(log_dens, control) {
  dens <- exp(log_dens)
  weights <- 1 / dim(log_dens)[[2]]
  log_lik_pred <- -Inf

  iter <- 1
  while (iter < control[["max_iter"]]) {
    iter <- iter + 1

    dens_weighted <- sweep(dens, 2, weights, `*`)

    dens_weighted_total <- apply(dens_weighted, 1, sum)

    log_lik <- sum(log(dens_weighted_total))
    if (control[["verbose"]]) {
      cli::cli_inform("Iteration: {iter}, Log-Likelihood: {log_lik}")
    }
    if (log_lik - log_lik_pred < control[["tolerance"]]) {
      break
    }
    log_lik_pred <- log_lik

    responsibility <- sweep(dens_weighted, 1, dens_weighted_total, `/`)
    weights <- apply(responsibility, 2, mean)
  }
  structure(list(weights = weights),
            class = "prorata")
}

#' @export
control_prorata <- function(max_iter = Inf,
                            tolerance = 1e-6,
                            verbose = FALSE) {
  structure(list(max_iter = max_iter,
                 tolerance = tolerance,
                 verbose = verbose),
            class = "control_prorata")
}

#' @export
predict.prorata <- function(object, new_data, ...) {
  out <- new_data[, , 1]
  out[] <- apply(sweep(new_data, 3, object[["weights"]], `*`),
                 1:2, sum)
  out
}

x_prorata <- function(x) {
  x_total <- apply(x, c(1, 3), sum)
  sweep(x, c(1, 3), x_total, `/`)
}

check_new_data_prorata <- function(new_data) {
  new_data_total <- apply(new_data, c(1, 3), sum)
  stopifnot(
    apply(new_data_total, 1, near_all)
  )
}
