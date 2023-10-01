#' @export
pro_rata <- function(y, x,
                     control = control_pro_rata()) {
  dens <- dens_pro_rata(y, x)

  weights <- 1 / dim(x)[[3]]
  log_lik_pred <- -Inf

  iter <- 1
  while (iter < control[["max_iter"]]) {
    iter <- iter + 1

    dens_weighted <- sweep(dens, 3, weights, `*`)
    dens_weighted_total <- apply(dens_weighted, 1:2, sum)

    log_lik <- sum(log(dens_weighted_total))
    if (control[["verbose"]]) {
      cli::cli_inform("Iteration: {iter}, Log-Likelihood: {log_lik}")
    }
    if (log_lik - log_lik_pred < control[["tolerance"]]) {
      break
    }
    log_lik_pred <- log_lik

    responsibility <- sweep(dens_weighted, 1:2, dens_weighted_total, `/`)
    weights <- apply(responsibility, 3, mean)
  }
  structure(list(weights = weights),
            class = "pro_rata")
}

#' @export
control_pro_rata <- function(max_iter = Inf,
                             tolerance = 1e-3,
                             verbose = FALSE) {
  structure(list(max_iter = max_iter,
                 tolerance = tolerance,
                 verbose = verbose),
            class = "control_pro_rata")
}

#' @export
predict.pro_rata <- function(object, new_data, ...) {
  out <- sweep(new_data, 3, object[["weights"]], `*`)
  apply(out, 1:2, sum)
}

dens_pro_rata <- function(y, x) {
  sweep(x, 1:2, y, \(x, y) (x / sum(x)) ^ y)
}
