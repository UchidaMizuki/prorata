#' @export
prorata_frequency <- function(y, x,
                              control = control_prorata()) {
  check_y_x_prorata(y, x)
  x <- x_prorata(x)

  # Multinomial Density
  dm_x <- dim(x)
  dm_x_1 <- dm_x[[1]]
  dm_x_3 <- dm_x[[3]]

  dens <- matrix(NA_real_, dm_x_1, dm_x_3)

  for (i in seq_len(dm_x_1)) {
    y_i <- y[i, ]

    for (k in seq_len(dm_x_3)) {
      x_ik <- x[i, , k]

      dens[i, k] <- stats::dmultinom(y_i,
                                     prob = x_ik)
    }
  }

  prorata_impl(dens = dens,
               control = control)
}

#' @export
prorata_importance <- function(y, x,
                               control = control_prorata()) {
  check_y_x_prorata(y, x)
  x <- x_prorata(x)

  # Multivariate Normal Density
  dm_x <- dim(x)
  dm_x_1 <- dm_x[[1]]
  dm_x_3 <- dm_x[[3]]

  dens <- matrix(NA_real_, dm_x_1, dm_x_3)

  for (i in seq_len(dm_x_1)) {
    y_i <- y[i, ]
    y_i_total <- sum(y_i)

    for (k in seq_len(dm_x_3)) {
      x_ik <- x[i, , k]

      # https://www.stat.umn.edu/geyer/5102/notes/brand.pdf
      dens[i, k] <- mvtnorm::dmvnorm(y_i / y_i_total,
                                     mean = x_ik,
                                     sigma = (diag(x_ik) - outer(x_ik, x_ik))) / y_i_total
    }
  }

  prorata_impl(dens = dens,
               control = control)
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

prorata_impl <- function(dens, control) {
  dens <- pmax(dens, .Machine$double.xmin)
  dens <- pmin(dens, .Machine$double.xmax)

  weights <- 1 / dim(dens)[[2]]
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
  new_data

  out <- new_data[, , 1]
  out[] <- apply(sweep(new_data, 3, object[["weights"]], `*`),
                 1:2, sum)
  out
}

y_prorata <- function(y) {
  dm <- dim(y)
  len_dm <- length(dm)
  if (len_dm == 0) {
    y <- matrix(y,
                nrow = 1)
  } else if (len_dm == 1) {
    dim(y) <- c(1, dm)
  }
  y
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
