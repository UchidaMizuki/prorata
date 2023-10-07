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
