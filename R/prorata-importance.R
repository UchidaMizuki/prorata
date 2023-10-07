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
