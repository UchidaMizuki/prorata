set.seed(1234)

test_that("`pro_rata()` works", {
  l <- 20
  m <- 5
  n <- 3

  y <- as.table(matrix(runif(l * m), l, m))
  x <- as.table(array(runif(l * m * n),
                      dim = c(l, m, n)))

  x_1 <- x
  x_1[, , 1] <- y
  prorata_1 <- pro_rata(y, x_1,
                        control = control_pro_rata(tolerance = 1e-6))
  expect_equal(sum(prorata_1$weights), 1)
  expect_equal(unname(prorata_1$weights), c(1, 0, 0),
               tolerance = 1e-3)

  x_2 <- x
  x_2[, , 2] <- y
  prorata_2 <- pro_rata(y, x_2,
                        control = control_pro_rata(tolerance = 1e-6))
  expect_equal(sum(prorata_2$weights), 1)
  expect_equal(unname(prorata_2$weights), c(0, 1, 0),
               tolerance = 1e-3)

  x_3 <- x
  x_3[, , 3] <- y
  prorata_3 <- pro_rata(y, x_3,
                        control = control_pro_rata(tolerance = 1e-6))
  expect_equal(sum(prorata_3$weights), 1)
  expect_equal(unname(prorata_3$weights), c(0, 0, 1),
               tolerance = 1e-3)
})
