set.seed(1234)

test_that("`prorata()` works", {
  I <- 20
  J <- 5
  K <- 3

  y <- as.table(matrix(runif(I * J), I, J))
  x <- as.table(array(runif(I * J * K),
                      dim = c(I, J, K)))

  # 1
  x_1 <- x
  x_1[, , 1] <- y

  prorata_1 <- prorata(y, x_1)
  expect_equal(sum(prorata_1$weights), 1)
  expect_equal(unname(prorata_1$weights), c(1, 0, 0),
               tolerance = 1e-3)

  prorata_1 <- prorata(y, x_1,
                       type = "absolute")
  expect_equal(sum(prorata_1$weights), 1)
  expect_equal(unname(prorata_1$weights), c(1, 0, 0),
               tolerance = 1e-3)

  # 3
  x_3 <- x
  x_3[, , 3] <- y

  prorata_3 <- prorata(y, x_3)
  expect_equal(sum(prorata_3$weights), 1)
  expect_equal(unname(prorata_3$weights), c(0, 0, 1),
               tolerance = 1e-3)

  prorata_3 <- prorata(y, x_3,
                       type = "absolute")
  expect_equal(sum(prorata_3$weights), 1)
  expect_equal(unname(prorata_3$weights), c(0, 0, 1),
               tolerance = 1e-3)
})
