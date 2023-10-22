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
  x_1[, , 1] <- y + matrix(runif(I * J, max = 1e-4), I, J)

  prorata_1 <- prorata(y, x_1)
  expect_equal(sum(prorata_1), 1)
  expect_equal(as.double(prorata_1), c(1, 0, 0),
               tolerance = 1e-2)

  prorata_1 <- prorata(y, x_1,
                       type = "absolute")
  expect_equal(sum(prorata_1), 1)
  expect_equal(as.double(prorata_1), c(1, 0, 0),
               tolerance = 1e-2)

  # 3
  x_3 <- x
  x_3[, , 3] <- y + matrix(runif(I * J, max = 1e-4), I, J)

  prorata_3 <- prorata(y, x_3)
  expect_equal(sum(prorata_3), 1)
  expect_equal(as.double(prorata_3), c(0, 0, 1),
               tolerance = 1e-2)

  prorata_3 <- prorata(y, x_3,
                       type = "absolute")
  expect_equal(sum(prorata_3), 1)
  expect_equal(as.double(prorata_3), c(0, 0, 1),
               tolerance = 1e-2)
})
