near_all <- function(x) {
  stats::var(x) < .Machine$double.eps ^ 0.5
}

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}
