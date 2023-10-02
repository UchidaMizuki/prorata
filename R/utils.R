near_all <- function(x) {
  stats::var(x) < .Machine$double.eps ^ 0.5
}
