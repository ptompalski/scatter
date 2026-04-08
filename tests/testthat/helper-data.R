make_scatter_data <- function() {
  set.seed(42)
  tibble::tibble(
    truth = c(10, 11, 12, 13, 20, 21, 22, 23, 30, 31, 32, 60),
    estimate = c(11, 10, 13, 12, 19, 22, 21, 24, 29, 32, 30, 58),
    group = rep(c("A", "B", "C"), each = 4),
    group2 = rep(rep(c("X", "Y"), each = 2), times = 3)
  )
}
