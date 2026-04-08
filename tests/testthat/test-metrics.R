test_that("agreement_metrics returns default metrics and labels", {
  df <- tibble::tibble(
    truth = c(3, 5, 2.5, 7),
    estimate = c(2.8, 5.1, 2.6, 7.2)
  )

  out <- agreement_metrics(df, truth, estimate)
  expect_true(all(c("rsq", "md", "rmd", "rmse", "rrmse") %in% names(out)))

  labeled <- agreement_metrics(
    df,
    truth,
    estimate,
    metrics = list("Bias" = md, yardstick::rmse),
    label = TRUE
  )

  expect_true(all(c("Bias", "rmse", "label") %in% names(labeled)))
  expect_match(labeled$label, "Bias: ")
  expect_match(labeled$label, "rmse: ")
})

test_that("md, rmd, and rrmse helpers handle core branches", {
  df <- tibble::tibble(
    truth = c(10, 20, NA, 40),
    estimate = c(12, 19, 30, 35)
  )

  expect_equal(md_vec(c(1, 2), c(2, 4)), 1.5)
  expect_equal(round(rmd_vec(c(10, 20), c(12, 18)), 2), 0)
  expect_true(is.na(rmd_vec(c(10, NA), c(12, 18), na_rm = FALSE)))

  expect_equal(
    round(rrmse_vec(c(10, 20), c(12, 18), normalization = "mean"), 2),
    round(sqrt(mean(c(4, 4))) / mean(c(10, 20)) * 100, 2)
  )
  expect_equal(
    round(rrmse_vec(c(10, 20), c(12, 18), normalization = "range"), 2),
    round(sqrt(mean(c(4, 4))) / diff(range(c(10, 20))) * 100, 2)
  )
  expect_true(is.na(rrmse_vec(c(5, 5), c(4, 6), normalization = "range")))
  expect_true(is.na(rrmse_vec(c(10, NA), c(12, 18), na_rm = FALSE)))
})
