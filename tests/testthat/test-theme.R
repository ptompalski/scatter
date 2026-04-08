test_that("theme_baseR returns a ggplot theme object", {
  th <- theme_baseR(font_size = 14, font_family = "serif")

  expect_s3_class(th, "theme")
  expect_equal(th$axis.title$size, 14)
  expect_equal(th$axis.title$family, "serif")
})
