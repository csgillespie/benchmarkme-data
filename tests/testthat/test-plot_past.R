test_that("Test plot_past", {
  skip_on_cran()
  expect_true(is.data.frame(plot_past()))
  expect_true(is.data.frame(plot_past(test=c("prog", "matrix_fun"))))
  expect_true(is.data.frame(plot_past(byte_optimize = FALSE, blas_optimize = TRUE)))
  expect_true(is.data.frame(plot_past(byte_optimize = TRUE)))
  expect_error(plot_past("testing"))
}
)
