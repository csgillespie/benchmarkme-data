test_that("Test plot_past", {
  skip_on_cran()
  expect_true(is.data.frame(plot_past("prog")))
  expect_true(is.data.frame(plot_past(test_group=c("matrix_fun"))))
  expect_true(is.data.frame(plot_past("prog", byte_optimize = FALSE)))
  expect_true(is.data.frame(plot_past("prog", byte_optimize = TRUE)))
}
)
