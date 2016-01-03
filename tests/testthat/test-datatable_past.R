test_that("Test datatable_past", {
  skip_on_cran()
  expect_true(is.list(get_datatable_past()))
  expect_true(is.list(get_datatable_past(TRUE)))
  expect_true(is.list(get_datatable_past(FALSE)))
  }
)
