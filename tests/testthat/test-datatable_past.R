test_that("Test datatable_past", {
  skip_on_cran()
  data_table = get_datatable_past()
  expect_true(is.list(data_table))
}
)
