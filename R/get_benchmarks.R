get_benchmarks = function(collapse = FALSE) {
  benchmarks  = c("prog", "matrix_fun", "matrix_cal",
                  "read5", "read50", "read200",
                  "write5", "write50", "write200")
  if (collapse) {
    benchmarks = paste(benchmarks, collapse = ", ")
  }
  benchmarks
}
