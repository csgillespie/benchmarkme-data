#' Interactive table of results
#'
#' A summary of past results
#' @inheritParams plot_past
#' @export
#' @examples
#' ## Need the DT package
#' ## View all results for prog test
#' get_datatable_past("prog")
get_datatable_past = function(test_group, 
                              blas_optimize = NULL,
                              cores = 0) {

  if(!requireNamespace("DT", quietly = TRUE))
    stop("Install DT package to use datatable", call. = FALSE)

  if(missing(test_group) || !(test_group %in% get_benchmarks())) {
    stop("test_group should be one of\n\t",  get_benchmarks(collapse = TRUE),
         call. = FALSE)
  }
  
  results = select_results(test_group, blas_optimize = blas_optimize, 
                           cores = cores)
  results$time = signif(results$time, 4)

  # ## Format
  # results = results[,c("rank", "time", "cpu", "byte_optimize", "blas_optimize", "sysname", "test_group")]
  # colnames(results) = c("Rank", "Time (sec)", "CPU", "Byte Compile", "BLAS Opt", "OS", "Test")
  # results = results[,c(TRUE, TRUE, TRUE, is.null(byte_optimize), is.null(blas_optimize),
  #                      TRUE, length(unique(results$Test)) > 1)]
  DT::datatable(results, rownames = FALSE)
}
