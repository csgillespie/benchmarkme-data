#' Interactive table of results
#' 
#' A summary of past results
#' @inheritParams plot_past
#' @export
#' @examples 
#' ## Need the DT package
#' ## View all results for prog test
#' get_datatable_past("prog")
#' ## View matrix_fun test
#' get_datatable_past("matrix_fun")
#' ## View matrix_fun test - only BLAS results
#' get_datatable_past("matrix_fun", blas_optimize=TRUE)
get_datatable_past = function(test_group, 
                              byte_optimize=NULL, blas_optimize = NULL) {
  .Deprecated(msg = "This function is deprecated and will be removed in the next version. 
              Instead, use https://jumpingrivers.shinyapps.io/benchmarkme/")
  if(!requireNamespace("DT", quietly = TRUE))
    stop("Install DT package to use datatable")
  
  if(missing(test_group) || !(test_group %in% get_benchmarks())) {
    stop("test_group should be one of\n\t", 
         get_benchmarks(collapse = TRUE),
         call. = FALSE)
  }

  results = select_results(test_group, byte_optimize = byte_optimize, 
                           blas_optimize = blas_optimize)
  results$time = signif(results$time, 4)
  
  ## Format
  results = results[,c("rank", "time", "cpu", "byte_optimize", "blas_optimize", "sysname", "test_group")]
  colnames(results) = c("Rank", "Time (sec)", "CPU", "Byte Compile", "BLAS Opt", "OS", "Test")
  results = results[,c(TRUE, TRUE, TRUE, is.null(byte_optimize), is.null(blas_optimize), 
                       TRUE, length(unique(results$Test)) > 1)]
  DT::datatable(results, rownames=FALSE) 
}
