#' Interactive table of results
#' 
#' Compare your results against past results. Your results is 
#' shown in Orange.
#' @inheritParams plot_past
#' 
#' @export
get_datatable_past = function(byte_optimize=NULL) {
  if(!requireNamespace("DT", quietly = TRUE))
    stop("Install DT package to use datatable")
  
  
  ## Load past data
  tmp_env = environment()
  data(past_results, package="benchmarkmeData", envir = tmp_env)
  results = tmp_env$past_results
  results$cpus = as.character(results$cpus)
  
  if(!is.null(byte_optimize)){
    if(byte_optimize) {
      results = results[results$byte_optimize > 0.5,]
    } else {
      results = results[results$byte_optimize < 0.5,]
    }
  }
  
  results$timings = signif(results$timings, 5)
  results = results[order(results$timings), ]
  results$rank = 1:nrow(results)
  
  if(is.null(byte_optimize)){
    results = results[,c("rank", "cpus", "timings", "byte_optimize")]
    colnames(results) = c("Rank", "CPU", "Time (sec)", "Byte Compile")
  } else {
    results = results[,c("rank", "cpus", "timings")]
    colnames(results) = c("Rank", "CPU", "Time (sec)")
  }
  DT::datatable(results, rownames=FALSE) 
}