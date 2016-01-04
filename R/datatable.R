#' Interactive table of results
#' 
#' A summary of past results
#' @inheritParams plot_past
#' @export
get_datatable_past = function(test=NULL, byte_optimize=NULL) {
  if(!requireNamespace("DT", quietly = TRUE))
    stop("Install DT package to use datatable")

  ## Load past data
  tmp_env = new.env()
  data(past_results, package="benchmarkmeData", envir = tmp_env)
  results = tmp_env$past_results
  
  if(!is.null(byte_optimize)){
    if(byte_optimize) {
      results = results[results$byte_optimize > 0.5,]
    } else {
      results = results[results$byte_optimize < 0.5,]
    }
  }
  
  if(is.null(test)) test = unique(results$test)
  results = results[results$test %in% test,]
  
  ## Aggregate over test
  ## Ensure that we have timings for all required tests.
  results = aggregate(time ~ id + byte_optimize + cpu + date + sysname, 
                      data=results, 
                      FUN=function(i) ifelse(length(i) == length(test), sum(i), NA))
  results = results[!is.na(results$time), ]
  
  results$time = signif(results$time, 4)
  results = results[order(results$time), ]
  results$rank = 1:nrow(results)
  colnames(results)
  if(is.null(byte_optimize)){
    results = results[,c("rank", "time", "cpu", "byte_optimize", "sysname")]
    colnames(results) = c("Rank", "Time (sec)", "CPU", "Byte Compile", "OS")
  } else {
    results = results[,c("rank", "time", "cpu", "sysname")]
    colnames(results) = c("Rank", "Time (sec)", "CPU", "OS")
  }
  DT::datatable(results, rownames=FALSE) 
}
