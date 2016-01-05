#' Plot past results
#' 
#' Plot the previous benchmarks. This function creates two figures.
#' \itemize{
#' \item Figure 1: Total benchmark time over all benchmarks (in seconds) on the y-axis.
#' \item Figure 2: Relative time (compared to the smallest benchmark).
#' }
#' The data set used is \code{data(past_results)}.
#' @param test_group Default \code{NULL}. The default behaviour is to average over all tests. 
#' @param byte_optimize Default \code{NULL}. The default behaviour is to plot all results.
#' To plot only the byte optimized results, set to \code{TRUE}, otherwise \code{FALSE}.
#' @param log By default the y axis is plotted on the log scale. To change, set the 
#' the argument equal to the empty parameter string, \code{""}.
#' @examples 
#' ## Plot non byte optimize code
#' plot_past(byte_optimize=FALSE)
#' @importFrom graphics abline grid par plot points
#' @importFrom utils data
#' @export
plot_past = function(test_group=NULL, byte_optimize=NULL, log="y") {
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
  
  if(is.null(test_group)) test_group = unique(results$test_group)
  results = results[results$test_group %in% test_group,]
  
  ## Aggregate over test
  ## Ensure that we have timings for all required tests.
  results = aggregate(time ~ id + byte_optimize + cpu + date + sysname, 
                      data=results, 
                      FUN=function(i) ifelse(length(i) == length(test_group), sum(i), NA))
  results = results[!is.na(results$time), ]
  results = results[order(results$time), ]
  ## Sort plot
  op = par(mar=c(3,3,2,1), 
           mgp=c(2,0.4,0), tck=-.01,
           cex.axis=0.8, las=1, mfrow=c(1,2)) 
  on.exit(op)
  ymin = min(results$time)
  ymax = max(results$time)
  plot(results$time, xlab="Rank", ylab="Total timing (secs)", 
       ylim=c(ymin, ymax), xlim=c(1, nrow(results)+1), 
       panel.first=grid(), log=log)
  
  ## Relative timings  
  fastest = min(results$time)
  ymax = ymax/fastest
  plot(results$time/fastest, xlab="Rank", ylab="Relative timing", 
       ylim=c(1, ymax), xlim=c(1, nrow(results)+1), 
       panel.first=grid(), log=log)
  abline(h=1, lty=3)
  
  invisible(results)
}
