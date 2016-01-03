#' Plot past results
#' 
#' Plot the previous benchmarks. This function creates two figures.
#' \itemize{
#' \item Figure 1: Total benchmark time over all benchmarks (in seconds) on the y-axis..
#' \item Figure 2: Relative time (compared to the smallest benchmark).
#' }
#' The data set used is \code{data(past_results)}.
#' @param byte_optimize Default \code{NULL}. The default behaviour is to plot all results.
#' To plot only the byte optimized results, set to \code{TRUE}, otherwise \code{FALSE}.
#' @param log By default the y axis is plotted on the log scale. To change, set the 
#' the argument equal to the empty parameter string, \code{""}.
#' @examples 
#' ## Plot non byte optimize code
#' plot_past(byte_optimize=FALSE)
#' @export
plot_past = function(byte_optimize = NULL, log="y") {
  ## Load past data
  tmp_env = new.env()
  data(past_results, package="benchmarkmeData", envir = tmp_env)
  pas_res = tmp_env$past_results
  pas_res = pas_res[order(pas_res$timings), ]
  if(!is.null(byte_optimize) && byte_optimize)
    pas_res = pas_res[pas_res$byte_optimize > 0.5,]
  else if(!is.null(byte_optimize) && !byte_optimize)
    pas_res = pas_res[pas_res$byte_optimize < 0.5,]
  
  ## Sort plot
  op = par(mar=c(3,3,2,1), 
           mgp=c(2,0.4,0), tck=-.01,
           cex.axis=0.8, las=1, mfrow=c(1,2)) 
  on.exit(op)
  ymax = max(pas_res$timings)
  plot(pas_res$timings, xlab="Rank", ylab="Total timing (secs)", 
       ylim=c(1, ymax), xlim=c(1, nrow(pas_res)+1), 
       panel.first=grid(), log=log)
  
  ## Relative timings  
  fastest = min(pas_res$timings)
  ymax= ymax/fastest
  plot(pas_res$timings/fastest, xlab="Rank", ylab="Relative timing", 
       ylim=c(1, ymax), xlim=c(1, nrow(pas_res)+1), 
       panel.first=grid(), log=log)
  abline(h=1, lty=3)
}
