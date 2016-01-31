#' Shiny interface
#'
#' This function uses shiny to explore the data.
#'
#' @param includecode Logical. If \code{TRUE} the code
#' included in the ui.R and server.R source files will
#' be shown alongside the app.
#' @export
shine = function(includecode = FALSE){
  if(!requireNamespace("shiny", quietly = TRUE)) 
    stop("Install shiny package to use shine.")
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Install ggplot2 package to use shine.")
  
  
  appDir = system.file("shinyExamples", "plotting", package = "benchmarkmeData")
  if(nchar(appDir) == 0) {
    stop("Could not find example directory. Try reinstalling `benchmarkmeData`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = ifelse(includecode, "showcase", "normal"))
}