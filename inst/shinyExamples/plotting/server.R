library(benchmarkmeData)
library(ggplot2)

get_data = function(past_results, test) {
  past_results = past_results[past_results$test_group %in% test,]
  past_results = past_results[order(past_results$time),]
  past_results$rank = 1:nrow(past_results)
  past_results  
}

get_plot = function(past_results, 
                    facet_x="None", facet_y = "None", 
                    test="prog") {
  past_results = get_data(past_results, test)
  g = ggplot(past_results) + 
    geom_point(aes(rank, time)) + 
    scale_y_log10() + 
    theme_bw() + 
    xlab("Rank") + 
    ylab("Time (secs)")
  g+ get_facet(facet_x, facet_y)
  
}

lookup = function(facet_name) {
  to = c(".", "byte", "r_minor", "sysname", "blas")
  from = c("None", "Byte", "R Version", "OS", "BLAS")
  to[which(facet_name == from)]
}

get_facet = function(facet_x, facet_y) {
  if(facet_x == "None" && facet_y == "None") return(NULL)
  if(facet_x == facet_y) facet_y = "None"
  
  x = paste(lookup(facet_y), lookup(facet_x), sep= " ~ ")
  facet_grid(x)
}

server = function(input, output){
  data(past_results, package="benchmarkmeData")
  past_results$byte = ifelse(past_results$byte_optimize > 0.5, "Byte", "Not Byte")
  past_results$blas = ifelse(past_results$blas_optimize, "BLAS Optimised", "Standard")
  res = past_results
  
  rv = reactiveValues(plot = get_plot(res), 
                      facet_x = "None", facet_y = "None")
  
  observeEvent(input$facet_x, 
               {rv$plot <- get_plot(res, input$facet_x, input$facet_y, input$test)})
  observeEvent(input$facet_y, 
               {rv$plot <- get_plot(res, input$facet_x, input$facet_y, input$test)})
  observeEvent(input$test, 
               {rv$plot <- get_plot(res, input$facet_x, input$facet_y, input$test)})
  
  output$plot = renderPlot(rv$plot)
}

