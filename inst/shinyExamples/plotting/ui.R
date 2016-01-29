library("shiny")
opts = c("None", "Byte", "R Version", "OS", "BLAS")
ui = fluidPage(
  titlePanel("Benchmark data"),
  br(),
  plotOutput("plot"),
  hr(),
  fluidRow(
    column(3, 
           wellPanel(selectInput("facet_x", "Facet x", opts)
           )),
    column(3, 
           wellPanel(selectInput("facet_y", "Facet y", opts)
           )),
    column(3, 
           wellPanel(selectInput("test", "Benchmark test", unique(past_results$test_group))
           ))
   )
)