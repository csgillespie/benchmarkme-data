library("shiny")
ui_env = new.env()
data(past_results, package="benchmarkmeData", envir=ui_env)

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
           wellPanel(selectInput("test", "Benchmark test", unique(ui_env$past_results$test_group))
           ))
   )
)