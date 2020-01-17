rm(list = ls())
library(shiny)
library(shinydashboard)
library(DT)
ui <- dashboardPage(
  dashboardHeader(title = "My Chart"),
  dashboardSidebar(
    width = 0
  ),
  dashboardBody(
    box(title = "Data Path", status = "primary",height = "595" ,solidHeader = T,
        plotOutput("trace_plot")),
    box( title = "Case Analyses Details", status = "primary", height = 
           "595",width = "6",solidHeader = T, 
         column(width = 12,
                DT::dataTableOutput("trace_table"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
         )
    )))
server <- function(input, output) { 
  #Plot for Trace Explorer
  output$trace_plot <- renderPlot({
    plot(iris$Sepal.Length,iris$Sepal.Width)
  })
  output$trace_table <- renderDataTable({
    
    datatable(cbind(mtcars,mtcars), options = list(paging = FALSE))
    
  })
}
shinyApp(ui, server)