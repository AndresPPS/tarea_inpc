install.packages("shiny")



library(shiny)
library(dplyr)

ui <- fluidPage(
  sliderInput(inputId = "meses", label = "meses (máximo 6)", 
              min = 1, max = 6, value =6),
  
)

server <- function(input, output){
  output$meses <- renderText(
    input$meses
  )
  
  output$plot_ejemplo <- renderPlot(
    plot(d$fecha,d$inpc)
  )
  
}

shinyApp(ui, server)
