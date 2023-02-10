rm(list = ls())

setwd("D:/Progra/Clase5")

library(shiny)
library(dplyr)
library(lubridate)

inpc <- read.csv("inpc.csv", header = T, sep = ",", skip = 4)
#inpc <- inpc %>% select(2)
inpc <- na.omit(inpc)
colnames(inpc) <- c("fecha", "indice")
inpc$fecha <- as.Date.character(inpc$fecha, "%Y/%m/%d")

inpc_ts <- as.ts(inpc)
inpc.arima <- arima(inpc, order = c(2,2,2))
plot(inpc.arima)

ui <- fluidPage(
  sliderInput(inputId = "meses", label = "meses (m?ximo 6)", 
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
