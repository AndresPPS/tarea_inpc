rm(list = ls())

setwd("D:/Progra/Clase5")

library(shiny)
library(dplyr)
library(lubridate)
library(stringr)
library(zoo)


inpc <- read.csv("inpc.csv", header = T, sep = ",", skip = 4)
#inpc <- inpc %>% select(2)
inpc <- na.omit(inpc)
inpc <- inpc %>% select(2)
colnames(inpc) <- "indice"
#inpc$fecha <- ym(inpc$fecha, "%Y/%m/%d")

#inpc$fecha <- str_replace(inpc$fecha, "/", "-")

#inpc$fecha <- as.yearmon(inpc$fecha)

inpc_ts <- as.ts(inpc, start = c(1970, 1), frequency = 12)

inpc_df <- as.data.frame(inpc_ts)



indice <- inpc_df$indice
fechas <- inpc_df$fecha


inpc.arima <- arima(inpc_ts, order = c(2,2,2))
inpc.forecast <- predict(inpc.arima, 1)
prediction <- inpc.forecast$pred
plot(inpc.forecast)
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
