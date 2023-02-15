rm(list = ls())

setwd("D:/Progra/Clase5")

library(shiny)
library(dplyr)
library(lubridate)
library(stringr)
library(zoo)


inpc <- read.csv("C:/Users/grzlz/Code/icarus/tarea_inpc/inpc.csv", header = T, sep = ",", skip = 4)
#inpc <- inpc %>% select(2)
inpc <- na.omit(inpc)
fechas <- inpc %>% select(1)
indice <- inpc %>% select(2)


inpc <- inpc %>% select(2)
colnames(inpc) <- "indice"

inpc_ts <- as.ts(inpc, start = c(1970, 1), frequency = 12)

inpc.arima <- arima(inpc_ts, order = c(2,2,2))
# Correr modelo para predecir n meses 
inpc.forecast <- predict(inpc.arima, 5)

prediction <- inpc.forecast$pred
# Recuperar valor para mes n
prediction[2]


fechas <- data.frame(fechas = c("12/2022", "1/2023"))
indice <- data.frame(indice = c(7.1, 7.3))


funcion_portos <- function() {
  
  # Crear fechas
  fechas_predecidas <- data.frame(fechas = c("02/2023", "03/2023"))
  
  # Guardar predicciones
  predicciones <- data.frame(indice = c(prediction[1], prediction[2]))
  
  # Unir predicciones con indice
  indice_predicciones <- rbind(indice, predicciones)
  
  # Unir meses de prediccion con fechas
  fechas <- rbind(fechas, fechas_predecidas)
  
  # Unir predicciones con fechas
  df <- data.frame(indices = indice_predicciones, fechas = fechas)
  
  
  return(df)
  
}

data <- funcion_portos()



View(data)


































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
