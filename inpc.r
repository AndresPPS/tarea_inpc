rm(list = ls())

setwd("D:/Progra/Clase5")

library(shiny)
library(dplyr)
library(lubridate)
library(stringr)
library(zoo)
library(ggplot2)

inpc <- read.csv("inpc.csv", header = T, sep = ",", skip = 4)
inpc <- na.omit(inpc)

inpc2 <- inpc %>% select(2)
colnames(inpc2) <- "indice"


inpc_ts <- as.ts(inpc2, start = c(1970, 1), frequency = 12)

inpc.arima <- arima(inpc_ts, order = c(2,2,2))
inpc.forecast <- predict(inpc.arima, 6)
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


































funcion_andres <- function(){
  fechas <- as.data.frame(c("2023/02","2023/03", "2023/04", "2023/05", "2023/06", "2023/07"))
  colnames(fechas) = "fecha"
  
  prediccion <- as.data.frame(prediction)
  
  tabla_final <- merge(fechas,prediccion, by.x=0, by.y = 0)
  tabla_final$Row.names <- NULL
  
  tabla_final$x <- round(tabla_final$x, digits = 2)
  
  ggplot(tabla_final, aes(fecha, x, group = 1, label = x))+
    geom_point()+
    geom_line()+
    geom_text(size = 3, aes(vjust = 2, hjust = 0))
}

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
