rm(list = ls())

library(shiny)
library(dplyr)
library(lubridate)
library(stringr)
library(zoo)
library(ggplot2)

# setwd("D:/Progra/Clase5") #Paco
# setwd("C:/Users/Andy/Desktop/tarea_inpc")#Portos 
setwd("C:/Users/grzlz/Code/icarus/tarea_inpc")


inpc <- read.csv("inpc.csv", header = T, sep = ",", skip = 4) %>% 
  na.omit()

inpc2 <- inpc %>% 
  select("indice" = names(inpc)[2])

inpc_ts <- ts(inpc2, start = c(1970, 1), frequency = 12)



inpc.arima <- arima(inpc_ts, order = c(2,2,2))
inpc.forecast <- predict(inpc.arima, 6)
prediction <- inpc.forecast$pred


funcion_andres <- function(meses){
  if(meses >= 6){
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
  else{
    ggplot(mtcars, aes(cyl, hp, group = 1, label = x))+
      geom_point()+
      geom_line()+
      geom_text(size = 3, aes(vjust = 2, hjust = 0))
  }
  }


ui <- fluidPage(
  sliderInput(inputId = "meses", label = "meses (maximo 12)", 
              min = 1, max = 12, value =6),
  plotOutput(outputId="forecast_plot")
  
)




server <- function(input, output){
    output$forecast_plot <- renderPlot({funcion_andres(input$meses)}
  )
}

shinyApp(ui, server)
