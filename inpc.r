rm(list = ls())

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)

# setwd("D:/Progra/Clase5") #Paco
# setwd("C:/Users/Andy/Desktop/tarea_inpc")#Portos 
setwd("C:/Users/grzlz/Code/icarus/tarea_inpc")


inpc <- read.csv("inpc.csv", header = T, sep = ",", skip = 4) %>% 
  na.omit() 
inpc <- inpc %>% 
  select("fecha" = names(inpc)[1], "indice" = names(inpc)[2])

inpc_ts <- ts(inpc %>% select(indice), start = c(1970, 1), frequency = 12)

inpc_arima <- arima(inpc_ts, order = c(2,2,2))

inpc_last_year <- inpc %>% 
  filter(fecha >= "2022/01") %>% 
  mutate(fecha = ym(fecha))

funcion_predecir <- function(meses) {

  inpc_forecast <- predict(inpc_arima, meses)
  predicciones <- inpc_forecast$pred
  dinamic_df <- inpc_last_year
  for(prediccion in predicciones) {
    
    # Aqui mismo hacer tratamiento a las fechas 
    i <- 1
    mth <- max(dinamic_df$fecha) %m+% months(1)
    dat <- data.frame(fecha = mth, indice = prediccion)
    dinamic_df <- rbind(dinamic_df, dat)

    i <- i + 1
  }
  
  return(dinamic_df)
}

funcion_predecir(5)

predicciones_vector <- c()

prd <- function() {
  return("Funciona por favor")
}



for(prediccion in predicciones) {
  predicciones_vector <- append(predicciones_vector, prd())
}

pd


inpc_forecast <- predict(inpc_arima, 6)
prediction <- inpc_forecast$pred




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
