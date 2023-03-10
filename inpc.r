rm(list = ls())



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


inpc_ts <- ts(inpc2, start = c(1970, 1), frequency = 12)

inpc_df <- as.data.frame(inpc_ts)



#indice <- inpc_df$indice
#fechas <- inpc_df$fecha


inpc.arima <- arima(inpc_ts, order = c(2,2,2))
inpc.forecast <- predict(inpc.arima, 12)
prediction <- inpc.forecast$pred



funcion_meses <- function(meses) {
  df_meses <- data.frame(row.names = "fecha")
  acumulador <- months(1)
  for (m in 1:meses) {
    df_auxiliar <- data.frame(row.names = "fecha")
    df_auxiliar$fecha <- ymd(20230101) + acumulador
    df_meses <- rbind(df_meses, df_auxiliar)
    acumulador <- acumulador + months(1)
  }
  return(df_meses)
}

funcion_predicciones <- function(meses){
  prediccion <- predict(inpc.arima, meses)
  prediccion <- as.data.frame(prediccion$pred)
  
  return(prediccion)
  
}

funcion_final <- function(meses){
  df_meses <- funcion_meses(meses)
  df_prediccion <- funcion_predicciones(meses)
  df_final <- cbind(df_meses, df_prediccion)
  return(df_final)
}

funcion_plot <- function(meses){
    df_final <- funcion_final(meses)
    df_final$x <- round(df_final$x, 2) 
    ggplot(df_final, aes(fecha, x, group = 1, label = x))+
    geom_point()+
    geom_line()+
    geom_text(size = 3, aes(vjust = 2, hjust = 0))
}

funcion_fecha <- function(meses){
  
}  


ui <- fluidPage(
  sliderInput(inputId = "meses", label = "meses (maximo 12)", 
              min = 1, max = 12, value =6),
  plotOutput(outputId="forecast_plot")
  
)




server <- function(input, output){
    output$forecast_plot <- renderPlot({
      funcion_plot(input$meses)
      })
}

shinyApp(ui, server)
