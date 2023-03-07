rm(list = ls())

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)

# setwd("D:/Progra/Clase5") #Paco
# setwd("C:/Users/Andy/Desktop/tarea_inpc")#Portos 
# setwd("C:/Users/grzlz/Code/icarus/tarea_inpc")


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
  for (prediccion in predicciones) {
    mth <- max(dinamic_df$fecha) %m+% months(1)
    dat <- data.frame(fecha = mth, indice = prediccion)
    dinamic_df <- rbind(dinamic_df, dat)

  }
  
  dinamic_df %>%
    ggplot(aes(x = fecha, y = round(indice, 2))) + geom_point() + geom_line()
}

ui <- fluidPage(titlePanel("INPC"),
                sidebarLayout(sidebarPanel(
                  sliderInput(inputId = "meses", label = "meses (maximo 12)", min = 1, max = 12,value = 6
                  )
                ),
                mainPanel(plotOutput(outputId = "forecast_plot"))))


server <- function(input, output) {
  output$forecast_plot <- renderPlot({
    funcion_predecir(input$meses)
  })
}

shinyApp(ui, server)
