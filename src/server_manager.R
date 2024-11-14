# Script para manejar la lógica del server de la Shiny App.

library(R6)
library(shiny)
library(ggplot2)
library(plotly)
source("exchange_rate.R")

ServerManager <- R6Class(
  "ServerManager",
  public = list(
    get_server = function() {
      function(input, output) {
        datos_reactivos <- reactiveVal()
        
        observeEvent(input$descargar, {
          fecha_inicio <- format(as.Date(input$fecha_inicio, format = "%Y-%m-%d"), "%d/%m/%Y")
          fecha_final <- format(as.Date(input$fecha_final, format = "%Y-%m-%d"), "%d/%m/%Y")
          
          descargador <- tipo_cambio$new(
            indicador = 317,
            fecha_inicio = fecha_inicio,
            fecha_final = fecha_final,
            nombre = " nombre ",
            sub_niveles = "N",
            correo_electronico = " email ",
            token = " token "
          )
          
          datos <- descargador$descargar_datos()
          
          # transformaciones 
          datos <- datos %>%
            dplyr::mutate(
              DES_FECHA = format(as.Date(DES_FECHA, origin = "1900-01-01"), "%d/%m/%Y")
            ) %>%
            dplyr::select(-COD_INDICADORINTERNO)
          
          datos_reactivos(datos)
        })
        
        output$tabla_datos <- renderTable({
          datos_reactivos()
        })
        
        output$plot_datos <- renderPlotly({
          datos <- datos_reactivos()
          if (is.null(datos)) return(NULL)
          
          # gráfico
          plot_ly(
            data = datos,
            x = ~as.Date(DES_FECHA, format = "%d/%m/%Y"),
            y = ~NUM_VALOR,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = 'blue'),
            marker = list(size = 8)
          ) %>%
            layout(
              title = "Evolución del Tipo de Cambio",
              xaxis = list(title = "Fecha"),
              yaxis = list(title = "Valor Numérico"),
              plot_bgcolor = "#080808",
              paper_bgcolor = "#080808",
              font = list(color = "#D0CDC9")
            )
        })
      }
    }
  )
)
