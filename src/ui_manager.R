#

library(R6)
library(shiny)

UIManager <- R6Class(
  "UIManager",
  public = list(
    get_ui = function() {
      fluidPage(
        tags$style(HTML("
          body, label, .well, .shiny-input-container {
              background-color: #080808;
              color: #D0CDC9;
          }
          .form-control {
              background-color: #080808 !important;
              color: #D0CDC9 !important;
              border-color: #D0CDC9 !important;
          }
          .selectize-input, .selectize-dropdown {
              background-color: #080808 !important;
              color: #D0CDC9 !important;
          }
        ")),
        titlePanel("Visualización del Tipo de Cambio"),
        sidebarLayout(
          sidebarPanel(
            selectInput("institucion", "Seleccione la Institución Financiera:",
                        choices = list(
                          "BCCR" = "BCCR",
                          "BN" = "BN",
                          "BCR" = "BCR",
                          "BAC" = "BAC"
                        )
            ),
            selectInput("tipo", "Seleccione Tipo de Cambio:",
                        choices = list(
                          "Compra" = "compra",
                          "Venta" = "venta"
                        )
            ),
            dateInput("fecha_inicio", "Fecha de Inicio", 
                      value = Sys.Date() - 30, format = "dd/mm/yyyy"),
            dateInput("fecha_final", "Fecha Final",
                      value = Sys.Date(), format = "dd/mm/yyyy"),
            actionButton("descargar", "Obtener Datos", class = "btn-primary")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Tabla", tableOutput("tabla_datos")),
              tabPanel("Gráfico", plotlyOutput("plot_datos"))
            )
          )
        )
      )
    }
  )
)
