# Cargar bibliotecas
library(shiny)  
library(ggplot2)  
library(scales)  # Biblioteca para escalar ejes en gráficos
library(httr)  
library(jsonlite)  
library(dplyr)  
library(xml2)  
library(memoise)  # Biblioteca para cachear resultados de funciones
library(lubridate)  # Biblioteca para manipulación de fechas
library(plotly)  


# Paleta de colores personalizada
mi_paleta <- c("Cod gray" = "#080808", "Swirl" = "#D0CDC9")

# Funcion para obtener datos del BCCR
get_exchange_data <- function(indicador, fecha_inicio, fecha_final) {
  # Parámetros para la solicitud HTTP
  params <- list(
    Indicador = indicador,
    FechaInicio = fecha_inicio,
    FechaFinal = fecha_final,
    Nombre = "",
    SubNiveles = "N",
    CorreoElectronico = "",
    Token = ""
  )
  
  # URL del servicio web
  url <- "https://gee.bccr.fi.cr/Indicadores/Suscripciones/WS/wsindicadoreseconomicos.asmx/ObtenerIndicadoresEconomicosXML"
  
  tryCatch(
    {
      # Realizar la solicitud HTTP
      response <- POST(url, body = params, encode = "form")
      httr::stop_for_status(response)
      
      # Procesar la respuesta XML
      xml_content <- content(response, "text")
      xml_decoded <- gsub("&lt;", "<", xml_content)
      xml_decoded <- gsub("&gt;", ">", xml_decoded)
      xml_obj_decoded <- read_xml(xml_decoded)
      xml_obj_ns <- xml_ns_strip(xml_obj_decoded)
      records <- xml_find_all(xml_obj_ns, "//INGC011_CAT_INDICADORECONOMIC")
      
      # Crear un data frame con los resultados
      df <- data.frame(
        Fecha = xml_text(xml_find_all(records, "./DES_FECHA")),
        Valor = as.numeric(xml_text(xml_find_all(records, "./NUM_VALOR")))
      )
      
      return(df)
    },
    error = function(e) {
      print(paste("Error en la función 'get_exchange_data':", e$message))
      return(NULL)
    }
  )
}


# Versión de la función con caché
get_exchange_data_cached <- memoise(get_exchange_data)


# Función para obtener datos del Banco Mundial
get_wb_data <- function(codigo_pais, indicador, fecha_inicio, fecha_final) {
  # Construir la URL para la consulta
  url <- paste0("http://api.worldbank.org/v2/country/", codigo_pais,
                "/indicator/", indicador)
  query <- list(format = "json", date = paste0(fecha_inicio, ":", fecha_final))
  response <- GET(url, query = query)
  
  # Convertir la respuesta a un df
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  df <- as.data.frame(data[2])
  df <- df %>% 
    select("date", "value") %>% 
    rename("Fecha" = "date", "USD" = "value" )
  
  # Reformatear la columna USD para separar por comas
  df$USD <- sapply(strsplit(as.character(df$USD), split = ","), function(x) 
    as.numeric(x[1]))
  
  # Formatear los numeros en la columna USD 
  df$USD <- format(df$USD, big.mark = ",", scientific = FALSE)
  
  return(df)
}

#---------- Empieza shiny app ---------#

# Interfaz de usuario
ui <- fluidPage(
  tags$style(HTML("
    body, label, .well, .shiny-input-container {
        background-color: #080808; /* Cod gray */
        color: #D0CDC9; /* Swirl */
    }
    .form-control {
        background-color: #080808 !important; /* Cod gray */
        color: #D0CDC9 !important; /* Swirl */
        border-color: #D0CDC9 !important;
    }
    .selectize-input, .selectize-dropdown {
        background-color: #080808 !important;
        color: #D0CDC9 !important;
    }
    .shiny-input-slider {
        background: none !important;
    }
    .slider-track {
        background-color: #D0CDC9 !important;
    }
  ")),
  titlePanel("TicoFinance"),
  tabsetPanel(
    tabPanel("Calculadoras financieras", 
             sidebarLayout(
               sidebarPanel(
                 numericInput("capital", "Capital ($)", value = 1000, min = 0),
                 numericInput("Interes", "Interés", value = 6.5, min = 0,  
                              step = 0.1),
                 numericInput("tiempo", "Numero de años", value = 30, min = 0),
                 numericInput("periodos", "Períodos de capitalizacion", 
                              value = 12, 
                              min = 0),
                 numericInput("contribucion", "Contribución mensual ($)", 
                              value = 400,
                              min = 0),
                 textOutput("value")
               ),
               mainPanel(
                 textOutput("resultado"),
                 plotOutput("myPlot")
               )
             )
    ),
    tabPanel("Tipo de cambio",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput('dateRange', 'Elegir rango de fechas:', 
                                start = Sys.Date()-30, end = Sys.Date(), 
                                language = "es", # Translate los días a español
                                format = "dd/mm/yyyy", 
                                separator = " - "
                 ), 
                 selectInput("banco", "Seleccione un Banco:", 
                             choices = c("Tipo de cambio sugerido" = "bccr", 
                                         "Banco Nacional de Costa Rica" = "bn", 
                                         "Banco de Costa Rica" = "bcr"))
               ),
               mainPanel(
                 plotlyOutput("exchangePlot"),  
                 div(style = "max-height: 300px; overflow-y: auto;",
                     tableOutput("exchangeTable"),
                     tableOutput("bnTable")
                 )
               )
             )
    ), 
    tabPanel("Variables macroeconómicas",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Seleccionar Variable:",
                             choices = c("PIB nominal" = "NY.GDP.MKTP.CD",
                                         "PIB %" = "NY.GDP.MKTP.KD.ZG"),
                             selected = "PIB nominal")
               ),
               mainPanel(
                 plotlyOutput("pib_plot"),
                 tableOutput("pib_table"), 
                 tableOutput("pibTable_percent")
               )
             )
    )
  )    
)


# Definir el servidor 
server <- function(input, output) {
  # Función para calcular interés compuesto
  calcular_interes_compuesto <- function(c, i, t, p, c_m) {
    A = c * (1 + i/p)^(p*t) + c_m * (((1 + i/p)^(p*t) - 1) / (i/p))
    return(A)
  }
  
  # Resultado de la calculadora financiera
  output$resultado <- renderText({
    capital <- input$capital
    tasa <- input$Interes / 100
    tiempo <- input$tiempo
    periodos <- input$periodos
    contribucion <- input$contribucion
    monto_acumulado = calcular_interes_compuesto(capital, tasa, tiempo, periodos, contribucion)
    monto_acumulado_formateado <- format(monto_acumulado, big.mark = ",", scientific = FALSE)
    paste("Monto Acumulado: $", monto_acumulado_formateado)
  })
  
  # Gráfico de evolución del capital
  output$myPlot <- renderPlot({
    capital_inicial <- input$capital
    tasa_interes <- input$Interes / 100
    tiempo <- input$tiempo
    periodos <- input$periodos
    contribucion_mensual <- input$contribucion
    
    tiempo_seq <- seq(0, tiempo, length.out = 100)  
    capital_evolution <- sapply(tiempo_seq, function(t) {
      calcular_interes_compuesto(capital_inicial, tasa_interes, t, periodos, 
                                 contribucion_mensual)
    })
    
    data <- data.frame(tiempo = tiempo_seq, capital = capital_evolution)
    
    # Gr[afico]
    ggplot(data, aes(x = tiempo, y = capital)) +
      geom_line(color = mi_paleta["Swirl"]) +
      xlab("Tiempo (años)") +
      ylab("Capital ($)") +
      ggtitle("Evolución del Capital con Interés Compuesto") +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = mi_paleta["Cod gray"]),
        panel.background = element_rect(fill = mi_paleta["Cod gray"]),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = mi_paleta["Swirl"]),
        axis.text = element_text(color = mi_paleta["Swirl"]),
        plot.title = element_text(color = mi_paleta["Swirl"], hjust = 0.5)
      )
  })
  
  #---------- Tipo de cambio ----------#
  
  # Banco Central de Costa Rica
  bccr <- reactive({
    date_range <- input$dateRange
    fecha_inicio <- format(date_range[1], "%d/%m/%Y")
    fecha_final <- format(date_range[2], "%d/%m/%Y")
    
    df_compra <- get_exchange_data_cached("317", fecha_inicio, fecha_final)
    df_venta <- get_exchange_data_cached("318", fecha_inicio, fecha_final)
    
    if (nrow(df_compra) == 0 || nrow(df_venta) == 0) {
      return(data.frame(Fecha = character(0), Valor_compra = numeric(0),
                        Valor_venta = numeric(0)))
    }
    
    df_compra <- rename(df_compra, Fecha_compra = Fecha, Valor_compra = Valor)
    df_venta <- rename(df_venta, Fecha_venta = Fecha, Valor_venta = Valor)
    
    final_df <- inner_join(df_compra, df_venta,
                           by = c("Fecha_compra" = "Fecha_venta"))
    final_df$Fecha <- final_df$Fecha_compra
    final_df$Fecha_compra <- NULL
    final_df$Fecha_venta <- NULL
    
    final_df$Fecha <- as.Date(ymd_hms(final_df$Fecha))
    final_df$Fecha_format <- format(final_df$Fecha, "%d-%m-%Y")
    
    return(final_df)
  })
  
  # Banco Nacional 
  bn <- reactive({
    date_range <- input$dateRange
    fecha_inicio <- format(date_range[1], "%d/%m/%Y")
    fecha_final <- format(date_range[2], "%d/%m/%Y")
    
    df_compra <- get_exchange_data_cached("3149", fecha_inicio, fecha_final)
    df_venta <- get_exchange_data_cached("3208", fecha_inicio, fecha_final)
    
    if (nrow(df_compra) == 0 || nrow(df_venta) == 0) {
      return(data.frame(Fecha = character(0), Valor_compra = numeric(0),
                        Valor_venta = numeric(0)))
    }
    
    df_compra <- rename(df_compra, Fecha_compra = Fecha, Valor_compra = Valor)
    df_venta <- rename(df_venta, Fecha_venta = Fecha, Valor_venta = Valor)
    
    final_df <- inner_join(df_compra, df_venta,
                           by = c("Fecha_compra" = "Fecha_venta"))
    final_df$Fecha <- final_df$Fecha_compra
    final_df$Fecha_compra <- NULL
    final_df$Fecha_venta <- NULL
    
    final_df$Fecha <- as.Date(ymd_hms(final_df$Fecha))
    final_df$Fecha_format <- format(final_df$Fecha, "%d-%m-%Y")
    
    return(final_df)
  })
  
  
  # Banco de Costa Rica
  bcr <- reactive({
    print("Ejecutando la función 'bcr'")
    date_range <- input$dateRange
    fecha_inicio <- format(date_range[1], "%d/%m/%Y")
    fecha_final <- format(date_range[2], "%d/%m/%Y")
    
    df_compra <- get_exchange_data_cached("3148", fecha_inicio, fecha_final)
    df_venta <- get_exchange_data_cached("3207", fecha_inicio, fecha_final)
    
    
    if (nrow(df_compra) == 0 || nrow(df_venta) == 0) {
      return(data.frame(Fecha = character(0), Valor_compra = numeric(0),
                        Valor_venta = numeric(0)))
    }
    
    df_compra <- rename(df_compra, Fecha_compra = Fecha, Valor_compra = Valor)
    df_venta <- rename(df_venta, Fecha_venta = Fecha, Valor_venta = Valor)
    
    final_df <- inner_join(df_compra, df_venta,
                           by = c("Fecha_compra" = "Fecha_venta"))
    final_df$Fecha <- final_df$Fecha_compra
    final_df$Fecha_compra <- NULL
    final_df$Fecha_venta <- NULL
    
    final_df$Fecha <- as.Date(ymd_hms(final_df$Fecha))
    final_df$Fecha_format <- format(final_df$Fecha, "%d-%m-%Y")
    
    return(final_df)
  })
  
  
  # Lógica de selección de datos del banco
  datos_banco <- reactive({
    if (input$banco == "bccr") {
      return(bccr())
    } else if (input$banco == "bn") {
      return(bn())
    }
    else if (input$banco == "bcr"){
      return(bcr())
    }
  })
  
  # Tabla de datos de cambio
  output$exchangeTable <- renderTable({
    df <- datos_banco()
    if (is.null(df) || nrow(df) == 0) {
      print("No hay datos para mostrar en la tabla 'exchangeTable'")
      return(NULL)
    }
    df <- df[, c("Fecha_format", "Valor_compra", "Valor_venta")]
    df <- df %>% 
      rename("Fecha" = "Fecha_format", "Compra" = "Valor_compra", 
             "Venta"  = "Valor_venta")
    return(df)
  })
  
  # Gráfico 
  output$exchangePlot <- renderPlotly({
    df <- datos_banco()
    
    # Determinar el formato de las fechas en el eje x
    rango_fechas <- as.numeric(difftime(max(df$Fecha), min(df$Fecha),
                                        units = "days"))
    
    if (rango_fechas <= 60) {
      date_breaks <- "1 day"
      date_format <- "%b %d %Y"
    } else if (rango_fechas <= 365) {
      date_breaks <- "1 week"
      date_format <- "%b %d %Y"
    } else if (rango_fechas <= 5*365) {
      date_breaks <- "1 month"
      date_format <- "%b %Y"
    } else {
      date_breaks <- "1 year"
      date_format <- "%Y"
    }
    
    # gráfico 
    p <- ggplot(df, aes(x = Fecha)) +
      geom_line(aes(y = Valor_venta, color = "Swirl", group = 2)) +
      ylab("Venta ($)") +
      ggtitle("Evolución del tipo de cambio") +
      scale_x_date(
        date_breaks = date_breaks, 
        date_labels = date_format,
        limits = c(min(df$Fecha), max(df$Fecha))
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = mi_paleta["Cod gray"]),
        panel.background = element_rect(fill = mi_paleta["Cod gray"]),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = mi_paleta["Swirl"]),
        axis.text.x = element_text(color = mi_paleta["Swirl"], angle = 45, 
                                   hjust = 1),
        plot.title = element_text(color = mi_paleta["Swirl"], hjust = 0.5)
      ) +
      theme(legend.position = "none") 
    
    # Convertir el gráfico ggplot a Plotly
    ggplotly(p, tooltip = "y")
    
  })
  
  #---------- Variables Macroenomicas ---------#
  
  pib_data <- reactive({
    df <-   get_wb_data("CRI", input$variable, "1980", "2023")
    df$USD <- as.numeric(gsub(",", "", as.character(df$USD)))
    return(df)
  })
  
  # Grafico
  output$pib_plot <- renderPlotly({
    plot_ly(data = pib_data(), x = ~Fecha, y = ~USD, name = input$variable,
            type = 'scatter', mode = 'lines', line = list(color = mi_paleta["Swirl"], width = 2)) %>%
      layout(
        yaxis = list(title = "Valor"),
        xaxis = list(title = "Fecha"),
        title = "",
        showlegend = FALSE,
        legend = list(x = 0.1, y = 0.9),
        margin = list(l = 50, r = 50, b = 50, t = 50),
        paper_bgcolor = mi_paleta["Cod gray"],
        plot_bgcolor = mi_paleta["Cod gray"]
      )
  })
  
  # Tabla PIB
  gdp <-  reactive({
    df <- get_wb_data("CRI", input$variable, "1980", "2023")
    return(df)
  })
  
  # Tabla % PIB
  output$pib_table <- renderTable({
    df <- gdp()
    return(df)
  })
  
}

# Ejecutar la aplicación 
shinyApp(ui = ui, server = server)