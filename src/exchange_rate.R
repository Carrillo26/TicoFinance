# Clase para descargar data del BCCR

library(R6)
library(httr)
library(xml2)


tipo_cambio <- R6Class(
  "tipo_cambio",
  public = list(
    indicador = NULL,
    fecha_inicio = NULL,
    fecha_final = NULL,
    nombre = NULL,
    sub_niveles = NULL,
    correo_electronico = NULL,
    token = NULL,
    
    initialize = function(indicador, fecha_inicio, fecha_final, nombre, sub_niveles, correo_electronico, token) {
      self$indicador <- indicador
      self$fecha_inicio <- fecha_inicio
      self$fecha_final <- fecha_final
      self$nombre <- nombre
      self$sub_niveles <- sub_niveles
      self$correo_electronico <- correo_electronico
      self$token <- token
    },
    
    descargar_datos = function() {
      soap_body <- paste0(
        '<?xml version="1.0" encoding="utf-8"?>',
        '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ',
        'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ',
        'xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
        '<soap:Body>',
        '<ObtenerIndicadoresEconomicos xmlns="http://ws.sdde.bccr.fi.cr">',
        '<Indicador>', self$indicador, '</Indicador>',
        '<FechaInicio>', self$fecha_inicio, '</FechaInicio>',
        '<FechaFinal>', self$fecha_final, '</FechaFinal>',
        '<Nombre>', self$nombre, '</Nombre>',
        '<SubNiveles>', self$sub_niveles, '</SubNiveles>',
        '<CorreoElectronico>', self$correo_electronico, '</CorreoElectronico>',
        '<Token>', self$token, '</Token>',
        '</ObtenerIndicadoresEconomicos>',
        '</soap:Body>',
        '</soap:Envelope>'
      )
      
      print(soap_body)
      
      respuesta <- httr::POST(
        url = "https://gee.bccr.fi.cr/Indicadores/Suscripciones/WS/wsindicadoreseconomicos.asmx",
        add_headers(
          'Content-Type' = 'text/xml; charset=utf-8',
          'SOAPAction' = 'http://ws.sdde.bccr.fi.cr/ObtenerIndicadoresEconomicos'
        ),
        body = soap_body
      )
      
      if (httr::status_code(respuesta) != 200) {
        mensaje_error <- paste("Error en la solicitud:", httr::status_code(respuesta))
        print(httr::content(respuesta, as = "text"))
        stop(mensaje_error)
      }
      
      
      contenido <- httr::content(respuesta, as = "text")
      xml <- read_xml(contenido)
      
      valores <- xml_find_all(xml, ".//INGC011_CAT_INDICADORECONOMIC")
      datos <- data.frame(
        COD_INDICADORINTERNO = xml_text(xml_find_all(valores, ".//INDICADOR")),
        DES_FECHA = as.Date(xml_text(xml_find_all(valores, ".//FECHA"))),
        NUM_VALOR = as.numeric(xml_text(xml_find_all(valores, ".//VALOR"))),
        stringsAsFactors = FALSE
      )
      
      return(datos)
    }
  )
)

# Ejemplo de uso:
#descargar <- tipo_cambio$new(
# indicador = 317,
#  fecha_inicio = "01/01/2022",
#  fecha_final = "01/02/2022",
#  nombre = " nombre ",
#  sub_niveles = "N",
#  correo_electronico = "usuario@email.com",
#  token = "tokne"
#)

#datos <- descargar$descargar_datos()
