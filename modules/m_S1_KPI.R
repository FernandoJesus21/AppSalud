# Metric module with summary

import("shiny")
import("dplyr")
import("htmltools")
import("glue")
import("lubridate")

export("ui")
export("init_server")

consts <- use("constants.R")

ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "panel-header breakdown-header",
      div(
        class = "item",
        textOutput(ns("tituloCabecera"))
      ),
    ),
    div(
      class = "chart-breakdown-container",
      uiOutput(ns("summary"))
    )
  )
  
}

init_server <- function(id, df, y, m, p, i, i18n) {
  callModule(server, id, df, y, m, p, i, i18n)
}

server <- function(input, output, session, df, y, m, p, i, i18n) {
  
  plotTitle <- reactive({
    t <- i18n()$t(consts$obtenerAttr_por_id(unique(df$c1), "S1_KPI_title"))
  })
  
  selDF <- reactive({
    
    df %>%
      filter(
        consts$conditional(m() == "0", year(date) == y() & c2 == "A"), #en caso de que el usuario eligiera "todos los meses", filtro los registros con valores acumulados anuales
        consts$conditional(m() != "0", year(date) == y() & month(date) == m() & c2 == "M"), #si el usuario eligio un mes, filtro por ese mes
        consts$conditional(p() == "[Todas]", provincia == p()), #si se eligio "todas las provincias", quito aquellos casos sin clasificar
        consts$conditional(p() == "[Desconocido]", provincia == "SIN ESPECIFICAR"), #si se eligio "desconocido" quito todos los casos clasificados
        consts$conditional(p() %in% consts$listaProvincias, provincia == p()), #si la seleccion del usuario esta contenida en la lista de las provincias, filtro por esa seleccion
      )
    
  })
  
  
  output$tituloCabecera <- renderText({
    if(nrow(df) != 0){
      plotTitle()
    }else{
      paste0("test")
    }
  })
  
  
  
  output$summary <- renderUI({
    
    metric_total_value <- 0
    
    if(nrow(selDF()) != 0){
      metric_total_value <- format(selDF()$valor, big.mark = ".", decimal.mark = ",")
    }
    
    glue::glue('
        <img class="icon" src="assets/{i}">
        <span class="metric">{metric_total_value}</span>
      ') %>% HTML()
  })
}