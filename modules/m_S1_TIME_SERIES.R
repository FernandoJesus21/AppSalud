# Ggplot horizontal bar chart module

import("shiny")
import("shinyWidgets")
import("dplyr")
import("dygraphs")
import("utils")
import("lubridate")
import("shinycssloaders")
import("xts")

export("ui")
export("init_server")


consts <- use("constants.R")
expose("utilities/getMetricsChoices.R")
expose("utilities/getTranslationArray.R")


ui <- function(id, i18n) {
  ns <- NS(id)
  
  #medidas disponibles en la serie temporales
  selecciones <- getMetricsChoices(names(consts$lista_de_medidas), consts$lista_de_medidas, "measure_label")
  
  tagList(
    div(
      class = "panel-header breakdown-header",
      div(
        class = "item tituloC",
        textOutput(ns("tituloCabecera"))
      ),
      actionButton(ns("show_st_modal"), "", icon("window-maximize")),
      dropdownButton(
        #tags$h5(i18n$t("38")),
        tags$h5(textOutput(ns("measure_title"))),
        radioGroupButtons(
          inputId = ns("swap_plot"),
          choices = selecciones,
          selected = selecciones[[1]]
        ),
        #circle = TRUE,
        icon = icon("chart-simple"),
        #inline = T,
        inputId = ns("mydropdown")
      ),
      includeScript(consts$script_bloquear_selecciones)
    ),
    div(
      class = "chart-breakdown-container",
      dygraphOutput(ns("st_dashboard"), width = "100%", height = "150px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2)
    )
  )
}

#y: periodo anual
#m: mes
#p: provincia
#df: data frame necesario para graficar

#funcion que inicializa el modulo. Es llamada desde el script server.R
init_server <- function(id, df, y, m, p, i18n) {
  callModule(server, id, df, y, m, p, i18n)
}


#funcion server del modulo
server <- function(input, output, session, df, y, m, p, i18n) {
  
  ns <- session$ns
  
  plotTitle <- reactive({
    t <- i18n()$t(consts$obtenerAttr_por_id(input$swap_plot, "S1_TS_chart_title"))
  })
  
  output$measure_title <- renderText({
    t <- i18n()$t("50")
  })
  
  #titulo del panel del grafico
  output$tituloCabecera <- renderText({
    #consts$obtenerAttr_por_id(input$swap_plot, "desc_ST")
    plotTitle()
    
  })
  
  #observer que actualiza las medidas disponibles
  # esto depende de las categorias disponibles en el df que se pasa como parametro
  observeEvent(c(df, i18n()), {
    
    if(nrow(df) != 0){ #para el modo de pruebas (NO_DATA)
      
      plot_choices_ids <- names(consts$lista_de_medidas)
      raw_med_disponibles <- plot_choices_ids[plot_choices_ids %in% unique(df$c1)]
      #sel <- getMetricsChoices(raw_med_disponibles, consts$lista_de_medidas, "title_alt")
      sel <- getMetricsChoices(raw_med_disponibles, consts$lista_de_medidas, "measure_label")
      sel <- getTranslationArray(sel, i18n())
      updateRadioGroupButtons(session,
                              "swap_plot",
                              selected = sel[[1]],
                              choices = sel
      )
    }

  })
  

  
  #observer que dispara la funcion de render de la ventana modal cuando el usuario presion el boton maximizar
  observeEvent(input$show_st_modal, {
    showModal(
      modalDialog(
        dygraphOutput(session$ns("st_modal"), width = "1280", height = "480px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2),
        easyClose = F,
        footer = tagList(
          modalButton(i18n()$t("86"))
        )
      )
    )
  })

  
  #DF reactivo con todas los filtros aplicados segun la seleccion del usuario
  selDF <- reactive({
    
    df %>%
      filter(
        consts$conditional(input$swap_plot != "", c1 == input$swap_plot), #asigno el df de confirmados/fallecidos segun haya elegido el usuario
        consts$conditional(m() == "0", year(date) == y()), #en caso de que el usuario eligiera "todos los meses", filtro los registros con valores acumulados anuales
        consts$conditional(m() != "0", year(date) == y() & month(date) == m()), #si el usuario eligio un mes, filtro por ese mes
        consts$conditional(p() == "[Todas]", provincia == p()), #si se eligio "todas las provincias", quito aquellos casos sin clasificar
        consts$conditional(p() == "[Desconocido]", provincia == "SIN ESPECIFICAR"), #si se eligio "desconocido" quito todos los casos clasificados
        consts$conditional(p() %in% consts$listaProvincias, provincia == p()) #si la seleccion del usuario esta contenida en la lista de las provincias, filtro por esa seleccion
      )
    
  })
  
  #proceso que renderiza las series temporales en el panel de aplicacion
  output$st_dashboard <- renderDygraph({
    
    if(nrow(selDF()) != 0){
      st <- xts(selDF()$valor, order.by = selDF()$date)
      colnames(st) <- i18n()$t(names(getMetricsChoices(input$swap_plot, consts$lista_de_medidas, "measure_label")))
      dygraph(st, main = "") %>% 
        #dyBarChart() %>%
        dyOptions(fillGraph = TRUE, fillAlpha = 0.4, colors = consts$lista_de_medidas[[input$swap_plot]]$color)
        #dyCSS("www/dygraph.css")

    }else{
      cat("Se ha intentado graficar [ST] sin datos.\n")
      dygraph(stats::ts(1, frequency = 1, start = c(2023)), 
              main = "Sin datos") %>%
        dyLegend(width = 0) 
    }

  })
  
  #proceso que renderiza las series temporales maximizadas con la ventana modal
  output$st_modal <- renderDygraph({
    
    if(nrow(selDF()) != 0){
      st <- xts(selDF()$valor, order.by = selDF()$date)
      colnames(st) <- i18n()$t(names(getMetricsChoices(input$swap_plot, consts$lista_de_medidas, "measure_label")))
      dygraph(st, main = "") %>% 
        dyRangeSelector() %>%
        #dyBarChart() %>% 
        dyOptions(fillGraph = TRUE, fillAlpha = 0.4, colors = consts$lista_de_medidas[[input$swap_plot]]$color)
    }else{
      cat("Se ha intentado graficar [ST @modal] sin datos.\n")
      dygraph(stats::ts(1, frequency = 1, start = c(2023)), main = "Sin datos") %>%
        dyLegend(width = 0)
    }
    
  })
  
}