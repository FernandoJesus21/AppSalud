# Ggplot horizontal bar chart module

import("shiny")
import("shinyWidgets")
import("dplyr")
import("ggplot2")
import("echarts4r")
import("utils")
import("lubridate")
import("shinycssloaders")
import("shiny.i18n")

export("ui")
export("init_server")


consts <- use("constants.R")
expose("utilities/getMetricsChoices.R")
expose("utilities/getTranslationArray.R")




selecciones <- getMetricsChoices(c("C", "CF", "V"), consts$lista_de_medidas, "measure_label")


ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "panel-header breakdown-header",
      div(
        class = "item tituloC",
        textOutput(ns("tituloCabecera"))
      ),
      actionButton(ns("show_st_modal"), "", icon("window-maximize")),
      dropdownButton(
        #tags$h5("Medidas:"),
        tags$h5(textOutput(ns("measure_title"))),
        radioGroupButtons(
          inputId = ns("swap_plot"),
          choices = selecciones,
          selected = selecciones[1]
        ),
        circle = TRUE,
        icon = icon("chart-simple"),
        inline = T,
        inputId = ns("mydropdown")
      ),
      includeScript(consts$script_bloquear_selecciones)
    ),
    div(
      class = "chart-breakdown-container",
      plotOutput(ns("st_dashboard"), width = "100%", height = "150px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2)
    )
  )
}


init_server <- function(id, df, y, m, p, i18n, l) {
  callModule(server, id, df, y, m, p, i18n, l)
}



server <- function(input, output, session, df, y, m, p, i18n, l) {
  
  ns <- session$ns
  
  
  #el titulo que se encarga de mostrar la funcion de arriba es construido en esta funcion
  plotTitle <- reactive({
    t <- i18n()$t(consts$obtenerAttr_por_id(input$swap_plot, "S2_RE_chart_title"))
  })
  
  output$tituloCabecera <- renderText({
    
    if(p() == "[Todas]"){
      paste0(i18n()$t("80"), plotTitle())
    }else{
      paste0(p(), ": ", plotTitle())
    }
  })
  
  
  output$measure_title <- renderText({
    t <- i18n()$t("50")
  })
  
  #observer que realiza las traducciones
  observeEvent(c(df, i18n()), {
    sel <- getTranslationArray(selecciones, i18n())
    updateRadioGroupButtons(session,
                            "swap_plot",
                            selected = sel[[1]],
                            choices = sel
    )
  })
  
  
  observeEvent(input$show_st_modal, {
    showModal(
      modalDialog(
        echarts4rOutput(session$ns("st_modal"), width = "1280", height = "480px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2),
        easyClose = F,
        footer = tagList(
          modalButton(i18n()$t("86"))
        )
      )
    )
  })

  
  selDF <- reactive({
    
    df %>%
      filter(
        consts$conditional(input$swap_plot != "", c1 == input$swap_plot), #asigno el df de confirmados/fallecidos segun haya elegido el usuario
        consts$conditional(m() == "0", year(date) == y() & c2 == "A"), #en caso de que el usuario eligiera "todos los meses", filtro los registros con valores acumulados anuales
        consts$conditional(m() != "0", year(date) == y() & month(date) == m() & c2 == "M"), #si el usuario eligio un mes, filtro por ese mes
        consts$conditional(p() %in% consts$elecciones_pcias, provincia == p()), #si la seleccion del usuario esta contenida en la lista de las provincias, filtro por esa seleccion
      )
    
  })
  
  
  output$st_dashboard <- renderPlot({
    
    if(nrow(selDF()) != 0){

      ggplot(selDF(), aes(x = rango_etario, y = valor)) +
        geom_col(fill = consts$obtenerAttr_por_id(input$swap_plot, "color")) +
        theme_light() +
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank()
              )
      
    }else{
      cat("Se ha intentado graficar [S2][RANGO_ETARIO] sin datos.\n")

      
    }
    
  })
  
  
  output$st_modal <- renderEcharts4r({
    
    aux <- selDF()
    aux <- aux[match(c("<12","12-17","18-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99",">=100", "*sin dato*"), aux$rango_etario)]
    
    if(nrow(selDF()) != 0){
      
      aux |>
        e_charts(rango_etario, reorder = F) |>
        e_bar(valor, reorder = F) |>
        e_tooltip(trigger = "axis")|>
        e_legend(FALSE) |>
        e_title(plotTitle()) |>
        e_x_axis(axisLabel = list(interval = 0, rotate = 45)) |>
        e_color(consts$lista_de_medidas[[input$swap_plot]]$color)
      
    }else{
      cat("Se ha intentado graficar [S2][RANGO_ETARIO@modal] sin datos.\n")
      
    }
    
  })
  
}