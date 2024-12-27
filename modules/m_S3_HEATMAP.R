

import("shiny")
import("shinyWidgets")
import("dplyr")
import("ggplot2")
import("DT")
import("scales")
import("shinyfullscreen")
import("shinycssloaders")
import("lubridate")
import("shiny.i18n")

export("ui")
export("init_server")


consts <- use("constants.R")

expose("utilities/getTranslationArray.R")


plotLabels <- c("Primera", "Segunda", "Adicional", "Refuerzo", "Única", "Cant. Vacunas:")
names(plotLabels) <- c("26", "27", "28", "29", "30", "82")

ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "panel-header breakdown-header",
      div(class = "item tituloC", 
          textOutput(ns("tituloCabecera")),
      ),
      actionButton(ns("show_disp_ganimado"), "", icon("window-maximize"))
    ),
    div(
      class = "chart-breakdown-container",
      plotOutput(outputId = ns("ganimado_dashboard"), width = "100%", height = "180px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2)
    ) 
  ) 
}


init_server <- function(id, df, p, i18n) {
  callModule(server, id, df, p, i18n)
}



server <- function(input, output, session, df, p, i18n) {
  
  ns <- session$ns
  
  plotTitle <- reactive({
    t <- i18n()$t("351")
  })
  
  #se encarga de mostrar el titulo en la cabecera de la ventana
  output$tituloCabecera <- renderText({
    if(p() == "[Todas]"){
      paste0(i18n()$t("80"), plotTitle())
    }else{
      paste0(p(), ": ", plotTitle())
    }
  })
  
  labelTranslatePlot <- reactive({
    t <- names(getTranslationArray(plotLabels, i18n()))
  })
  
  dfr <- reactive({
    
    df %>%
      dplyr::filter(
        consts$conditional(p() == "[Todas]", provincia == p()), #si se eligio "todas las provincias", quito aquellos casos sin clasificar
        consts$conditional(p() %in% consts$listaProvincias, provincia == p()) #si la seleccion del usuario esta contenida en la lista de las provincias, filtro por esa seleccion
      )
    
  })
  
  observeEvent(input$show_disp_ganimado, {
    showModal(
      modalDialog(
        #plotly::plotlyOutput(session$ns("ganimado_modal"), width = "1280", height = "420px")  %>% withSpinner(color= consts$loading_color, image = consts$loading_image2),
        fullscreen_this(plotOutput(session$ns("ganimado_modal"), width = "1280", height = "420px"))  %>% withSpinner(color= consts$loading_color, image = consts$loading_image2),
        easyClose = F,
        footer = tagList(
          modalButton(i18n()$t("86"))
        )
      )
    )
  })


  output$ganimado_dashboard <- renderPlot({

    ggplot(dfr(), aes(x = date, y = c1, fill = valor)) +
      geom_tile(colour="white", linewidth = 0.25, width=31) + 
      scale_fill_viridis_c(trans = 'log',
                           breaks = c(10, 100, 1000, 10000, 100000, 1000000),
                           labels=label_comma(big.mark = ".", decimal.mark = ",")
      ) +
      scale_y_discrete(expand=c(0, 0), labels = labelTranslatePlot()[1:5]) +
      labs(x="", y="", fill = labelTranslatePlot()[6])+
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks=element_line(linewidth=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        legend.key.width=grid::unit(0.3, "cm")
      ) +
      scale_x_date(date_breaks = "3 months",
                   minor_breaks = NULL,
                   expand = expansion(add = 15),
                   labels = consts$format_dates)
    
  })
  

  #output$ganimado_modal <- plotly::renderPlotly({
  output$ganimado_modal <- renderPlot({
    
    
    #plotly::ggplotly(
      
      ggplot(dfr(), aes(x = date, y = c1, fill = valor)) +
        geom_tile(colour="white", linewidth = 0.25, width=31) + 
        scale_fill_viridis_c(trans = 'log',
                             breaks = c(10, 100, 1000, 10000, 100000, 1000000),
                             labels=label_comma(big.mark = ".", decimal.mark = ",")
        ) +
        scale_y_discrete(expand=c(0, 0), labels = labelTranslatePlot()[1:5])+
        labs(x="", y="", fill = labelTranslatePlot()[6])+
        theme_minimal(base_size=12) +
        theme(
          legend.text=element_text(face="bold"),
          axis.ticks=element_line(linewidth=0.4),
          plot.background=element_blank(),
          panel.border=element_blank(),
          legend.key.width=grid::unit(0.3, "cm")
        ) + 
        scale_x_date(date_breaks = "1 month",       
                     minor_breaks = NULL,              
                     expand = expansion(add = 15),  
                     labels = consts$format_dates)
      
    #) 
    
  })

  
}