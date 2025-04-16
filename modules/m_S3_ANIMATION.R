

import("shiny")
import("shinyWidgets")
import("dplyr")
import("ggplot2")
import("stats")
import("scales")
import("shinyfullscreen")
import("shinycssloaders")
import("lubridate")
import("DT")
import("shiny.i18n")

export("ui")
export("init_server")


consts <- use("constants.R")


ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "panel-header breakdown-header",
      div(class = "item tituloC",
          textOutput(ns("tituloCabecera")
          )
      ),
      actionButton(ns("show_disp_ganimado"), "", icon("window-maximize")),
    ),
    div(
      class = "chart-breakdown-container",
      plotly::plotlyOutput(outputId = ns("ganimado_dashboard"), width = "100%", height = "496px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2)
     ) 
  ) 
}


init_server <- function(id, df, p, i18n) {
  callModule(server, id, df, p, i18n)
}



server <- function(input, output, session, df, p, i18n) {
  
  ns <- session$ns
  
  plotTitle <- reactive({
    t1 = ""
    if(p() == "[Todas]"){
      t <- i18n()$t("311")
    }else{
      t <- i18n()$t("321")
    }
  })
  
  #se encarga de mostrar el titulo en la cabecera de la ventana
  output$tituloCabecera <- renderText({
    plotTitle()
  })
  
  
  observeEvent(input$show_disp_ganimado, {
    showModal(
      modalDialog(
        plotly::plotlyOutput(session$ns("ganimado_modal"), width = "1280", height = "720px")  %>% withSpinner(color= consts$loading_color, image = consts$loading_image2),
        easyClose = F,
        footer = tagList(
          modalButton(i18n()$t("86"))
        )
      )
    )
  })
  
  dfr <- reactive({

    df %>%
      dplyr::filter(
        consts$conditional(p() == "[Todas]", provincia %in% consts$listaProvincias & departamento == "[Todos]"), #si se eligio "todas las provincias", quito aquellos casos sin clasificar
        consts$conditional(p() %in% consts$listaProvincias, provincia == p() & departamento != "[Todos]"), #si la seleccion del usuario esta contenida en la lista de las provincias, filtro por esa seleccion
      )

  })
  
  model <- reactive({
    
    fit <- lm( C ~ CF, data = dfr())
    
  })
  
  
  #modelo lineal

  
  output$ganimado_dashboard <- plotly::renderPlotly({
    
    suppressWarnings({
    

      if(p() == "[Todas]"){
        
        
        fig <- plotly::plot_ly( dfr(),
                                x = ~CF, 
                                y = ~C, 
                                size = ~VCF, 
                                color = ~VCF, 
                                frame = ~periodo, 
                                text = paste0(dfr()$provincia, "\n", i18n()$t("53"), ": ", dfr()$V, "\n", i18n()$t("52"), ": ", dfr()$CF, "\n", i18n()$t("51"), ": ", dfr()$C), 
                                hoverinfo = "text",
                                type = 'scatter',
                                mode = 'markers'
        
        ) %>% plotly::layout(
          yaxis = list(
            type = "log",
            title = i18n()$t("51")
          ),
          xaxis = list(
            title = i18n()$t("52"),
            range= c(-20,max(dfr()$CF) +1000)
          )
        ) %>%
          plotly::add_annotations(
            x= .6,
            y= .25,
            xref = "paper",
            yref = "paper",
            text = i18n()$t("61"),
            showarrow = F
          )

        fig <- fig %>% plotly::animation_opts(
          1000, easing = "elastic", redraw = FALSE
        )


        fig <- fig %>%
          plotly::add_lines(x = ~CF,
                            y = fitted(model()),
                            inherit = FALSE,
                            line = list(
                              color = "green"
                            ),
                            showlegend = FALSE,
                            mode = "lines"
          )

        fig <- fig %>%
          plotly::add_lines(x = ~C,
                            y = ~C,
                            line = list(
                              color = "red"
                            ),
                            inherit = FALSE,
                            showlegend = FALSE,
                            mode = "lines"
          )

        fig <- fig %>%
          plotly::colorbar(
            orientation = "h",
            yanchor='bottom',
            title = '',
            thickness = 15,
            len = .6,
            y = .05,
            x =.6
          )
        
        fig
        
        
      }else{
        
        
        fig <- plotly::plot_ly( dfr(),
                                x = ~CF, 
                                y = ~C, 
                                size = ~VCF, 
                                color = ~VCF,
                                colors = "viridis",
                                frame = ~periodo, 
                                text = paste0(dfr()$provincia, "\n", i18n()$t("53"), ": ", dfr()$V, "\n", i18n()$t("52"), ": ", dfr()$CF, "\n", i18n()$t("51"), ": ", dfr()$C), 
                                hoverinfo = "text",
                                type = 'scatter',
                                mode = 'markers'
        ) %>% plotly::layout(
          yaxis = list(
            type = "log",
            title = i18n()$t("51")
          ),
          xaxis = list(
            title = i18n()$t("52"),
            range= c(-20,max(dfr()$CF) + 100)
          )
        ) %>%
          plotly::add_annotations(
            x= .6,
            y= .25,
            xref = "paper",
            yref = "paper",
            text = i18n()$t("61"),
            showarrow = F
          )
        
        fig <- fig %>% plotly::animation_opts(
          1000, easing = "elastic", redraw = FALSE
        )
        
        
        fig <- fig %>%
          plotly::add_lines(x = ~CF, 
                            y = fitted(model()),
                            inherit = FALSE,
                            line = list(
                              color = "green"
                            ),
                            showlegend = FALSE,
                            mode = "lines"
          )
        
        fig <- fig %>%
          plotly::add_lines(x = ~C, 
                            y = ~C,
                            line = list(
                              color = "red"
                            ),
                            inherit = FALSE,
                            showlegend = FALSE,
                            mode = "lines"
          )
        
        fig <- fig %>%
          plotly::colorbar(
            orientation = "h",
            yanchor='bottom',
            title = '',
            thickness = 15,
            len = .6,
            y = .05,
            x =.6
          )
        
        fig

        
        
      }
      

    
    })
    
  })
  
  

  output$ganimado_modal <- plotly::renderPlotly({
    
    suppressWarnings({
      
      
      if(p() == "[Todas]"){
        
        
        fig <- plotly::plot_ly( dfr(),
                                x = ~CF, 
                                y = ~C, 
                                size = ~VCF, 
                                color = ~VCF, 
                                frame = ~periodo, 
                                text = paste0(dfr()$provincia, "\n", i18n()$t("53"), ": ", dfr()$V, "\n", i18n()$t("52"), ": ", dfr()$CF, "\n", i18n()$t("51"), ": ", dfr()$C), 
                                hoverinfo = "text",
                                type = 'scatter',
                                mode = 'markers'

        ) %>% plotly::layout(
          yaxis = list(
            type = "log",
            title = i18n()$t("51")
          ),
          xaxis = list(
            title = i18n()$t("52"),
            range= c(-20,max(dfr()$CF) +1000)
          )
        ) %>%
          plotly::add_annotations(
            x= .6,
            y= .15,
            xref = "paper",
            yref = "paper",
            text = i18n()$t("61"),
            showarrow = F
          )
        
        fig <- fig %>% plotly::animation_opts(
          1000, easing = "elastic", redraw = FALSE
        )
        
        
        fig <- fig %>%
          plotly::add_lines(x = ~CF,
                            y = fitted(model()),
                            inherit = FALSE,
                            line = list(
                              color = "green"
                            ),
                            showlegend = FALSE,
                            mode = "lines"
          )
        
        fig <- fig %>%
          plotly::add_lines(x = ~C,
                            y = ~C,
                            line = list(
                              color = "red"
                            ),
                            inherit = FALSE,
                            showlegend = FALSE,
                            mode = "lines"
          )
        
        fig <- fig %>%
          plotly::colorbar(
            orientation = "h",
            yanchor='bottom',
            title = '',
            thickness = 15,
            len = .4,
            y = .05,
            x =.6
          )
        
        fig
        
        
        
        
      }else{
        
        
        fig <- plotly::plot_ly( dfr(),
                                x = ~CF, 
                                y = ~C, 
                                size = ~VCF, 
                                color = ~VCF,
                                colors = "viridis",
                                frame = ~periodo, 
                                text = paste0(dfr()$provincia, "\n", i18n()$t("53"), ": ", dfr()$V, "\n", i18n()$t("52"), ": ", dfr()$CF, "\n", i18n()$t("51"), ": ", dfr()$C), 
                                hoverinfo = "text",
                                type = 'scatter',
                                mode = 'markers'
        ) %>% plotly::layout(
          yaxis = list(
            type = "log",
            title = i18n()$t("51")
          ),
          xaxis = list(
            title = i18n()$t("52"),
            range= c(-20,max(dfr()$CF) + 100)
          )
        ) %>%
          plotly::add_annotations(
            x= .6,
            y= .15,
            xref = "paper",
            yref = "paper",
            text = i18n()$t("61"),
            showarrow = F
          )
        
        fig <- fig %>% plotly::animation_opts(
          1000, easing = "elastic", redraw = FALSE
        )
        
        
        fig <- fig %>%
          plotly::add_lines(x = ~CF, 
                            y = fitted(model()),
                            inherit = FALSE,
                            line = list(
                              color = "green"
                            ),
                            showlegend = FALSE,
                            mode = "lines"
          )
        
        fig <- fig %>%
          plotly::add_lines(x = ~C, 
                            y = ~C,
                            line = list(
                              color = "red"
                            ),
                            inherit = FALSE,
                            showlegend = FALSE,
                            mode = "lines"
          )
        
        fig <- fig %>%
          plotly::colorbar(
            orientation = "h",
            yanchor='bottom',
            title = '',
            thickness = 15,
            len = .4,
            y = .05,
            x =.6
          )
        
        fig
        
        
        
      }
      
      
      
    })
    
  })

  
}