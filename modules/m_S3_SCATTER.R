# Ggplot horizontal bar chart module
#https://stackoverflow.com/questions/70794452/how-to-change-the-type-of-graph-based-off-a-shiny-input

#importo bibliotecas
import("shiny")
import("shinyWidgets")
import("dplyr")
import("ggplot2")
import("scales")
import("shinyfullscreen")
import("shinycssloaders")
import("lubridate")
import("shiny.i18n")
#exporto funciones a utilizar en otros script
export("ui")
export("init_server")

#declaro el objeto constantes para utilizar sus funciones
consts <- use("constants.R")

expose("utilities/getTranslationArray.R")

selecciones <- c("Dispersión", "Histograma 2D", "Densidad 2D", "M. de calor")
names(selecciones) <- c("331", "332", "333", "334")

font <- "RobotoCondensed"

#Funcion ui del modulo (es invocada desde el script ui principal)
ui <- function(id) {
  #definicion del namespace para que el modulo funcione correctamente
  ns <- NS(id)
  #defino todos los elementos visuales que tendra el modulo
  tagList(
    div(
      class = "panel-header breakdown-header",
      div(class = "item tituloC", 
        textOutput(ns("tituloCabecera")),
      ),
      actionButton(ns("show_disp_modal"), "", icon("window-maximize")),
      dropdownButton(
        #tags$h5("Ver como:"),
        tags$h5(textOutput(ns("chart_type"))),
        radioGroupButtons(
          inputId = ns("selected_type"),
          choices = selecciones
        ),
        circle = TRUE,
        icon = icon("shapes"),
        inline = F,
        inputId = ns("mydropdown")
      ),
      includeScript(consts$script_bloquear_selecciones) #llamada a script adjunto (js)
    ),
    div(
      class = "chart-breakdown-container",
      plotOutput(outputId = ns("disp_dashboard"), width = "100%", height = "240px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2)
    ) 
  ) 
}

#funcion de inicializacion  del server (es invocada desde el script server principal)
#parametros: id: texto que es utilizado como identificador en la llamada a la funcion
#            df: data frame que es la fuente principal de datos a trabajar con la funcion
#            p: provincia elegida para filtrar la seleccion
init_server <- function(id, df, p, i18n) {
  callModule(server, id, df, p, i18n)
}


#funcion server del modulo: es pasado por parametro en la funcion de inicializacion
server <- function(input, output, session, df, p, i18n) {
  #definicion del namespace del lado del server para que el modulo funcione correctamente
  ns <- session$ns
  #observer que dispara la ventana modal cuando el usuario hace clic en el boton maximizar
  observeEvent(input$show_disp_modal, {
    showModal(
      modalDialog(
        fullscreen_this(plotOutput(session$ns("disp_modal"), width = "1280", height = "720px"))  %>% withSpinner(color= consts$loading_color, image = consts$loading_image2),
        easyClose = F,
        footer = tagList(
          modalButton(i18n()$t("86"))
        )
      )
    )
  })
  
  
  plotTitle <- reactive({
    t <- i18n()$t("341")
  })
  
  output$chart_type <- renderText({
    t <- i18n()$t("81")
  })
  
  #se encarga de mostrar el titulo en la cabecera de la ventana
  output$tituloCabecera <- renderText({
    if(p() == "[Todas]"){
      paste0(i18n()$t("80"), plotTitle())
    }else{
      paste0(p(), ": ", plotTitle())
    }
  })
  
  #observer que realiza las traducciones
  observeEvent(c(df, i18n()), {
    sel <- getTranslationArray(selecciones, i18n())
    updateRadioGroupButtons(session,
                            "selected_type",
                            selected = sel[[1]],
                            choices = sel
    )
  })
  
  # observeEvent(list(input$selected_type), {
  #   toggleDropdownButton(inputId = "mydropdown")
  # }, ignoreInit = T)
  
  #data frame a utilizar para el armado de los graficos: ya ha sido filtrado por la seleccion del usuario
  dfr <- reactive({
    if(p() == "[Todas]"){
      data <- df
    }else{
      data <- filter(df, provincia == p())
    }
  })

  #titulo de la cabecera del panel donde se mostraran los graficos
  # output$tituloCabecera <- renderText({
  #   paste0("Historico de fallecidos según edad (", input$selected_type, ")")
  # })
  
  #funcion que renderiza los graficos segun la seleccion del usuario: se encarga del grafico en el panel
  output$disp_dashboard <- renderPlot({

    if (input$selected_type %in% "Histograma 2D") {

      ggplot(dfr(), aes(x = date, y = edad)) +
        ylab(i18n()$t("24")) +
        geom_bin2d(bins = 110) +
        scale_fill_viridis_c() +
        scale_x_date(date_breaks = "3 months",          # Date labels for each month.
                     minor_breaks = NULL,              # No additional labels in-between `date_breaks`.
                     expand = expansion(add = 15),  # Add 15 days to the x-axis on the left and on the right.
                     labels = consts$format_dates) +
        scale_y_continuous(breaks = seq(0, 100, 10)) +
        theme_bw(base_family = font) +
        theme_bw() +
        theme(axis.text.x=element_text(angle=0, hjust=1, size = 10),
              axis.text.y=element_text(size =10),
              strip.placement = "outside",
              axis.title.x=element_blank(),
              legend.key.width=grid::unit(0.3, "cm")
        ) 
      
    } else if(input$selected_type %in% "Densidad 2D"){
      
      ggplot(dfr(), aes(x = date, y = edad)) +
        ylab(i18n()$t("24")) +
        stat_density_2d(aes(fill = after_stat(level)), geom = 'polygon', contour_var = "count", bins = 7) +
        scale_fill_viridis_c() +
        scale_x_date(date_breaks = "3 months",          # Date labels for each month.
                     minor_breaks = NULL,              # No additional labels in-between `date_breaks`.
                     expand = expansion(add = 15),  # Add 15 days to the x-axis on the left and on the right.
                     labels = consts$format_dates) +
        #geom_point(shape = '.') +
        scale_y_continuous(breaks = seq(0, 100, 10)) +
        theme_bw(base_family = font) +
        theme_bw() +
        theme(axis.text.x=element_text(angle=0, hjust=1, size = 10),
              axis.text.y=element_text(size =10),
              strip.placement = "outside",
              axis.title.x=element_blank(),
              legend.key.width=grid::unit(0.3, "cm")
        ) 
      
    } else if(input$selected_type %in% "M. de calor"){
      
      ggplot(dfr(), aes(x = date, y = edad)) +
        ylab(i18n()$t("24")) +
        stat_density_2d(
          geom = "raster",
          aes(fill = after_stat(density)),
          contour = FALSE
        ) + 
        scale_fill_viridis_c() +
        scale_x_date(date_breaks = "3 months",          # Date labels for each month.
                     minor_breaks = NULL,              # No additional labels in-between `date_breaks`.
                     expand = expansion(add = 15),  # Add 15 days to the x-axis on the left and on the right.
                     labels = consts$format_dates) +
        scale_y_continuous(breaks = seq(0, 100, 10)) +
        theme_bw(base_family = font) +
        theme_bw() +
        theme(axis.text.x=element_text(angle=0, hjust=1, size = 10),
              axis.text.y=element_text(size =10),
              strip.placement = "outside",
              axis.title.x=element_blank(),
              legend.key.width=grid::unit(0.3, "cm")
        ) 
      
    } else if(input$selected_type %in% "Dispersión"){
      
      ggplot(dfr(), aes(x = date, y = edad)) +
        ylab(i18n()$t("24")) +
        geom_point(size = 1, colour = "#3f1d5c", alpha = 0.3) +
        scale_x_date(date_breaks = "3 months",          # Date labels for each month.
                     minor_breaks = NULL,              # No additional labels in-between `date_breaks`.
                     expand = expansion(add = 15),  # Add 15 days to the x-axis on the left and on the right.
                     labels = consts$format_dates) +
        scale_y_continuous(breaks = seq(0, 100, 10)) +
        theme_bw(base_family = font) +
        theme_bw() +
        theme(axis.text.x=element_text(angle=0, hjust=1, size = 10),
              axis.text.y=element_text(size =10),
              strip.placement = "outside",
              axis.title.x=element_blank()
        ) 
      
    }
    
  })
  
  #funcion que renderiza los graficos segun la seleccion del usuario: se encarga del grafico maximizado
  output$disp_modal <- renderPlot({
    
    if (input$selected_type %in% "Histograma 2D") {
      
      ggplot(dfr(), aes(x = date, y = edad)) +
        ylab(i18n()$t("24")) +
        geom_bin2d(bins = 110) +
        scale_fill_viridis_c() +
        scale_x_date(date_breaks = "1 month",          # Date labels for each month.
                     minor_breaks = NULL,              # No additional labels in-between `date_breaks`.
                     expand = expansion(add = 15),  # Add 15 days to the x-axis on the left and on the right.
                     labels = consts$format_dates) +
        scale_y_continuous(breaks = seq(0, 105, 5)) +
        theme_bw(base_family = font) +
        theme_bw() +
        theme(axis.text.x=element_text(angle=0, hjust=1, size = 10),
              axis.text.y=element_text(size =12),
              strip.placement = "outside",
              axis.title.x=element_blank()
        )  +
        labs(title = i18n()$t("341"))
      
      
      
    } else if(input$selected_type %in% "Densidad 2D"){
      
      ggplot(dfr(), aes(x = date, y = edad)) +
        ylab(i18n()$t("24")) +
        geom_point(shape = '.') +
        stat_density_2d(aes(fill = after_stat(level)), geom = 'polygon', contour_var = "count", bins = 7) +
        #scale_fill_viridis_c(name = "Densidad") +
        scale_fill_viridis_c() +
        scale_x_date(date_breaks = "1 month",          # Date labels for each month.
                     minor_breaks = NULL,              # No additional labels in-between `date_breaks`.
                     expand = expansion(add = 15),  # Add 15 days to the x-axis on the left and on the right.
                     labels = consts$format_dates) +
        scale_y_continuous(breaks = seq(0, 105, 5)) +
        theme_bw(base_family = font) +
        theme_bw() +
        theme(axis.text.x=element_text(angle=0, hjust=1, size = 10),
              axis.text.y=element_text(size =12),
              strip.placement = "outside",
              axis.title.x=element_blank()
        )  +
        labs(title = i18n()$t("341"))
      
    } else if(input$selected_type %in% "M. de calor"){
      
      ggplot(dfr(), aes(x = date, y = edad)) +
        ylab(i18n()$t("24")) +
        stat_density_2d(
          geom = "raster",
          aes(fill = after_stat(density)),
          contour = FALSE
        ) + 
        scale_fill_viridis_c() +
        scale_x_date(date_breaks = "1 month",          # Date labels for each month.
                     minor_breaks = NULL,              # No additional labels in-between `date_breaks`.
                     expand = expansion(add = 15),  # Add 15 days to the x-axis on the left and on the right.
                     labels = consts$format_dates) +
        scale_y_continuous(breaks = seq(0, 105, 5)) +
        theme_bw(base_family = font) +
        theme_bw() +
        theme(axis.text.x=element_text(angle=0, hjust=1, size = 10),
              axis.text.y=element_text(size =12),
              strip.placement = "outside",
              axis.title.x=element_blank()
        )  +
        labs(title = i18n()$t("341"))
      
    } else if(input$selected_type %in% "Dispersión"){
      
      ggplot(dfr(), aes(x = date, y = edad)) +
        ylab(i18n()$t("24")) +
        geom_point(size = 3, colour = "#3f1d5c", alpha = 0.3) +
        scale_x_date(date_breaks = "1 month",          # Date labels for each month.
                     minor_breaks = NULL,              # No additional labels in-between `date_breaks`.
                     expand = expansion(add = 15),  # Add 15 days to the x-axis on the left and on the right.
                     labels = consts$format_dates) +
        scale_y_continuous(breaks = seq(0, 105, 5)) +
        theme_bw(base_family = font) +
        theme_bw() +
        theme(axis.text.x=element_text(angle=0, hjust=1, size = 10),
              axis.text.y=element_text(size =12),
              strip.placement = "outside",
              axis.title.x=element_blank()
        )  +
        labs(title = i18n()$t("341"))
      
    }
    
  })

  
}