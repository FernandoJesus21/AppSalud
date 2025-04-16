# Ggplot horizontal bar chart module
#https://stackoverflow.com/questions/70794452/how-to-change-the-type-of-graph-based-off-a-shiny-input

#importar bibliotecas
import("shiny")
import("shinyWidgets")
import("dplyr")
import("DT")
import("leaflet")
import("grDevices")
import("lubridate")
import("shinycssloaders")
import("shiny.i18n")


export("ui")
export("init_server")
expose("utilities/getMetricsChoices.R")
expose("utilities/getTranslationArray.R")

#declaro el objeto constantes para utilizar sus funciones
consts <- use("constants.R")

#Estas medidas son las que estaran disponibles en el mapa
selecciones_mapas <- getMetricsChoices(c("C", "CF", "V", "RM", "RC100", "RCF100"), consts$lista_de_medidas, "measure_label")


#funcion que recibe el spdf(objeto geoespacial), la medida(me) y un flag para indicar si el zoom debe estar activado o no.
#esta funcion se encarga de crear y retornar el objeto de mapa
dibujar_mapa <- function(spdf, me, auto_pos = T){
  
  #creo la paleta de colores
  pal <- colorNumeric(palette = consts$obtenerAttr_por_id(me , "palet2"), domain = spdf@data$valor)
  #creo el formato de la etiqueta flotante (popup)
  lab <- sprintf(
    "<strong>%s</strong><br/>valor: %s</sup>",
    spdf@data$nam, formatC(round(spdf@data$valor, 4), format="fg", big.mark = ",", decimal.mark = ".")
  ) %>% lapply(htmltools::HTML)
  #creo el objeto mapa
  m <- leaflet(spdf) %>% 
    addPolygons(
      fillColor = ~pal(spdf@data$valor) ,
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "white",
        dashArray = "",
        fillOpacity = 1,
        bringToFront = TRUE),
      label = lab,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    leaflet::addLegend(pal = pal, values = spdf@data$valor, opacity = 0.7, title = "", position = "bottomright") %>%
    #leaflet::addLegend(pal = pal, values = spdf@data$valor, opacity = 0.7, title = consts$obtenerAttr_por_id(me, "title"), position = "bottomright") %>%
    addProviderTiles("CartoDB.PositronNoLabels")
  #defino segun el flag si el zoom y la vista personalizada se van a utilizar
  if(auto_pos){
    m <- m %>%
      setView(lat = -38, lng = -64, zoom = 4)
  }
  
  return(m)
}

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
        #tags$h5("Medidas:"),
        tags$h5(textOutput(ns("measure_title"))),
        radioGroupButtons(
          inputId = ns("selected_measure"),
          choices = selecciones_mapas
        ),
        circle = TRUE,
        icon = icon("chart-simple"),
        inline = T,
        inputId = ns("mydropdown")
      ),
      includeScript(consts$script_bloquear_selecciones) #llamada a script adjunto (js)
    ),
    div(
      class = "chart-breakdown-container",
      leafletOutput(outputId = ns("map_dashboard"), width = "100%", height = "496px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2)
      
      ) 
  ) 
}

#funcion de inicializacion  del server (es invocada desde el script server principal)
#parametros: id: texto que es utilizado como identificador en la llamada a la funcion
#            df: data frame que es la fuente principal de datos a trabajar con la funcion
#            sp_pcias: objeto geoespacial de las provincias
#            sp_deptos: objeto geoespacial de los departamentos
#            y: periodo anual elegido para filtrar la seleccion
#            m: mes elegido para filtrar la seleccion
#            p: provincia elegida para filtrar la seleccion
init_server <- function(id, df, sp_pcias, sp_deptos, y, m, p, i18n, l) {
  callModule(server, id, df, sp_pcias, sp_deptos, y, m, p, i18n, l)
}


#funcion server del modulo: es pasado por parametro en la funcion de inicializacion
server <- function(input, output, session, df, sp_pcias, sp_deptos, y, m, p, i18n, l) {
  
  #definicion del namespace del lado del server para que el modulo funcione correctamente
  ns <- session$ns
  
  plotTitle <- reactive({
    t <- i18n()$t(consts$obtenerAttr_por_id(input$selected_measure, "S2_MAP_chart_title"))
  })
  
  output$measure_title <- renderText({
    t <- i18n()$t("50")
  })
  
  
  #titulo de la cabecera del panel donde se mostrara el mapa
  output$tituloCabecera <- renderText({
    plotTitle()
  })
  
  #observer que realiza las traducciones
  observeEvent(c(df, i18n()), {
    sel <- getTranslationArray(selecciones_mapas, i18n())
    updateRadioGroupButtons(session,
                            "selected_measure",
                            selected = sel[[1]],
                            choices = sel
    )
  })
  
  
  #data frame a utilizar para el armado del mapa: ya ha sido filtrado por la seleccion del usuario
  selDF <- reactive({
    
    df %>%
      filter(
        consts$conditional(input$selected_measure != "", c1 == consts$lista_de_medidas[[input$selected_measure]]$id), #asigno el df de confirmados/fallecidos segun haya elegido el usuario
        consts$conditional(m() == "0", year(date) == y() & c2 == "A"), #en caso de que el usuario eligiera "todos los meses", filtro los registros con valores acumulados anuales
        consts$conditional(m() != "0", year(date) == y() & month(date) == m() & c2 == "M") #si el usuario eligio un mes, filtro por ese mes
      )

  })
  
  
  #observer que dispara la ventana modal cuando el usuario hace clic en el boton maximizar
  observeEvent(input$show_disp_modal, {
    showModal(
      modalDialog(
        leafletOutput(ns("map_modal"), width = "1280", height = "720px")  %>% withSpinner(color= consts$loading_color, image = consts$loading_image2),
        easyClose = F,
        footer = tagList(
          modalButton(i18n()$t("86"))
        )
      )
    )
  })
  
  
  #objeto geoespacial a utilizar para el armado del mapa: ya ha sido filtrado por la seleccion del usuario
  selSPDF <- reactive({
    
    aux <- filter(selDF(), departamento != "*sin dato*")
    
    if(p() != "[Todas]"){
      aux <- filter(aux, departamento != "[Todos]")
      spdf <- sp_deptos[sp_deptos@data$provincia == p(),]
      spdf@data <- left_join(spdf@data, aux, by = c("in1" = "idg"))
      
    }else{
      aux <- filter(aux, departamento == "[Todos]")
      spdf <- sp_pcias
      spdf@data <- left_join(spdf@data, aux, by = "idp")
    }
    return(spdf)
  })
  
  #indica segun sea la seleccion del usuario, si el zoom estará activo o no (es el parametro que se le envia a la funcion que crea el mapa)
  enfoqueMapa <- reactive({
    
    if(p() != "[Todas]"){
      z <- F
    }else{
      z <- T
    }
    return(z)
    
  })
  
  #funcion que renderiza el mapa mostrado en el panel del aplicativo
  output$map_dashboard <- renderLeaflet({
    
    if(nrow(selDF()) != 0){
      dibujar_mapa(selSPDF(), input$selected_measure, enfoqueMapa())
    }
    
  })
  
  #funcion que renderiza el mapa maximizado
  output$map_modal <- renderLeaflet({
    
    if(nrow(selDF()) != 0){
      dibujar_mapa(selSPDF(), input$selected_measure, enfoqueMapa())
    }
    
  })

  
}