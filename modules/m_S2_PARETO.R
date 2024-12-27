# Ggplot horizontal bar chart module
#https://community.rstudio.com/t/applying-multiple-filters-for-a-reactive-table/4932
import("shiny")
import("dplyr")
import("shinyWidgets")
import("echarts4r")
#import("DT")
import("utils")
import("lubridate")
import("shinycssloaders")
import("data.table")
import("shiny.i18n")

export("ui")
export("init_server")


consts <- use("constants.R")
expose("utilities/getMetricsChoices.R")
expose("utilities/getTranslationArray.R")

#medidas disponibles para el grafico pareto
selecciones_pareto <- getMetricsChoices(c("C", "CF", "V"), consts$lista_de_medidas, "measure_label")

#esta funcion construye el df que se utilizara para crear el pareto:
# consiste en construir un ranking de las categorias mas relevantes segun la medida seleccionada, unificando las demas en una sola categoria
buildRankDF <- function(rankValue, df, featureName, selected_pcia, colorMedida){
  
  aux <- arrange(df, desc(valor)) %>%
    mutate(color = colorMedida)
  if(nrow(aux) > rankValue){
    top <- head(aux, rankValue)
    aux <- filter(aux, !(aux[[featureName]] %in% top[[featureName]])) %>%
      group_by(c1, c2) %>%
      summarise(valor = sum(valor),
                date = max(date),
                .groups = "drop") %>%
      mutate(v = paste0("El resto de ", featureName, "s")) %>%
      mutate(idp = "0") %>%
      mutate(idg = "0") %>%
      setnames(old = "v", new = featureName) %>%
      mutate(color = "#aaa")
    if(featureName == "departamento"){
      aux$provincia <- selected_pcia
    }else{
      aux$departamento <- "[Todos]"
    }
    top <- top %>%
      rbind(aux)
  }else{
    df <- df %>%
      arrange(desc(valor)) %>%
      mutate(color = colorMedida)
    return(df)
  }
  return(top)
  
}


ui <- function(id) {
  ns <- NS(id)
  
  #obtengo la lista de todas las medidas
  selecciones <- c("1", "2")
  
  tagList(
      div(
        class = "panel-header breakdown-header",
        div(class = "item tituloC",
            textOutput(ns("tituloCabecera")
        ),    
      ),
      actionButton(ns("show_pie_chart_modal"), "", icon("window-maximize")),
      dropdownButton(
        #tags$h5("Medidas:"),
        tags$h5(textOutput(ns("measure_title"))),
        radioGroupButtons(
          inputId = ns("swap_plot"),
          choices = selecciones_pareto
          #selected = selecciones_pareto[[1]]
          #por defecto el menu mostrara todas las medidas antes de ser actualizado con las medidas permitidas por la funcion observer
        ),
        circle = TRUE,
        icon = icon("chart-simple"),
        inline = T,
        inputId = ns("mydropdown")
      ),
      #includeScript(consts$script_pantalla_completa),
      includeScript(consts$script_bloquear_selecciones)
    ),
    div(
      class = "chart-breakdown-container",
      echarts4rOutput(ns("pareto_dashboard"), width = "100%", height = "300px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2)
      #DTOutput(ns("tbl"), width = "100%", height = "400px")%>% withSpinner(color=consts$loading_color, image = consts$loading_image2)
      
    )
  )
}

init_server <- function(id, df, y, m, p, i18n, l) {
  callModule(server, id, df, y, m, p, i18n, l)
}



server <- function(input, output, session, df, y, m, p, i18n, l) {
  
  #al iniciar la aplicacion, y luego de que el usuario pulse alguna opcion del menu, este se cierra (deprecated)
  # automaticamente
  # observeEvent(list(input$swap_plot), {
  #   toggleDropdownButton(inputId = "mydropdown")
  # }, ignoreInit = consts$triggerMenuDropdown)

  
  #observer que actualiza la lista de medidas proporcionada en un comienzo, dejando disponible 
  # solo las medias permitidas
  # las medidas permitidas son aquellas que estan presentes en la columna c1 del df proporcionado
  
  #observer que realiza las traducciones
  observeEvent(c(df, i18n()), {
    sel <- getTranslationArray(selecciones_pareto, i18n())
    updateRadioGroupButtons(session,
                            "swap_plot",
                            selected = sel[[1]],
                            choices = sel
    )
  })
  
  
  #observer que proporciona la funcionalidad de la ventana modal
  observeEvent(input$show_pie_chart_modal, {
    showModal(
      modalDialog(
        #DTOutput(ns("tbl"), width = "100%", height = "400px")%>% withSpinner(color=consts$loading_color, image = consts$loading_image2),
        echarts4rOutput(session$ns("pareto_modal"), width = "1280", height = "640px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2),
        easyClose = F,
        footer = tagList(
          modalButton(i18n()$t("86"))
        )
      )
    )
  })
  
  
  #logica condicional del modulo
  # segun la seleccion del usuario, se encarga de hacer los filtros necesarios al df
  selDF <- reactive({
    
    df %>%
      filter(
        consts$conditional(input$swap_plot != "", c1 == input$swap_plot), #asigno el df de confirmados/fallecidos segun haya elegido el usuario
        consts$conditional(m() == "0", year(date) == y() & c2 == "A"), #en caso de que el usuario eligiera "todos los meses", filtro los registros con valores acumulados anuales
        consts$conditional(m() != "0", year(date) == y() & month(date) == m() & c2 == "M"), #si el usuario eligio un mes, filtro por ese mes
        consts$conditional(p() == "[Todas]", provincia %in% consts$listaProvincias & departamento == "[Todos]"), #si se eligio "todas las provincias", quito aquellos casos sin clasificar
        #consts$conditional(p() == "[Desconocido]", provincia == "*sin dato*" & departamento != "[Todos]"), #si se eligio "desconocido" quito todos los casos clasificados
        consts$conditional(p() %in% consts$listaProvincias, provincia == p() & departamento != "[Todos]"), #si la seleccion del usuario esta contenida en la lista de las provincias, filtro por esa seleccion
      )
    
  })
  
  #el titulo que se encarga de mostrar la funcion de arriba es construido en esta funcion
  plotTitle <- reactive({
    t1 = ""
    if(p() == "[Todas]"){
      t1 = "S2_PAR_chart_province_title"
    }else{
      t1 = "S2_PAR_chart_department_title"
    }
    t <- i18n()$t(consts$obtenerAttr_por_id(input$swap_plot, t1))
  })
  
  output$measure_title <- renderText({
    t <- i18n()$t("50")
  })
  
  #titulo del panel del grafico pareto
  output$tituloCabecera <- renderText({
    plotTitle()
  })

  #logica para graficar en el panel del dashboard
  output$pareto_dashboard <- renderEcharts4r({
    
    
    if(p() == "[Todas]"){
      
      aux <- buildRankDF(15, selDF(), "provincia", p(), consts$lista_de_medidas[[input$swap_plot]]$color)

      aux |>
        #arrange(desc(valor)) |>
        mutate(porc.acumulado = (cumsum(valor)/sum(valor) * 100)) |>
        e_charts(provincia) |>
        e_bar(valor) |>
        e_line(porc.acumulado, y_index = 1) |>
        #e_axis_labels(y = "valor", x = "provincia") |>
        e_tooltip(trigger = "axis")|>
        e_legend(FALSE) |>
        e_add_nested("itemStyle", color) |>
        e_color('#8c8c8c')
      
    }else{
      
      aux <- buildRankDF(15, selDF(), "departamento", p(), consts$lista_de_medidas[[input$swap_plot]]$color)

      aux |>
        #arrange(desc(valor)) |>
        mutate(porc.acumulado = (cumsum(valor)/sum(valor) * 100)) |>
        e_charts(departamento) |>
        e_bar(valor) |>
        e_line(porc.acumulado, y_index = 1) |>
        #e_axis_labels(y = "valor", x = "departamento") |>
        e_tooltip(trigger = "axis")|>
        e_legend(FALSE) |>
        e_add_nested("itemStyle", color) |>
        e_color('#8c8c8c')
        #e_color(color = c(consts$lista_de_medidas[[input$swap_plot]]$color, '#8c8c8c'))
      
    }
    


  })

  #logica para graficar en la ventana modal
  output$pareto_modal <- renderEcharts4r({

    
    if(p() == "[Todas]"){
      
      aux <- buildRankDF(15, selDF(), "provincia", p(), consts$lista_de_medidas[[input$swap_plot]]$color)
      aux |>
        #arrange(desc(valor)) |>
        mutate(porc.acumulado = (cumsum(valor)/sum(valor) * 100)) |>
        e_charts(provincia) |>
        e_bar(valor) |>
        e_line(porc.acumulado, y_index = 1) |>
        #e_axis_labels(y = "valor", x = "provincia") |>
        e_tooltip(trigger = "axis")|>
        e_legend(FALSE) |>
        e_title(plotTitle()) |>
        e_x_axis(axisLabel = list(interval = 0, rotate = 45)) |>
        e_add_nested("itemStyle", color) |>
        e_color('#8c8c8c')
        #e_color(color = c(consts$lista_de_medidas[[input$swap_plot]]$color, '#8c8c8c'))
      
    }else{
      
      aux <- buildRankDF(15, selDF(), "departamento", p(), consts$lista_de_medidas[[input$swap_plot]]$color)
      aux |>
        #arrange(desc(valor)) |>
        mutate(porc.acumulado = (cumsum(valor)/sum(valor) * 100)) |>
        e_charts(departamento) |>
        e_bar(valor) |>
        e_line(porc.acumulado, y_index = 1) |>
        #e_axis_labels(y = "valor", x = "departamento") |>
        e_tooltip(trigger = "axis")|>
        e_legend(FALSE) |>
        e_title(plotTitle()) |>
        e_x_axis(axisLabel = list(interval = 0, rotate = 45)) |>
        e_add_nested("itemStyle", color) |>
        e_color('#8c8c8c')
        #e_color(color = c(consts$lista_de_medidas[[input$swap_plot]]$color, '#8c8c8c'))
      
    }
    
  })
  
}