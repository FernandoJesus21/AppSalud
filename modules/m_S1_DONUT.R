# Ggplot horizontal bar chart module
#https://community.rstudio.com/t/applying-multiple-filters-for-a-reactive-table/4932
import("shiny")
import("dplyr")
import("shinyWidgets")
import("echarts4r")
import("DT")
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
font <- "Roboto"


#Funcion encargada de transformar el DF que recibe:
# - Selecciona el top X de provincias/deptos segun la medida proporcionada
# - al resto de las pcias/deptos los agrupa en una unica categoria: 'otros'
# el DF transformado por esta funcion es utilizado para el grafico de dona, de modo que siempre se muestre
#  las pcias/deptos mas relevantes segun la medida seleccionada, dejando al resto de
#  las mismas en la categoria 'otros'
# rankValue: numero entero que indica la cantidad de puestos que tendrá el ranking
# df: DF a trasnformar
# featureName: si es provincia o departamento
# selected_pcia: seleccion de provincia realizada por el usuario
buildDF <- function(rankValue, df, featureName, selected_pcia){
  
  aux <- arrange(df, desc(valor))
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
      setnames(old = "v", new = featureName)
    if(featureName == "departamento"){
      aux$provincia <- selected_pcia
    }else{
      aux$departamento <- "[Todos]"
    }
    top <- top %>%
      rbind(aux)
  }else{
    return(df)
  }
  return(top)
  
}

selecciones <- getMetricsChoices(c("C", "CF", "V"), consts$lista_de_medidas, "measure_label")



ui <- function(id) {
  ns <- NS(id)
  
  
  tagList(
      div(
        class = "panel-header breakdown-header",
        div(class = "item tituloC",
            textOutput(ns("tituloCabecera")
        ),    
      ),
      actionButton(ns("show_pie_chart_modal"), "", icon("window-maximize")),
      dropdownButton(
        #tags$h5(i18n$t("38")),
        tags$h5(textOutput(ns("measure_title"))),
        radioGroupButtons(
          inputId = ns("swap_plot"),
          choices = selecciones,
          selected = selecciones[[1]]
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
      echarts4rOutput(ns("pie_chart_dashboard"), width = "100%", height = "400px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2)
    )
  )
}

init_server <- function(id, df, y, m, p, i18n, l) {
  callModule(server, id, df, y, m, p, i18n, l)
}



server <- function(input, output, session, df, y, m, p, i18n, l) {

  #definicion del namespace del lado del server para que el modulo funcione correctamente
  ns <- session$ns
  
  plotTitle <- reactive({
    t1 = ""
    if(p() == "[Todas]"){
      t1 = "S1_DON_chart_province_title"
    }else{
      t1 = "S1_DON_chart_department_title"
    }
    t <- i18n()$t(consts$obtenerAttr_por_id(input$swap_plot, t1))
  })
  
  output$measure_title <- renderText({
    t <- i18n()$t("50")
  })
  
  #se encarga de mostrar el titulo en la cabecera de la ventana
  output$tituloCabecera <- renderText({
    plotTitle()
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
  
  
  #logica condicional del modulo
  # segun la seleccion del usuario, se encarga de hacer los filtros necesarios al df
  selDF <- reactive({
    
    df %>%
      filter(
        consts$conditional(input$swap_plot != "", c1 == input$swap_plot), #asigno el df de confirmados/fallecidos segun haya elegido el usuario
        consts$conditional(m() == "0", year(date) == y() & c2 == "A"), #en caso de que el usuario eligiera "todos los meses", filtro los registros con valores acumulados anuales
        consts$conditional(m() != "0", year(date) == y() & month(date) == m() & c2 == "M"), #si el usuario eligio un mes, filtro por ese mes
        consts$conditional(p() == "[Todas]", provincia %in% consts$listaProvincias & departamento == "[Todos]"), #si se eligio "todas las provincias", quito aquellos casos sin clasificar
        consts$conditional(p() == "[Desconocido]", provincia == "*sin dato*" & departamento != "[Todos]"), #si se eligio "desconocido" quito todos los casos clasificados
        consts$conditional(p() %in% consts$listaProvincias, provincia == p() & departamento != "[Todos]"), #si la seleccion del usuario esta contenida en la lista de las provincias, filtro por esa seleccion
      )
    
  })
  
  #llamada a funcion para crear el ranking que posteriormente se utilizara para graficar
  donutDF <- reactive({
    
    if(p() == "[Todas]"){
      buildDF(6, selDF(), "provincia", p())
    }else{
      buildDF(6, selDF(), "departamento", p())
    }
  })
  
  #obtengo la paleta de colores a utilizar segun sea la medida seleccionada
  paleta_de_colores <- reactive({
    
    colores <- c(consts$lista_de_medidas[[input$swap_plot]]$palet, "#9E9FA0")
    
  })
  
  #observer que proporciona la funcionalidad de la ventana modal
  observeEvent(input$show_pie_chart_modal, {
    showModal(
      modalDialog(
        echarts4rOutput(session$ns("pie_chart_modal"), width = "1280", height = "640px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2),
        easyClose = F,
        footer = tagList(
          modalButton(i18n()$t("86"))
        )
      )
    )
  })

  #logica para graficar en el panel del dashboard
  output$pie_chart_dashboard <- renderEcharts4r({

    if(nrow(donutDF()) != 0){
      
      if(p() != "[Todas]"){
        
        donutDF() |>
          e_charts(departamento) |>
          e_pie(valor, radius = c("40%", "65%"), center = c("50%", "43%")) |>
          e_labels(show = TRUE,
                   formatter = "{c} \n {d}%",
                   position = "outside",
                   #fontFamily = font,
                   fontSize = 12,
                   alignTo = "edge",
                   edgeDistance = "10"
                   ) |>
          e_legend(bottom = "0") |>
          e_tooltip(
            formatter = htmlwidgets::JS(paste0("
                function(params){
                    return( 'depto: ' + '<strong>' + params.name + '</strong>' + '<br />", i18n()$t("84"), " ' + params.percent + '%')
                }
            "))
          ) |>
          e_color(color = paleta_de_colores())
        
      }else{
          
        donutDF() |>
          e_charts(provincia) |>
          e_pie(valor, radius = c("40%", "65%"), center = c("50%", "43%")) |>
          e_labels(show = TRUE,
                   formatter = "{c} \n {d}%",
                   position = "outside",
                   #fontFamily = font,
                   fontSize = 12,
                   alignTo = "edge",
                   edgeDistance = "10"
                   ) |>
          e_legend(bottom = "0") |>
          e_tooltip(
            formatter = htmlwidgets::JS(paste0("
                function(params){
                    return( '", i18n()$t("23"), ": ' + '<strong>' + params.name + '</strong>' + '<br />", i18n()$t("84"), " ' + params.percent + '%')
                }
            "))
          ) |>
          e_color(color = paleta_de_colores())
      }

    }else{
      liquid <- data.frame(val = c(0, 0, 0))
      cat("Se ha intentado graficar [PIE_CHART] sin datos.\n")

      liquid |>
        e_charts() |>
        e_liquid(val) |>
        e_title(i18n()$t("83"))
    }



  })

  #logica para graficar en la ventana modal
  output$pie_chart_modal <- renderEcharts4r({

    if(nrow(donutDF()) != 0){
      
      if(p() != "[Todas]"){
        
        donutDF() |>
          e_charts(departamento) |>
          e_pie(valor, radius = c("45%", "70%")) |>
          e_title(plotTitle()) |>
          e_labels(show = TRUE,
                   formatter = "{c} \n ({d}%)",
                   position = "outside",
                   #fontFamily = font,
                   fontSize = 15,
                   alignTo = "edge",
                   edgeDistance = "200"
                   ) |>
          e_legend(right = 0) |>
          e_tooltip(
            formatter = htmlwidgets::JS(paste0("
                function(params){
                    return( 'depto: ' + '<strong>' + params.name + '</strong>' + '<br />", i18n()$t("84"), " ' + params.percent + '%')
                }
            "))
          )|>
          e_color(color = paleta_de_colores())
        
      }else{
        
        donutDF() |>
          e_charts(provincia) |>
          e_pie(valor, radius = c("45%", "70%")) |>
          e_title(plotTitle()) |>
          e_labels(show = T,
                   formatter = "{c} \n ({d}%)",
                   position = "outside",
                   #fontFamily = font,
                   fontSize = 15,
                   alignTo = "edge",
                   edgeDistance = "200"
                   ) |>
          e_legend(right = 0) |>
          e_tooltip(
            formatter = htmlwidgets::JS(paste0("
                function(params){
                    return( '", i18n()$t("23"), ": ' + '<strong>' + params.name + '</strong>' + '<br />", i18n()$t("84"), " ' + params.percent + '%')
                }
            "))
          )|>
          e_color(color = paleta_de_colores())

      }
      

      
    }else{
      liquid <- data.frame(val = c(0, 0, 0))
      cat("Se ha intentado graficar [PIE_CHART @modal] sin datos.\n")
      
      liquid |> 
        e_charts() |> 
        e_liquid(val) |>
        e_title(i18n()$t("83")) 
    }
    
  })
  
}