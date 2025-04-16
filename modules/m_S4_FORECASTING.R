#importo bibliotecas
import("shiny")
import("shinyWidgets")
import("dplyr")
import("DT")
import("utils")
import("lubridate")
import("shinycssloaders")
import("writexl")
import("dygraphs")
import("xts")
import("forecast")
import("data.table")

options(scipen = 12)
#expongo funciones a otros script
export("ui")
export("init_server")

#declaro objeto constantes para utilizar el script
consts <- use("constants.R")
expose("utilities/getMetricsChoices.R")
expose("utilities/getTranslationArray.R")

#selecciones posibles de la lista de medidas
selecciones <- getMetricsChoices(c("C", "CF"), consts$lista_de_medidas, "measure_label")

#################################################################################
# Funciones de respaldo
################################################################################

#funcion para generar el df que va a ser enviado a la funcion de graficado
getDygraphsDF <- function(data, model, forecast_df, last_selected_date=NULL, pred_rows, test_rows){
  #sin importar de que modelo haya venido el forecast el formato de graficado debe ser unico
  # El df de salida es alterado segun la ultima fecha seleccionada por el usuario.
  #forecast_df debe incluir las filas de testing+ las de prediccion
  df <- filter(data, date <= last_selected_date) #se filtra segun la fecha ultima
  totalRows <- nrow(df) + pred_rows - test_rows #cantidad total de filas posibles
  trainRows <- totalRows - pred_rows #cantidad de filas del conjunto de entrenamiento
  test <- data.frame("test"=forecast_df[1:test_rows,]) #df solo de fechas que van en el conjunto de testing
  pred <- data.frame("pred"=forecast_df[(test_rows+1):pred_rows,]) #df de fechas a predecir
  df$real[(trainRows+1):(totalRows-pred_rows+test_rows)] <- NA #anulo los valores reales cuyas fechas estan dentro del conjunto de testing
  df$test <- NA #creo fila de testing
  df$test[(trainRows+1):(totalRows-pred_rows+test_rows)] <- test$test #agrego los valores de testing conforme a las fechas correspondientes
  #agrego filas adicionales para contemplar las n predicciones. A su vez tambien genero las fechas a predecir
  df <- rbind(df, data.frame(date=seq(df$date[nrow(df)]+days(1), (df$date[nrow(df)]+days(pred_rows-test_rows)), "day"), real=NA, test=NA))
  df$pred <- NA #creo fila de predicciones
  df$pred[((totalRows-pred_rows+test_rows+1):totalRows)] <- pred$pred #agrego los valores de las predicciones conforme a las fechas correspondientes
  return(df)
}

#funcion personalizada para el boton de descarga
customDownloadbutton <- function(outputId, label = "Download"){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("file"), label)
}

#funcion personalizada para crear el modelo
fitModel <- function(model_type, time_series){
  model <- NA
  if(model_type == "ARIMA"){
    model <- auto.arima(time_series, lambda = "auto", stationary = F)
  }
  if(model_type == "ETS"){
    model <- ets(time_series, lambda = "auto")
  }
  if(model_type == "NAIVE"){
    model <- "NAIVE"
  }
  if(model_type == "AVERAGE"){
    model <- "AVERAGE"
  }
  return(model)
}

#funcion personalizada para aplicar la prevision
customForecast <- function(model, training_set=NULL, last_selected_date=NULL, h){
  #para todos los modelos se genera el mismo formato de df para el forecasting.
  #el df consistirá de una unica columna con los valores predichos desde el dia siguiente
  # al ultimo dia disponible en el conjunt de entrenamiento hasta 40 dias en el futuro
  df <- NA
  #si el modelo es naive o average, su tratamiento es distinto ya que su calculo es explicito (no depende de ninguna funcion de forecasting dedicada)
  if(is.character(model)){
    if(model == "NAIVE"){
      df <- data.frame("pred" = rep(training_set[date == (last_selected_date-days(30)), ]$real, h))
      return(df)
    }
    if(model == "AVERAGE"){
      value <- mean(filter(training_set, training_set$date %in% seq((last_selected_date-days(59)),(last_selected_date-days(30)), "day"))$real)
      df <- data.frame("pred"= rep(value, h))
      return(df)
    }
  }
  #si el modelo no es ninguno de los anteriores se asume que puede ser ARIMA o ETS. La funcion debe recibir el objeto 'modelo' generado por su
  # funcion dedicada. A continuación se usa este objeto modelo para hacer la predicción. A diferencia de los otros modelos, el calculo no es explicito.
  #finalmente se castea a df el objeto forecast y se modifica para que su formato coincida con el definido.
  df <- as.data.frame(forecast(model,h=h)) %>% 
    setnames(old = "Point Forecast", new = "pred") %>%
    select(pred)
  return(df)
}

getModelParameters <- function(model){
  #funcion para obtener los parametros principales de los modelos
  # ARIMA y ETS
  if(any(class(model) == "ets")){
    return(model$method)
  }
  if(any(class(model) == "ARIMA", na.rm = T)){
    return(paste0("ARIMA(", paste0(unname(arimaorder(model)), c(",", ",", ""), collapse = " "), ")"))
  }
  return("-")
}

#################################################################################
# Funcion UI
################################################################################

ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "panel-header breakdown-header",
      div(
        class = "item tituloC",
        textOutput(ns("tituloCabecera"))
      ),
      downloadButton(outputId =  ns('downloadData'), label = ""),
      dropdownButton(
        tags$h5(textOutput(ns("measure_title"))),
        radioGroupButtons(
          inputId = ns("swap_plot"),
          choices = selecciones,
          selected = selecciones[[1]]
        ),
        circle = TRUE,
        icon = icon("chart-simple"),
        inline = T,
        inputId = ns("mydropdown")
      ),
      customDownloadbutton(outputId = ns("report"), label = "")
    ),
    div(
      class = "chart-breakdown-container",
      style = "font-size:80%;",
      dygraphOutput(ns("fgraph"), height = "496px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2)
      #textOutput(ns("fgraph"))
      #DTOutput(ns("fgraph"))
    ),
    includeScript(consts$script_bloquear_selecciones)
  )
  
  
  
}

# funcion para invocar el modulo desde server.R
init_server <- function(id, df, r, a, p, y, i18n) {
  callModule(server, id, df, r, a, p, y, i18n)
}

#################################################################################
# Funcion server
################################################################################

server <- function(input, output, session, df, r, a, p, y, i18n) {
  ns <- session$ns
  
  #VARIABLES REACTIVAS
  tooFewRowsSelectedErr <- reactiveVal() #flag para determinar si el usuario selecciono menos de 90 filas
  tooFewRowsSelectedErr(F)
  selModel <- reactiveVal() #modelo elegido
  selEst <- reactiveVal() #estimacion en base al modelo elegido
  
  #funcion para exportar el dataset a xlsx
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("test", Sys.time(), '.xlsx')
    },
    
    content = function(file) {
      write_xlsx(plotDF(), file)
    }
  )
  
  #funcion para generar el reporte html
  output$report <- downloadHandler(
    filename = function() {
      paste("report", Sys.time(), '.html')
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      params <- list(
                     ts = selData(),
                     nombre_modelo = a(),
                     plotDF = plotDF(), 
                     modelo = selModel(),
                     selRMSE = selRMSE(),
                     medida = names(which(selecciones == input$swap_plot)),
                     lang = i18n()
                     )
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  #funcion traductora del titulo del panel
  output$tituloCabecera <- renderText({
    i18n()$t("412")
  })
  
  #funcion traductora del titulo del selector de visualizacion de medidas
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
  
  
  #funcion filtradora principal del data frame recibido
  selDF <- reactive({
    df %>%
      dplyr::filter(
        consts$conditional(input$swap_plot != "", c1 == consts$lista_de_medidas[[input$swap_plot]]$id), #asigno el df de confirmados/fallecidos segun haya elegido el usuario
        consts$conditional(p() == "[Todas]", provincia == p()), #si se eligio "todas las provincias", quito aquellos casos sin clasificar
        consts$conditional(p() %in% consts$listaProvincias, provincia == p()), #si la seleccion del usuario esta contenida en la lista de las provincias, filtro por esa seleccion
        consts$conditional(year(format(r(), "%Y-%m-%d")[1]) != "", as.numeric(year(date)) >= year(format(r(), "%Y-%m-%d")[1])),
        consts$conditional(year(format(r(), "%Y-%m-%d")[2]) != "", as.numeric(year(date)) <= year(format(r(), "%Y-%m-%d")[2])),
        consts$conditional(month(format(r(), "%Y-%m-%d")[1]) != "", as.numeric(date) >= r()[1]),
        consts$conditional(month(format(r(), "%Y-%m-%d")[2]) != "", as.numeric(date) <= r()[2])
      )
  })
  
  
  #verifica si el usuario selecciona menos de 90 dias, afecta la flag tooFewRowsSelectedErr
  observeEvent(c(selDF(), a()),{
    if(nrow(selDF()) < 90){
      tooFewRowsSelectedErr(T)
    }else{
      tooFewRowsSelectedErr(F)
    }
  })
  
  #si ocurre que tooFewRowsSelectedErr=T se reemplaza la seleccion del usuario con un df con datos mock
  selData <- reactive({
    if(!tooFewRowsSelectedErr()){
      data <- select(selDF(), date, valor) %>%
        filter(date <= format(r(), "%Y-%m-%d")[2])
      setnames(data, old = "valor", new = "real")
    }else{
      data <- data.frame("date" = seq(as.Date("2000-01-01"), by = "day", length.out = 90), "valor" = 0)
    }
  })
  
  #df del conjunto de entrenamiento
  selTrain <- reactive({
    train <- selData()[1:(nrow(selData())-30),]
  })
  
  #df del conjunto de test
  selTest <- reactive({
    test <- selData()[(nrow(selData())-29):nrow(selData()),]
  })
  
  #st del conjunto de entrenamiento
  selTSTrain <- reactive({
    st_train <- xts(selTrain()$real, order.by = selTrain()$date)
  })
  
  #actualiza el modelo segun la seleccion del usuario
  observeEvent(c(a(), selData()),{
    selModel(fitModel(a(), selTSTrain()))
  })
  
  #actualiza la prevision segun la seleccion del modelo del usuario
  observeEvent(c(selModel(), selData()), {
    selEst(customForecast(model=selModel(), training_set=selData(), last_selected_date=as.Date(format(r(), "%Y-%m-%d")[2]), h=40))
  })
  
  #df con los valores reales y los predichos
  selPred <- reactive({
    est <- data.frame("pred" = selEst()[1:30,]) %>%
      cbind(selTest())
  })
  
  #generacion del RMSE
  selRMSE <- reactive({
    rmse <- sqrt(mean((selPred()$real - selPred()$pred)^2))
  })
  
  #df usado para graficar
  plotDF <- reactive({
    getDygraphsDF(data=selData(), model=a(), forecast_df=selEst(), last_selected_date=as.Date(format(r(), "%Y-%m-%d")[2]), pred_rows=40, test_rows=30)
  })
  
  # output$fgraph <- renderDT({
  #   getDygraphsDF(data=selData(), model=a(), forecast_df=selEst(), last_selected_date=as.Date(format(r(), "%Y-%m-%d")[2]), pred_rows=40, test_rows=30)
  #   #selData()
  # })
  
  # output$fgraph <- renderText({
  #   selRMSE()
  # })
  
  #generacion del grafico de serie temporal
  output$fgraph <- renderDygraph({

    if(tooFewRowsSelectedErr()){
      dygraph(selData(), main = i18n()$t("491"))
    }else{
      dygraph(plotDF(), 
              main = paste0(i18n()$t(consts$lista_de_medidas[[input$swap_plot]]$S4_ST_forecasting_main_title), 
                            "<br> <small>", i18n()$t("415"), 
                            "<br> RMSE: <strong>", round(selRMSE(), 2) ,"</strong></small> 
                             <br> <small> Params: <strong>", getModelParameters(selModel()), "</strong></small>")) %>%
        dyRangeSelector()
    }
  })
  

  
}