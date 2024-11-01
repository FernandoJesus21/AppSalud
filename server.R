

shinyServer(function(input, output, session) {
  
  #Elementos en comun
  # estos elementos se usan en varias secciones de la aplicacion
  
  #obtiene el lenguaje seleccionado por el usuario
  selected_language <- reactive({
    input$selected_language
  })
  
  #objeto reactivo que cambia el lenguaje de la app segun el valor de <selected_language>
  i18n <- reactive({
    selected <- selected_language()
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    translator
  })

  
  ##############################################################################
  # Seccion resumen (S1)
  ##############################################################################
  
  #funciones de traduccion de las seccion, utilizan el objeto reactivo <i18n()> 
  #para consultar la taduccion que debe mostrar segun el lenguaje activo
  
  output$S1_app_tab_title <- renderText({
    i18n()$t("100")
  })
  
  output$S1_app_title <- renderText({
    i18n()$t("101")
  })
  
  
  output$S1_year_label <- renderText({
    i18n()$t("21")
  })
  
  output$S1_month_label <- renderText({
    i18n()$t("22")
  })
  
  
  output$S1_province_label <- renderText({
    
    i18n()$t("23")
  })
  
  #Getters para las dimensiones ubicadas en la cabecera de la seccion
  # el cambio en su valor afecta a varios elementos.
  
  #periodo anual seleccionado
  selected_year <- reactive({
    input$selected_year
  })
  #periodo mensual seleccionado
  selected_month <- reactive({
    input$selected_month
  })
  #provincia seleccionada
  selected_pcia <- reactive({
    input$selected_pcia
  })
  
  
  #Observadores de eventos: se encargan de actualizar las traducciones en los selectores de dimensiones
  # tambien actualizan los elementos disponibles a ser seleccionados segun la seleccion hecha en un selector previo
  
  #define la logica de los selectores de los meses segun el año seleccionado:
  # Si se selecciona "todos los meses" el valor retornado es 0, de lo contrario,
  #  retorna en numero correspondiente a ese mes.
  # Esta logica tambien evita que se seleccione un mes y año inexistente, actualizando
  #  las selecciones de meses disponibles segun el año seleccionado.
  observeEvent(c(input$selected_year, input$selected_language), {
    months_choices <-
      getMonthsChoices(input$selected_year,
                       data_last_day,
                       data_first_day
    )
    selected_month <-
      ifelse(input$selected_month %in% months_choices,
             input$selected_month,
             "0")
    months_choices <- getTranslationArray(months_choices, i18n())
    
    updateSelectInput(session,
                      "selected_month",
                      selected = selected_month,
                      choices = months_choices)
  })
  
  #observer para las provincias
  observeEvent(c(input$selected_pcia, input$selected_language), {
    a <- "[Todas]"
    names(a) <- i18n()$t("14")
    selected_province <- ifelse(input$selected_pcia %in% consts$listaProvincias2, input$selected_pcia, a)
    updateSelectInput(session,
                      "selected_pcia",
                      selected = selected_province,
                      choices = c(a, consts$listaProvincias2))
  })
  

  
  #funciones de llamado para elementos graficos
  # estas funciones invocan a los modulos de la aplicacion, pasando los datos necesarios 
  # para su ejecucion

  #grafico de dona
  m_S1_DONUT$init_server(
    "pie_chart",
    df = lista_datasets[["DFP_DONA_S1"]],
    y = selected_year,
    m = selected_month,
    p = selected_pcia,
    i18n = i18n,
    l = selected_language
  )

  #Serie temporal (casos confirmados y fallecidos)
  m_S1_TIME_SERIES$init_server(
    "series_covid",
    df = filter(lista_datasets[["DFP_ST_S1"]], c1 %in% c("C", "CF", "C_AC", "CF_AC")),
    y = selected_year,
    m = selected_month,
    p = selected_pcia,
    i18n = i18n
  )

  #Serie temporal (vacunaciones)
  m_S1_TIME_SERIES$init_server(
    "series_vacunados",
    df = filter(lista_datasets[["DFP_ST_S1"]], c1 %in% c("V", "V_AC")),
    y = selected_year,
    m = selected_month,
    p = selected_pcia,
    i18n = i18n
  )

  #Indicador (casos confirmados)
  m_S1_KPI$init_server(
    "C",
    df = filter(lista_datasets[["DFP_KPI_S1"]], c1 == "C"),
    y = selected_year,
    m = selected_month,
    p = selected_pcia,
    i = "icons/001-prueba-de-covid.png",
    i18n = i18n
  )

  #indicador (casos confirmados fallecidos)
  m_S1_KPI$init_server(
    "CF",
    df = filter(lista_datasets[["DFP_KPI_S1"]], c1 == "CF"),
    y = selected_year,
    m = selected_month,
    p = selected_pcia,
    i = "icons/warning.png",
    i18n = i18n
  )

  #indicador (vacunaciones)
  m_S1_KPI$init_server(
    "V",
    df = filter(lista_datasets[["DFP_KPI_S1"]], c1 == "V"),
    y = selected_year,
    m = selected_month,
    p = selected_pcia,
    i = "icons/injection.png",
    i18n = i18n
  )

  #indicador (vacunados con el esquema primario completo)
  m_S1_KPI$init_server(
    "VesqC",
    df = filter(lista_datasets[["DFP_KPI_S1"]], c1 == "VesqC"),
    y = selected_year,
    m = selected_month,
    p = selected_pcia,
    i = "icons/vaccine-record.png",
    i18n = i18n
  )
  
  ##############################################################################
  # Seccion mapas (S2)
  ##############################################################################
  
  #funciones de traduccion de las seccion, utilizan el objeto reactivo <i18n()> 
  #para consultar la taduccion que debe mostrar segun el lenguaje activo
  
  output$S2_app_tab_title <- renderText({
    i18n()$t("200")
  })
  
  output$S2_app_title <- renderText({
    i18n()$t("201")
  })
  
  output$S2_year_label <- renderText({
    i18n()$t("21")
  })
  
  output$S2_month_label <- renderText({
    i18n()$t("22")
  })

  output$S2_province_label <- renderText({
    i18n()$t("23")
  })

  #Getters para las dimensiones ubicadas en la cabecera de la seccion
  # el cambio en su valor afecta a varios elementos.
  
  #periodo anual seleccionado
  selected_year_S2 <- reactive({
    input$selected_year_S2
  })
  #periodo mensual seleccionado
  selected_month_S2 <- reactive({
    input$selected_month_S2
  })
  #medida seleccionada
  selected_measure <- reactive({
    input$selected_measure
  })
  #pronvicia seleccionada
  selected_pcia_S2 <- reactive({
    input$selected_pcia_S2
  })
  
  #Observadores de eventos: se encargan de actualizar las traducciones en los selectores de dimensiones
  # tambien actualizan los elementos disponibles a ser seleccionados segun la seleccion hecha en un selector previo
  
  
  #define la logica de los selectores de los meses segun el año seleccionado:
  # Si se selecciona "todos los meses" el valor retornado es 0, de lo contrario,
  #  retorna en numero correspondiente a ese mes.
  # Esta logica tambien evita que se seleccione un mes y año inexistente, actualizando
  #  las selecciones de meses disponibles segun el año seleccionado.
  observeEvent(c(input$selected_year_S2, input$selected_language), {
    months_choices <-
      getMonthsChoices(input$selected_year_S2,
                       data_last_day,
                       data_first_day)
    selected_month_S2 <-
      ifelse(input$selected_month_S2 %in% months_choices,
             input$selected_month_S2,
             "0")
    months_choices <- getTranslationArray(months_choices, i18n())
    updateSelectInput(session,
                      "selected_month_S2",
                      selected = selected_month_S2,
                      choices = months_choices)
  })
  
  #observer para las provincias
  observeEvent(c(input$selected_pcia_S2, input$selected_language), {
    a <- "[Todas]"
    names(a) <- i18n()$t("14")
    selected_province <- ifelse(input$selected_pcia_S2 %in% consts$listaProvincias2, input$selected_pcia_S2, a)
    updateSelectInput(session,
                      "selected_pcia_S2",
                      selected = selected_province,
                      choices = c(a, consts$listaProvincias2))
  })

  #funciones de llamado para elementos graficos
  # estas funciones invocan a los modulos de la aplicacion, pasando los datos necesarios 
  # para su ejecucion
  
  #mapa de calor geografico
  m_S2_GEO_MAP$init_server(
    "mapa",
    df = lista_datasets[["DFP_MAP_S2"]],
    sp_pcias = pcias,
    sp_deptos = deptos,
    y = selected_year_S2,
    m = selected_month_S2,
    p = selected_pcia_S2,
    i18n = i18n,
    l = selected_language
  )
  #grafico pareto
  m_S2_PARETO$init_server(
    "pareto",
    df = lista_datasets[["DFP_MAP_S2"]],
    y = selected_year_S2,
    m = selected_month_S2,
    p = selected_pcia_S2,
    i18n = i18n,
    l = selected_language
  )
  # #grafico de barras
  m_S2_AGE_RANGE$init_server(
    "rango_etario",
    df = lista_datasets[["DFP_RE_S2"]],
    y = selected_year_S2,
    m = selected_month_S2,
    p = selected_pcia_S2,
    i18n = i18n,
    l = selected_language
  )
  
  ##############################################################################
  # Seccion otros graficos (S3)
  ##############################################################################
  
  #funciones de traduccion de las seccion, utilizan el objeto reactivo <i18n()> 
  #para consultar la taduccion que debe mostrar segun el lenguaje activo
  
  output$S3_app_tab_title <- renderText({
    i18n()$t("300")
  })
  
  output$S3_app_title <- renderText({
    i18n()$t("301")
  })

  output$S3_province_label <- renderText({
    i18n()$t("23")
  })
  
  #Getters para las dimensiones ubicadas en la cabecera de la seccion
  # el cambio en su valor afecta a varios elementos.
  
  # #provincia seleccionada
  selected_pcia_S3 <- reactive({
    input$selected_pcia_S3
  })
  
  #Observadores de eventos: se encargan de actualizar las traducciones en los selectores de dimensiones
  # tambien actualizan los elementos disponibles a ser seleccionados segun la seleccion hecha en un selector previo
  
  #observer para las provincias 
  observeEvent(c(input$selected_pcia_S3, input$selected_language), {
    a <- "[Todas]"
    names(a) <- i18n()$t("14")
    selected_province <- ifelse(input$selected_pcia_S3 %in% consts$listaProvincias2, input$selected_pcia_S3, a)
    updateSelectInput(session,
                      "selected_pcia_S3",
                      selected = selected_province,
                      choices = c(a, consts$listaProvincias2))
  })
  
  #funciones de llamado para elementos graficos
  # estas funciones invocan a los modulos de la aplicacion, pasando los datos necesarios 
  # para su ejecucion
  
  #grafico animado
  m_S3_ANIMATION$init_server("grafico_animado",
                            df = lista_datasets[["DFP_CF_GAN_S3"]],
                            p = selected_pcia_S3,
                            i18n = i18n
                            )
  

  #grafico de dispersion y variantes
  m_S3_SCATTER$init_server("dispersion",
                        df = lista_datasets[["DFP_CF_U"]],
                        p = selected_pcia_S3,
                        i18n = i18n
                        )

  # #grafico de mapa de calor
  m_S3_HEATMAP$init_server("heatmap",
                           df = lista_datasets[["DFP_HM_S3"]],
                           p = selected_pcia_S3,
                           i18n = i18n
                           )


  
  
  ##############################################################################
  # Seccion forecasting (S4)
  ##############################################################################
  
  #funciones de traduccion de las seccion, utilizan el objeto reactivo <i18n()> 
  #para consultar la taduccion que debe mostrar segun el lenguaje activo
  
  output$S4_app_tab_title <- renderText({
    i18n()$t("400")
  })
  
  output$S4_app_title <- renderText({
    i18n()$t("401")
  })
  
  output$S4_measure_label <- renderText({
    i18n()$t("33")
  })
  
  output$S4_province_label <- renderText({
    i18n()$t("23")
  })
  
  output$S4_algorithm_label <- renderText({
    i18n()$t("31")
  })
  
  output$S4_range_label <- renderText({
    i18n()$t("32")
  })
  
  #Getters para las dimensiones ubicadas en la cabecera de la seccion
  # el cambio en su valor afecta a varios elementos.
  
  selected_range_S4 <- reactive({
    input$selected_range_S4
  })

  selected_algoritmo_S4 <- reactive({
    input$selected_algoritmo_S4
  })
  
  #pronvicia seleccionada
  selected_pcia_S4 <- reactive({
    input$selected_pcia_S4
  })
  
  #Observadores de eventos: se encargan de actualizar las traducciones en los selectores de dimensiones
  # tambien actualizan los elementos disponibles a ser seleccionados segun la seleccion hecha en un selector previo
  
  #observer que realiza las traducciones
  observeEvent(c(input$selected_measure_S4, input$selected_language), {
    sel <- getMetricsChoices(c("C", "CF", "C_AC", "CF_AC"), consts$lista_de_medidas, "measure_label")
    sel <- getTranslationArray(sel, i18n())
    selected_measure <- input$selected_measure_S4
    updateSelectInput(session,
                      "selected_measure_S4",
                      selected = selected_measure,
                      choices = sel
    )
  })
  
  #observer para las provincias
  observeEvent(c(input$selected_pcia_S4, input$selected_language), {
    a <- "[Todas]"
    names(a) <- i18n()$t("14")
    selected_province <- ifelse(input$selected_pcia_S4 %in% consts$listaProvincias2, input$selected_pcia_S4, a)
    updateSelectInput(session,
                      "selected_pcia_S4",
                      selected = selected_province,
                      choices = c(a, consts$listaProvincias2))
  })

  #funciones de llamado para elementos graficos
  # estas funciones invocan a los modulos de la aplicacion, pasando los datos necesarios 
  # para su ejecucion
  
  m_S4_FORECASTING$init_server("fgraph",
                               df = lista_datasets[["DFP_ST_S1"]],
                               r = selected_range_S4,
                               a = selected_algoritmo_S4,
                               p = selected_pcia_S4,
                               y = selected_year_S4,
                               i18n = i18n
  )
  
  ##############################################################################
  # Seccion datasets (S5)
  ##############################################################################
  
  #Getters para las dimensiones ubicadas en la cabecera de la seccion
  # el cambio en su valor afecta a varios elementos.
  
  #dataset seleccionado
  selected_dataset <- reactive({
    input$dataset_selection
  })
  
  
  #funciones de traduccion de las seccion, utilizan el objeto reactivo <i18n()> 
  #para consultar la taduccion que debe mostrar segun el lenguaje activo
  
  output$S5_app_tab_title <- renderText({
    i18n()$t("500")
  })
  
  output$S5_app_title <- renderText({
    i18n()$t("501")
  })
  
  output$S5_dataset_label <- renderText({
    i18n()$t("511")
  })

  #funciones de llamado para elementos graficos
  # estas funciones invocan a los modulos de la aplicacion, pasando los datos necesarios 
  # para su ejecucion
  
  #tabla que muestra el dataset
  m_S5_DATASET$init_server("tbl",
                     df = lista_datasets,
                     ds = selected_dataset,
                     i18n = i18n
                     )

  
  ##############################################################################
  # Seccion acerca de (S6)
  ##############################################################################
  
  #funciones de traduccion de las seccion, utilizan el objeto reactivo <i18n()> 
  #para consultar la taduccion que debe mostrar segun el lenguaje activo
  
  output$S6_app_tab_title <- renderText({
    i18n()$t("600")
  })
  
  output$S6_app_title <- renderText({
    i18n()$t("601")
  })
  
  output$S6_version_title <- renderText({
    i18n()$t("611")
  })
  
  output$S6_build_date_title <- renderText({
    i18n()$t("613")
  })
  
  output$S6_reload_date_title <- renderText({
    i18n()$t("614")
  })
  
  #funciones de llamado para elementos graficos
  # estas funciones invocan a los modulos de la aplicacion, pasando los datos necesarios 
  # para su ejecucion
  
  m_S6_ABOUT$init_server("tbl_info",
                       df = about,
                       i18n = i18n
                       )
  

  
  ##############################################################################
  #FIN
  
})
