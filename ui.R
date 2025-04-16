


# carga de constantes
consts <- use("constants.R")

# carga de funciones de utilidad
source("utilities/getMetricsChoices.R")
source("utilities/getTimeFilterChoices.R")
source("utilities/getProvinceChoices.R")
source("utilities/getTranslationArray.R")


#translator$set_translation_language('es')

## ui.R ##


ui <- fixedPage(
  #usei18n(translator),
  #useShinyjs(),
  #includeScript(consts$script_animar_botones), #carga de script (js)
  tags$div(
      id = "lang",
      pickerInput(inputId = "selected_language",
                  label = "",
                  choices = consts$flags$val,
                  choicesOpt = list(content = consts$flags$img)),
      
      ),
    

    div(
      class = "tabset-general",

      tabsetPanel(
        type = "tabs",

        ##############################################################################
        # Seccion resumen (S1)
        ##############################################################################
        
        tabPanel(
          #"Resumen",
          textOutput("S1_app_tab_title"),
          icon = icon("heart-pulse"),
          htmlTemplate(
            "www/resumen.html",
            appTitle = textOutput("S1_app_title"),
            appVersion = consts$app_version,
            dashboardLogo = consts$shinyLogo,
            selectYear = selectInput(
              "selected_year",
              #"Año",
              label = textOutput("S1_year_label"),
              choices = getYearChoices(data_first_day, data_last_day),
              selected = "2021",
              selectize = TRUE
            ),
            selectMonth = selectInput(
              "selected_month",
              #"Mes",
              label = textOutput("S1_month_label"),
              choices = getMonthsChoices(year = NULL, data_last_day, data_first_day),
              selected = "0",
              selectize = TRUE
            ),
            selectPcia = selectInput(
              "selected_pcia",
              #"Provincia",
              label = textOutput("S1_province_label"),
              #choices = getProvinceChoices(), #solucion 1
              choices = c("[Todas]", consts$listaProvincias2), #solucion 2
              selected = "[Todas]",
              selectize = TRUE
            ),
            
            kpi_1 = m_S1_KPI$ui("C"),
            kpi_2 = m_S1_KPI$ui("CF"),
            kpi_3 = m_S1_KPI$ui("V"),
            kpi_4 = m_S1_KPI$ui("VesqC"),
            pie_chart = m_S1_DONUT$ui("pie_chart"),
            stc = m_S1_TIME_SERIES$ui("series_covid"),
            stf = m_S1_TIME_SERIES$ui("series_vacunados"),
            texto_footer = consts$texto_footer
          )
        ),
        
        ##############################################################################
        # Seccion mapas (S2)
        ##############################################################################
        
        tabPanel(
          #"Mapas",
          textOutput("S2_app_tab_title"),
          icon = icon("map"),
          htmlTemplate(
            "www/mapas.html",
            appTitle = textOutput("S2_app_title"),
            appVersion = consts$app_version,
            dashboardLogo = consts$shinyLogo,
            selectYear = selectInput(
              "selected_year_S2",
              #"Año",
              label = textOutput("S2_year_label"),
              choices = getYearChoices(data_first_day, data_last_day),
              selected = "2021",
              selectize = TRUE
            ),
            selectMonth = selectInput(
              "selected_month_S2",
              #"Mes",
              label = textOutput("S2_month_label"),
              choices = getMonthsChoices(year = NULL, data_last_day, data_first_day),
              selected = "0",
              selectize = TRUE
            ),
            selectPcia = selectInput(
              "selected_pcia_S2",
              #"Provincia",
              label = textOutput("S2_province_label"),
              choices = c("[Todas]", consts$listaProvincias2),
              selected = "[Todas]",
              selectize = TRUE
            ),
            mapa = m_S2_GEO_MAP$ui("mapa"),
            pareto = m_S2_PARETO$ui("pareto"),
            rangoEtario = m_S2_AGE_RANGE$ui("rango_etario"),
            texto_footer = consts$texto_footer
          )
          
        ),
        
        ##############################################################################
        # Seccion otros graficos (S3)
        ##############################################################################
        
        tabPanel(
          #"Otros gráficos",
          textOutput("S3_app_tab_title"),
          icon = icon("chart-column"),
          htmlTemplate(
            "www/otros_graficos.html",
            appTitle = textOutput("S3_app_title"),
            appVersion = consts$app_version,
            dashboardLogo = consts$shinyLogo,
            selectPcia = selectInput(
              "selected_pcia_S3",
              #"Provincia",
              label = textOutput("S3_province_label"),
              choices = c("[Todas]", consts$listaProvincias2),
              selected = "Buenos Aires",
              selectize = TRUE
            ),
            ganimado = m_S3_ANIMATION$ui("grafico_animado"),
            dispersion = m_S3_SCATTER$ui("dispersion"),
            mcalor = m_S3_HEATMAP$ui("heatmap"),
            texto_footer = consts$texto_footer
          )

        ),
        
        ##############################################################################
        # Seccion forecasting (S4)
        ##############################################################################
        
        tabPanel(
          #"Forecasting",
          textOutput("S4_app_tab_title"),
          icon = icon("gauge"),
          htmlTemplate(
            "www/forecasting.html",
            appTitle = textOutput("S4_app_title"),
            appVersion = consts$app_version,
            dashboardLogo = consts$shinyLogo,
            provincia = selectInput(
              "selected_pcia_S4",
              #"Provincia",
              label = textOutput("S4_province_label"),
              choices = c("[Todas]", consts$listaProvincias2),
              selected = "[Todas]",
              selectize = TRUE
            ),
            fechas = dateRangeInput(
              "selected_range_S4",
              label = textOutput("S4_range_label"),
              start  = "2020-03-01",
              end    = reload_date-days(1),
              min    = "2020-03-01",
              max    = reload_date-days(1),
              format = "yy/mm/dd",
              separator = " - ",
              language = "es",
              autoclose = T,
              width = "245px"
            ),
            algoritmo = selectInput(
              "selected_algoritmo_S4",
              #"Algoritmo",
              label = textOutput("S4_algorithm_label"),
              choices = c("NAIVE", "AVERAGE", "ETS", "ARIMA"),
              selected = "ARIMA",
              selectize = TRUE
            ),
            fgraph = m_S4_FORECASTING$ui("fgraph"),
            texto_footer = consts$texto_footer
          )
          
        ),
        
        ##############################################################################
        # Seccion dataset (S5)
        ##############################################################################
        
        tabPanel(
          #"Datasets",
          textOutput("S5_app_tab_title"),
          icon = icon("table-list"),
          htmlTemplate(
            "www/datasets.html",
            #appTitle = consts$app_title_STBL,
            appTitle = textOutput("S5_app_title"),
            appVersion = consts$app_version,
            dashboardLogo = consts$shinyLogo,
            dataset_selection = selectInput(
              "dataset_selection",
              #"dataset",
              label = textOutput("S5_dataset_label"),
              choices = indice_datasets,
              selectize = TRUE
            ),
            tbl = m_S5_DATASET$ui("tbl"),
            texto_footer = consts$texto_footer
          )

        ),
        
        ##############################################################################
        # Seccion acerca de (S6)
        ##############################################################################

        tabPanel(
          #"Acerca de...",
          textOutput("S6_app_tab_title"),
          icon = icon("circle-info"),
          htmlTemplate(
            "www/info.html",
            appTitle = textOutput("S6_app_title"),
            appVersion = consts$app_version,
            dashboardLogo = consts$shinyLogo,
            infoIcon = consts$infoIcon,
            colorTile = div(
              class = "logo-info",
              HTML(
                "<img src='assets/logo_appSalud_nobg2.png' alt='logo' style='width:220px;height:120px;'>"
              ),
              div(
                class = "logo-desc",
                strong(textOutput("S6_version_title")), consts$app_version,
                br(),
                strong(textOutput("S6_reload_date_title")), version_date,
                br(),
                strong(textOutput("S6_build_date_title")), reload_date,
                br()
              ),
              div(class = "pal-info",)

            ),
            mainTile = m_S6_ABOUT$ui("tbl_info", translator),
            texto_footer = consts$texto_footer
          )
        )
        
      )
    )
)
