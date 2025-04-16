# Ggplot horizontal bar chart module

import("shiny")
import("shinyWidgets")
import("dplyr")
import("DT")
import("utils")
import("lubridate")
import("shinycssloaders")
import("shiny.i18n")

export("ui")
export("init_server")


consts <- use("constants.R")

expose("utilities/getTranslationArray.R")

tableLabels1 <- c("recurso", "versión", "licencia", "enlaces")
names(tableLabels1) <- c("615", "619", "618", "617")


tableLabels2 <- c("recurso", "detalles", "enlaces")
names(tableLabels2) <- c("615", "616", "617")

ui <- function(id, i18n) {
  ns <- NS(id)
  usei18n(i18n)
  div(class = "tabset-info",
  
  tabsetPanel(type = "tabs",
              tabPanel(
                textOutput(ns("SS_tab_1")),
                div(class = "info-project-text",
                    textOutput(ns("SS_par_1")),
                    br(),
                    textOutput(ns("SS_par_2")),
                    br(),
                    strong(textOutput(ns("SS_par_3"))),
                    br(),
                    textOutput(ns("SS_par_4")), a("<AppSalud_ETL_Framework>", href="https://github.com/FernandoJesus21/AppSalud_ETL_Framework", target="_blank"),
                    br(),
                    textOutput(ns("SS_par_51")), a("<AppSalud_Forecasting>", href="https://github.com/FernandoJesus21/AppSalud_Forecasting", target="_blank"),
                    br(),
                    textOutput(ns("SS_par_5")), a("<AppSalud_app>", href="https://github.com/FernandoJesus21/AppSalud", target="_blank"),
                    br(),
                    div(class = "shapes",
                        HTML(
                          "<img src='assets/ilustracion_03.png' style='width:573px;height:198px;border-radius: 0px 0px 10px 0px;'>"),
                    )
                    )         
              ),
              tabPanel(
                textOutput(ns("SS_tab_2")),
                       div(class = "info-resource-text",
                           textOutput(ns("SS_par_6"))
                       ),
                       div(
                         class = "chart-breakdown-container", style = "font-size:90%" ,
                         DTOutput(ns("st_tbl_datos"), width = "100%", height = "100px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2)
                       )
              ),
              tabPanel(
                textOutput(ns("SS_tab_3")),
                div( class = "info-resource-text",
                  textOutput(ns("SS_par_7"))
                ),
                div(
                    class = "chart-breakdown-container", style = "font-size:90%" ,
                    DTOutput(ns("st_dashboard"), width = "100%", height = "100px") %>% withSpinner(color=consts$loading_color, image = consts$loading_image2)
                    )
              )
  )
  )
  
}

init_server <- function(id, df, i18n) {
  callModule(server, id, df, i18n)
}



server <- function(input, output, session, df, i18n) {
  
  ns <- session$ns
  
  output$SS_tab_1 <- renderText({
    i18n()$t("620")
  })
  
  output$SS_tab_2 <- renderText({
    i18n()$t("630")
  })
  
  output$SS_tab_3 <- renderText({
    i18n()$t("640")
  })
  
  output$SS_tab_4 <- renderText({
    i18n()$t("650")
  })
  
  output$SS_par_1 <- renderText({
    i18n()$t("621")
  })
  
  output$SS_par_2 <- renderText({
    i18n()$t("622")
  })
  
  output$SS_par_3 <- renderText({
    i18n()$t("623")
  })
  
  output$SS_par_4 <- renderText({
    i18n()$t("624")
  })
  
  output$SS_par_5 <- renderText({
    i18n()$t("625")
  })
  
  output$SS_par_51 <- renderText({
    i18n()$t("626")
  })
  
  output$SS_par_6 <- renderText({
    i18n()$t("631")
  })
  
  output$SS_par_7 <- renderText({
    i18n()$t("641")
  })
  
  labelTranslateTable1 <- reactive({
    t <- names(getTranslationArray(tableLabels1, i18n()))
  })
  
  labelTranslateTable2 <- reactive({
    t <- names(getTranslationArray(tableLabels2, i18n()))
  })
  
  output$st_dashboard = renderDT({
    
    aux <- df %>%
      filter(tipo != "datos") %>%
      select(recurso, version, licencia, enlaces)
      
    
    datatable(
      aux, colnames = labelTranslateTable1(), filter = 'none', rownames = F,
      options = list(
        columnDefs = list(list(className = 'dt-left', targets = c(0, 1,2, 3))),
        ordering=F,
        autoWidth = TRUE,
        scrollX = TRUE,
        pageLength = 13,
        lengthChange = F,
        searching = F
      ),
      escape = FALSE,
      selection = "none",
      class = "display nowrap compact"
      
    )
  
    }
  )
  
  
  output$st_tbl_datos = renderDT({
    
    aux <- df %>%
      filter(tipo == "datos") %>%
      select(recurso, detalles, enlaces)
    
    
    datatable(
      aux, colnames = labelTranslateTable2(), filter = 'none', rownames = F,
      options = list(
        columnDefs = list(list(className = 'dt-left', targets = c(0, 1, 2))),
        ordering=F,
        autoWidth = TRUE,
        scrollX = TRUE,
        pageLength = 13,
        lengthChange = F,
        searching = F
      ),
      escape = FALSE,
      selection = "none",
      class = "display nowrap compact"
      
    )
    
  }
  )
  

}