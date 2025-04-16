
#importo bibliotecas
import("shiny")
import("shinyWidgets")
import("dplyr")
import("DT")
import("utils")
import("lubridate")
import("shinycssloaders")
import("writexl")

#expongo funciones a otros script
export("ui")
export("init_server")

#declaro objeto constantes para utilizar el script
consts <- use("constants.R")


ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "panel-header breakdown-header",
      div(class = "item tituloC",
          #"Visor del dataset"
          textOutput(ns("panel_title")),
          ),
      #downloadButton(outputId =  ns('downloadData'), label = textOutput(ns("download_label")))
      downloadButton(outputId =  ns('downloadData'), label = "")
    ),
    div(
      class = "chart-breakdown-container",
      style = "font-size:80%",
      DTOutput(ns("tbl_dashboard"), width = "100%",  height = "496px") %>% withSpinner(
        color = consts$loading_color,
        image = consts$loading_image2
      ),
      includeScript(consts$script_bloquear_selecciones)
    ),
    
  )
  
  
  
}

#df: lista de datasets a mostrar
#ds: identificador del dataset seleccionado
init_server <- function(id, df, ds, i18n) {
  callModule(server, id, df, ds, i18n)
}



server <- function(input, output, session, df, ds, i18n) {
  ns <- session$ns
  
  #filtra el dataset seleccionado de la lista por id
  dfr <- reactive({
    df[[ds()]]
    
  })
  
  #funcion para exportar el dataset a xlsx
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(ds(), Sys.time(), '.xlsx')
    },
    
    content = function(file) {
      write_xlsx(dfr(), file)
    }
  )
  
  output$panel_title <- renderText({
    t <- i18n()$t("512")
  })
  
  # output$download_label <- renderText({
  #   t <- i18n()$t("513")
  # })
  
  #proceso que renderiza la tabla
  output$tbl_dashboard <- renderDT({
    aux <- dfr() %>%
      mutate(across(everything(), as.character))
    
    datatable(
      aux,
      extensions = c('Buttons', "FixedColumns", "FixedHeader", "Scroller"),
      options = list(
        dom = 'Bfrtip',
        lengthMenu = list(c(5, 15,-1), c('5', '15', 'All')),
        pageLength = 14,
        buttons = list(
          list(
            extend = "collection",
            text = paste0("14 ", i18n()$t("521")),
            action = DT::JS(
              "function ( e, dt, node, config ) {
                                    dt.page.len(14);
                                    dt.ajax.reload();
                                }"
            )
          ),
          list(
            extend = "collection",
            text = paste0("50 ", i18n()$t("521")),
            action = DT::JS(
              "function ( e, dt, node, config ) {
                                    dt.page.len(50);
                                    dt.ajax.reload();
                                }"
            )
          ),
          list(
            extend = "collection",
            text = i18n()$t("522"),
            action = DT::JS(
              "function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }"
            )
          ),
          "excel",
          "csv",
          "pdf"
        )
      ),
      filter = list(position = 'top', clear = FALSE),
      class = "display nowrap compact"
    )
    
  })
  
}