

options(scipen = 12)


library(shiny)
library(data.table)
library(DT)
library(ggplot2)
library(plotly)
library(lubridate)
library(readr)
library(dplyr)
library(modules)
library(sass)
library(glue)
library(ggfortify)
library(echarts4r)
library(dygraphs)
library(shinyWidgets)
library(shinyjs)
library(scales)
library(shinyfullscreen)
library(shinycssloaders)
library(leaflet)
library(fst)
library(xts)
library(geojsonio)
library(formattable)
library(forecast)
library(gfonts)
library(writexl)
library(stats)
library(shiny.i18n)

translator <- Translator$new(translation_json_path='lang/translation.json')

#CFG <- read_csv("config.csv", col_types = cols(value = col_character()))

#custom font (only works in shinyapps.io)
dir.create('~/.fonts')
file.copy("www/fonts/RobotoCondensed-Regular.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

#fecha de recarga de datos
df_reload <- read.fst("www/data/fst/reload_date.fst")
reload_date <- df_reload$V_data_last_update

#fecha de inicio que influira en los periodos disponibles en las selecciones
data_first_day <- "2020-03-01" %>% as.Date()
data_last_day <- as.Date(paste0(year(df_reload$etl_date), "-", month(df_reload$etl_date), "-01"))

#lista de dataframes
lista_datasets <- list()

source("utilities/getExternalLink.R")


#Function compiling sass files to one css file
sass(
  sass_file("styles/main.scss"),
  output = "www/main.css",
  options = sass_options(output_style = "compressed"),
  cache = NULL
)

#Lectura de datos

#DFP grafico de dona S1
lista_datasets[["DFP_DONA_S1"]] <-
  read.fst("www/data/fst/DFP_DONA_S1.fst", as.data.table = T)
#DFP indicadores S1
lista_datasets[["DFP_KPI_S1"]] <-
  read.fst("www/data/fst/DFP_KPI_S1.fst", as.data.table = T)
#DFP series temporales de la S1
lista_datasets[["DFP_ST_S1"]] <-
  read.fst("www/data/fst/DFP_ST_S1.fst", as.data.table = T)


#DFP mapa S2
lista_datasets[["DFP_MAP_S2"]] <-
  read.fst("www/data/fst/DFP_MAP_S2.fst", as.data.table = T) %>%
  mutate(idp = as.numeric(idp)) %>%
  mutate(idg = as.numeric(idg))
#DFP grafico de barra S2
lista_datasets[["DFP_RE_S2"]] <-
  read.fst("www/data/fst/DFP_RE_S2.fst", as.data.table = T) %>%
  mutate(rango_etario = factor(
    rango_etario,
    levels = c(
      "<12",
      "12-17",
      "18-29",
      "30-39",
      "40-49",
      "50-59",
      "60-69",
      "70-79",
      "80-89",
      "90-99",
      ">=100",
      "*na*"
    )
  ))

#DFP grafico dispersion y variantes S3
lista_datasets[["DFP_CF_U"]] <-
  read.fst("www/data/fst/DFP_CF_U.fst", as.data.table = T)
#DFP grafico animado de S3
lista_datasets[["DFP_CF_GAN_S3"]] <-
  read.fst("www/data/fst/DFP_CF_GAN_S3.fst", as.data.table = T) %>%
  mutate(VCF = round((V / (CF + 1)), 2)) %>%
  filter(provincia != "*na*") %>%
  filter(departamento != "*na*")
#DFP grafico mapa de calor S3
lista_datasets[["DFP_HM_S3"]] <-
  read.fst("www/data/fst/DFP_HM_S3.fst", as.data.table = T) %>%
  mutate(aplicaciones = valor) %>%
  mutate(valor = ifelse(valor == 0, 1, valor))



# if (CFG[CFG$variable == "TEST_MODE", ]$value == "T") {
#   source("app_tests.R")
#   
# }



#Archivo de poligonos para el mapa
pcias <-
  geojson_read("www/data/geojson/poly_provincias.json", what = "sp")
deptos <-
  geojson_read("www/data/geojson/poly_departamentos.json", what = "sp")
deptos@data$in1 <- as.numeric(deptos@data$in1)

#Archivo de creditos y componentes de terceros
about <-
  read_csv("www/data/csv/about.csv", show_col_types = FALSE) %>%
  mutate(enlaces = paste0("<a href='", enlaces, "'target='_blank'>", enlaces, "</a>"))



# Constantes
consts <- use("constants.R")

# Carga de modulos:

m_S1_KPI <- use("./modules/m_S1_KPI.R")
m_S1_DONUT <- use("./modules/m_S1_DONUT.R")
m_S1_TIME_SERIES <- use("./modules/m_S1_TIME_SERIES.R")
m_S2_AGE_RANGE <- use("./modules/m_S2_AGE_RANGE.R")
m_S2_GEO_MAP <- use("./modules/m_S2_GEO_MAP.R")
m_S2_PARETO <- use("./modules/m_S2_PARETO.R")
m_S3_ANIMATION <- use("./modules/m_S3_ANIMATION.R")
m_S3_SCATTER <- use("./modules/m_S3_SCATTER.R")
m_S3_HEATMAP <- use("./modules/m_S3_HEATMAP.R")
m_S4_FORECASTING <- use("./modules/m_S4_FORECASTING.R")
m_S5_DATASET <- use("./modules/m_S5_DATASET.R")
m_S6_ABOUT <- use("./modules/m_S6_ABOUT.R")

# Indice de los DFPs
indice_datasets <- names(lista_datasets)

#fecha de la version de la aplicacion
version_date <- df_reload$etl_date












