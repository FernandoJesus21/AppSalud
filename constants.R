

################################################################################
# Archivo constants.R
# Este script contiene variables que no son modificadas a lo largo de la ejecucion
#  del aplicativo
################################################################################


#funcion condicional que simplifica el uso de if anidados
conditional <- function(condition, success) {
  if (condition)
    success
  else
    TRUE
}

#funcion que retorna el atributo seleccionado de todos los elementos de la lista
obtenerAttr_medidas <- function(atr) {
  return(unname(unlist(sapply(lista_de_medidas, function(x)
    x[atr]))))
}

#funcion que retorna el atributo seleccionado de un elemento segun su id
obtenerAttr_por_id <- function(id, atr) {
  aux <-
    names(lista_de_medidas)[sapply(lista_de_medidas, function(x)
      id %in% x)]
  return(unname(unlist(lista_de_medidas[[aux]][atr])))
  
}

#funcion que formatea las fechas, haciendo que cada primer mes disponible del año muestre su año de la siguiente manera:

#jul  ago sep oct nov dic ene feb mar abr
#2020                     2021

format_dates <- function(x) {
  months <- strftime(x, format = "%b")
  years <- lubridate::year(x)
  if_else(
    is.na(lag(years)) | lag(years) != years,
    true = paste(months, years, sep = "\n"),
    false = months
  )
}


#importa bibliotecas a utilizar
import("dplyr")
import("htmltools")
import("grDevices")

#titulos de las secciones de la aplicacion
app_title_RESUMEN <- "Tablero COVID-19"
app_title_S2 <- "Mapas"
app_title_S3 <- "Otros gráficos"
app_title_S4 <- "Forecasting"
app_title_STBL <- "Datasets"
app_title_INFO <- "Acerca de..."

#texto al pie de la aplicacion
texto_footer <- "Fernando J. Heredia | 2024"
#version de la aplicacion
app_version <- "4.5.3"

#fecha de inicio que influira en los periodos disponibles en las selecciones
# data_first_day <- "2020-03-01" %>% as.Date()
# data_last_day <- "2024-10-31" %>% as.Date()

#este logo corresponde al logo de la aplicacion
shinyLogo <- HTML(
  "
   <img src='assets/logo_appSalud_nobg2.png' alt='logo' style='width:111px;height:60px;'>
"
)

#icono de documentacion, se muestra en la seccion INFO
infoIcon <- HTML(
  "
   <a href='man/manual_de_usuario.pdf'  target='_blank'><img src='assets/icons/manual-book.png' alt='logo' style='width:48px;height:48px;'></a>
"
)

#gif y color del icono de carga
loading_image2 <- "assets/icons/Double Ring-1s-200px.gif"
loading_color <- "#15354A"

#inicializacion de los scripts externos
script_bloquear_selecciones <- "www/js/bloquear_selecciones.js"
script_animar_botones <- "www/js/animar_botones.js"
script_pantalla_completa <- "www/js/pantalla_completa.js"


#lista que contiene las variables que se utilizaran en el aplicativo y sus atributos
lista_de_medidas <- list(
  C = list(
    id = "C",
    measure_label = "51",
    S1_KPI_title = "111",
    S1_DON_chart_province_title = "121",
    S1_DON_chart_department_title = "131",
    S1_TS_chart_title = "141",
    S2_MAP_chart_title = "211",
    S2_RE_chart_title = "231",
    S2_PAR_chart_province_title = "241",
    S2_PAR_chart_department_title = "251",
    S4_ST_forecasting_main_title = "413",
    color = "#E56B6F",
    palet = c(
      "#355070",
      "#6D597A",
      "#B56576",
      "#E56B6F",
      "#E88C7D",
      "#EAAC8B"
    ),
    palet2 = colorRampPalette(colors = c("#ffd6d7", "#e85d5f"), space = "Lab")(8)
  ),
  C_AC = list(
    id = "C_AC",
    measure_label = "58",
    S1_TS_chart_title = "142",
    color = "#E56B6F",
    palet = c(
      "#355070",
      "#6D597A",
      "#B56576",
      "#E56B6F",
      "#E88C7D",
      "#EAAC8B"
    ),
    palet2 = colorRampPalette(colors = c("#ffd6d7", "#e85d5f"), space = "Lab")(8)
  ),
  CF = list(
    id = "CF",
    measure_label = "52",
    S1_KPI_title = "112",
    S1_DON_chart_province_title = "122",
    S1_DON_chart_department_title = "132",
    S1_TS_chart_title = "143",
    S2_MAP_chart_title = "212",
    S2_RE_chart_title = "232",
    S2_PAR_chart_province_title = "242",
    S2_PAR_chart_department_title = "252",
    S4_ST_forecasting_main_title = "414",
    color = "#B56576",
    palet = c(
      "#355070",
      "#6D597A",
      "#B56576",
      "#E56B6F",
      "#E88C7D",
      "#EAAC8B"
    ),
    palet2 = colorRampPalette(colors = c("#f7c8d2", "#a61232"), space = "Lab")(8)
  ),
  CF_AC = list(
    id = "CF_AC",
    measure_label = "59",
    S1_TS_chart_title = "144",
    color = "#B56576",
    palet = c(
      "#355070",
      "#6D597A",
      "#B56576",
      "#E56B6F",
      "#E88C7D",
      "#EAAC8B"
    ),
    palet2 = colorRampPalette(colors = c("#f7c8d2", "#a61232"), space = "Lab")(8)
  ),
  V = list(
    id = "V",
    measure_label = "53",
    S1_KPI_title = "113",
    S1_DON_chart_province_title = "123",
    S1_DON_chart_department_title = "133",
    S1_TS_chart_title = "145",
    S2_MAP_chart_title = "213",
    S2_RE_chart_title = "233",
    S2_PAR_chart_province_title = "243",
    S2_PAR_chart_department_title = "253",
    color = "#355070",
    palet = c(
      "#355070",
      "#6D597A",
      "#B56576",
      "#E56B6F",
      "#E88C7D",
      "#EAAC8B"
    ),
    palet2 = colorRampPalette(colors = c("#bfddf2", "#355070"), space = "Lab")(8)
  ),
  V_AC = list(
    id = "V_AC",
    measure_label = "60",
    S1_TS_chart_title = "146",
    color = "#355070",
    palet = c(
      "#355070",
      "#6D597A",
      "#B56576",
      "#E56B6F",
      "#E88C7D",
      "#EAAC8B"
    ),
    palet2 = colorRampPalette(colors = c("#bfddf2", "#355070"), space = "Lab")(8)
  ),
  VesqC = list(
    id = "VesqC",
    measure_label = "54",
    S1_KPI_title = "114",
    color = "#355070",
    palet = c(
      "#355070",
      "#6D597A",
      "#B56576",
      "#E56B6F",
      "#E88C7D",
      "#EAAC8B"
    ),
    palet2 = colorRampPalette(colors = c("#bfddf2", "#355070"), space = "Lab")(8)
  ),
  RM = list(
    id = "RM",
    measure_label = "55",
    S2_MAP_chart_title = "214",
    color = "#a61232",
    palet = c(
      "#355070",
      "#6D597A",
      "#B56576",
      "#E56B6F",
      "#E88C7D",
      "#EAAC8B"
    ),
    palet2 = colorRampPalette(colors = c("#ffd1db", "#e00434"), space = "Lab")(8)
  ),
  RC100 = list(
    id = "RC100",
    measure_label = "56",
    S2_MAP_chart_title = "215",
    color = "#E56B6F",
    palet = c(
      "#355070",
      "#6D597A",
      "#B56576",
      "#E56B6F",
      "#E88C7D",
      "#EAAC8B"
    ),
    palet2 = colorRampPalette(colors = c("#ffd6d7", "#e85d5f"), space = "Lab")(8)
  ),
  RCF100 = list(
    id = "RCF100",
    measure_label = "57",
    S2_MAP_chart_title = "216",
    color = "#B56576",
    palet = c(
      "#355070",
      "#6D597A",
      "#B56576",
      "#E56B6F",
      "#E88C7D",
      "#EAAC8B"
    ),
    palet2 = colorRampPalette(colors = c("#f7c8d2", "#a61232"), space = "Lab")(8)
  )
)


#lista de provincias utilizada en los selectores
listaProvincias <- c(
  "Buenos Aires",
  "CABA",
  "Catamarca",
  "Chaco",
  "Chubut",
  "Córdoba",
  "Corrientes",
  "Entre Ríos",
  "Jujuy",
  "La Pampa",
  "La Rioja",
  "Mendoza",
  "Misiones",
  "Neuquén",
  "Río Negro",
  "Salta",
  "San Juan",
  "San Luis",
  "Santa Cruz",
  "Santa Fe",
  "Santiago del Estero",
  "Tierra del Fuego",
  "Tucumán",
  "Formosa"
)

#lista completa de selecciones, incluye las provincias y la opcion de seleccionar todas
elecciones_pcias <- c("[Todas]",
                      #"[Desconocido]",
                      listaProvincias)


listaProvincias2 <- listaProvincias
names(listaProvincias2) <- listaProvincias2

flags <- data.frame(
  val = c("es","en")
)

flags$img = c(
  sprintf("<img src='assets/icons/spain.png' width=30px><div class='jhr'>%s</div></img>", flags$val[1]),
  sprintf("<img src='assets/icons/united-kingdom.png' width=30px><div class='jhr'>%s</div></img>", flags$val[2])
)
