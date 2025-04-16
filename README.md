# AppSalud | Aplicación WEB para analítica de la COVID-19

Aplicación web de tipo *dashboard* que hace uso de la visualización de datos y el modelado de series temporales para monitorear el comportamiento de la COVID-19 en la República Argentina.

![alt text](https://github.com/FernandoJesus21/AppSalud/blob/main/appSalud_01.png?raw=true)

# Puntos clave

1) Desarrollada con el framework Shiny (lenguaje R).
2) Variables principales: casos confirmados, casos confirmados registrados como fallecimientos, cantidad de vacunas aplicadas.
3) Permite interactuar con los elementos gráficos por año, mes y provincia.
4) Modelos disponibles: NAIVE, AVERAGE, ETS, ARIMA.
5) Posibilidad de consultar y descargar los conjuntos de datos desde la aplicación.

# Fuentes de datos

1) Ministerio de salud, para las [variables relativas a la COVID-19](http://datos.salud.gob.ar/dataset?groups=covid-19).
2) INDEC, variables demográficas del [CENSO 2022](https://censo.gob.ar/).
3) Instituto Geográfico Nacional, para las [capas SIG](https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG).

# Detalles

Probado bajo entornos Windows y GNU/Linux con R 4.4.0 y 4.4.1

- En Windows requiere de [rtools](https://cran.r-project.org/bin/windows/Rtools/) para la instalación de algunas bibliotecas.
- En GNU/Linux la instalación de algunas bibliotecas de R puede requerir la instalación previa de paquetes del sistema.

Una vez clonado el repositorio, ejecutar renv::restore() o renv::install() para instalar todas las bibliotecas requeridas para el proyecto.

1. Los archivos de datos que lee esta aplicación son creados en su mayoría por el [proceso ETL](https://github.com/FernandoJesus21/AppSalud_ETL_Framework).
2. La herramienta de modelos de previsión puede ser consultada de manera independiente desde [aquí](https://github.com/FernandoJesus21/AppSalud_Forecasting).

# Problemas conocidos

Pueden ocurrir problemas con la instalación y compilación de la biblioteca 'terra'. Si esto ocurre intentar instalar la biblioteca manualmente.

# Créditos

A [Appsilon](https://www.appsilon.com/) por el desarrollo de la plantilla '*shiny enterprise dashboard*' utilizada como base para este proyecto.
