
import("lubridate")

getMonthsChoices <- function(year = NULL, data_last_day, data_first_day) {
  months <- c(1:12)
  monthNames <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
  names(months) <- monthNames
  #names(months) <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  months_choices <- months

  if (is.null(year) || year == year(data_last_day)) {
    months_choices <- months[1:month(data_last_day)]
  }
  if (is.null(year) || year == year(data_first_day)) {
    months_choices <- months[month(data_first_day):12]
  }
  
  c("13" = "0", months_choices)
  
}

getYearChoices <- function(data_start_date, data_last_day) {
  c(year(data_last_day):year(data_start_date))
}
