

#' Change data.table Idate to date
#'
#' @param dt data.table
#' @param which the target date variable. either 'date' or 'first_last'
#'
#' @return a data.table
#' @export
#'
#' @examples clean_date(dt, which = "first_last")
clean_date <- function(dt, which = "date") {
  if (which == "date") {
    dt[, date := lubridate::as_date(date)][]
  } else if (which == "first_last") {
    dt[, `:=`(first_purchase_date = lubridate::as_date(first_purchase_date),
              last_purchase_date  = lubridate::as_date(last_purchase_date))][]
  }
}




#' Create RFM Metric variables
#'
#' @param dt data.table
#' @param analysis_date date type, the date of conducting the analysis.
#'
#' @return a data.table with Recency, Frequency, Monetary metrics and other
#' variables
#' @export
#'
#' @examples set_up_data_table(dt, "2021-02-25")
set_up_data_table <- function(dt, analysis_date) {
  # Needs load_data output

  f_dt <- data.table::copy(dt)[, .(first_purchase_date = min(date),
                                   last_purchase_date = max(date),
                                   total_quantity = sum(quantity),
                                   frequency = data.table::uniqueN(date),
                                   monetary = sum(revenue)),
                               keyby = .(customer_id)][]

  f_dt[, recency := as.double(gsub("[[:alpha:]]", "", analysis_date-last_purchase_date))][]
  f_dt[, life_time := as.double(gsub("[[:alpha:]]", "", analysis_date-first_purchase_date))][]

  data.table::setcolorder(f_dt, c("customer_id", "first_purchase_date",
                                  "last_purchase_date", "total_quantity",
                                  "recency", "frequency", "monetary", "life_time"))
  return(f_dt)
}



#' Log Transformation
#'
#' @param dt data.table
#' @param var character value, it can be a numeric variable, more than one numeric
#' variables or 'all'
#'
#' @details when var is 'all' recency, frequency and monetary variable will be
#' transformed.
#'
#' @return a data.table
#' @export
#'
#' @examples log_transform(dt, 'all')
log_transform <- function(dt, var) {
  if (length(var) == 1) {
    if (var != "none") {
      if (var == "all") {
        data.table::copy(dt)[, data.table::`:=`(recency = log(recency + 1),
                                    frequency = log(frequency + 1),
                                    monetary = log(monetary + 1))][]
      } else {
        dt[[var]] <- log(dt[[var]] + 1)
        dt
      }
    } else {
      dt
    }
  } else {
    data.table::copy(dt)[, (var) := lapply(.SD, \(.x) log(.x + 1)), .SDcols = var][]
  }
}
