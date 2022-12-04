
#' Dashboard value box output value
#'
#' @param dt a data.table containing variables such as customer_id, product, revenue,
#' and life_time.
#' @param what character value, the type of value to return. it can be any of
#' 'n_customers', 'n_product', 'total_revenue' or 'average_lt'.
#' @param rfm_dt the main data.
#'
#' @return numeric value depending on the value of what arg.
#' @export
#'
#' @examples get_value(dt, what = 'total_revenue')
get_value <- function(dt, what, rfm_dt) {
  if (what == "n_customers") {
    unique(dt[["customer_id"]]) |> length() |> scales::comma()

  } else if (what == "n_product") {
    unique(dt[["product"]]) |> length() |> scales::comma()

  } else if (what == "total_revenue") {
    sum(dt[["revenue"]]) |> scales::comma()

  } else if (what == "average_lt") {
    mean(rfm_dt[["life_time"]]) |> scales::comma()
  }
}



#' Clean plot label and strings
#'
#' @param string  a character value to clean and transform.
#' @param str_remove a string in the string arg to remove.
#'
#' @return a string with title format and space between words.
#' @export
#'
#' @examples clean_label("first_shiny_app", str_remove = "first")
clean_label <- function(string, str_remove) {
  c_str <- gsub("[[:punct:]]", " ", string)

  if (!missing(str_remove)) {
    c_str <- gsub(str_remove, "", c_str)
  }
  gsub(pattern = "\\b([[:lower:]])([[:lower:]]+)",
       replacement = "\\U\\1\\L\\2",
       x = c_str,
       perl = TRUE) |>
    trimws()
}



#' App slider values.
#'
#' @param dt a data.table
#' @param var a variable to extract the min, max and a step value.
#'
#' @return list containing min, max, step.
#' @export
#'
#' @examples distribution_zoom_value(dt, 'monetary')
distribution_zoom_value <- function(dt, var) {
  dplyr::lst(
    min = round(min(dt[[var]]), 2),
    max = round(max(dt[[var]]), 2),
    stp = round(max*0.1, 2)
  )
}



#' Box plot
#'
#' @param dt a data.table
#' @param var a variable to plot.
#' @param zoom a vector of to values to limit the number of observations that
#' fall within the generated axis.
#' @param log_var logical, whether to log transform the variable in var.
#'
#' @return a ggplot object if interactive = FALSE else a plotly object.
#' @export
#'
#' @examples rfm_boxplot(dt, 'recency', FALSE, TRUE)
#'
rfm_boxplot <- function(dt, var, zoom = NULL, log_var = FALSE) {
  if (log_var) {
    dt <- log_transform(dt = dt, var = var)
  }

  if (!is.null(zoom)) {
    zoom <- ifelse(length(zoom) == 1, list(c(0, zoom)), list(zoom))[[1]]

    dt <- data.table::copy(dt)[data.table::between(get(var), zoom[1], zoom[2])]
  }

  f_plt <- dt |>
    echarts4r::e_charts() |>
    echarts4r::e_boxplot_(serie = var) |>
    echarts4r::e_tooltip(trigger = "item") |>
    echarts4r::e_color(line_color) |>
    echarts4r::e_theme("macarons")

  if (!is.null(zoom)) {
    echarts4r::e_y_axis(f_plt, min = zoom[1], max = zoom[2])

  } else {
    f_plt
  }
}



#' Distribution plot
#'
#' @param dt a data.table
#' @param var a variable to plot.
#' @param type type of plot output either 'hist' for histogram or 'dens' for density.
#' @param zoom a vector of to values to limit the number of observations that
#' fall within the generated axis.
#' @param bins number of bins see ?ggplot2::geom_histogram function.
#' @param log_var logical, whether to log transform the variable in var.
#'
#' @return a ggplot object if interactive = FALSE else a plotly object.
#' @export
#'
#' @examples rfm_distribution_plot(dt, 'frequency' zoom = c(0, 150))
#'
rfm_distribution_plot <- function(dt, var,
                                  type = "hist", zoom = NULL, bins = 30,
                                  log_var = FALSE) {

  if (log_var) {
    dt <- log_transform(dt = dt, var = var)
  }

  if (!is.null(zoom)) {
    zoom <- ifelse(length(zoom) == 1, list(c(0, zoom)), list(zoom))[[1]]

    dt <- data.table::copy(dt)[data.table::between(get(var), zoom[1], zoom[2])]
  }

  f_e <- echarts4r::e_charts(data = dt)

  if (type == "hist") {
    f_e <- echarts4r::e_histogram_(f_e, serie = var, name = "Count",
                                   breaks = bins, legend = FALSE)

  } else if (type == "dens") {
    f_e <- echarts4r::e_density_(f_e, serie = var, name = "Count",
                                 breaks = bins, legend = FALSE) |>
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(digits = 2))

  } else {
    stop("argument `type` must be either 'hist' or 'dens'")
  }

  f_e |>
    echarts4r::e_color(bar_color) |>
    echarts4r::e_axis_labels(x = "", y = "Count") |>
    echarts4r::e_tooltip(tigger = "axis",
                         backgroundColor = "#FAFAFA") |>
    echarts4r::e_theme("macarons")
}




#' Extract first purchase month, quarter and year variable
#'
#' @param dt data.table
#'
#' @return a data.table with first purchase month, quarter and year variables
#' @export
#'
#' @examples
create_date_dt <- function(dt) {
  data.table::copy(dt)[, .(first_purchase_date)
        ][, first_purchase_month := lubridate::`day<-`(first_purchase_date, 1)
          ][, quarter := data.table::quarter(first_purchase_month)
            ][, year := data.table::year(first_purchase_month)][]
  # first_purchase_date not useful outside this function.
}



#' Monthly customer intake plot.
#'
#' @param dt a data.table containing first_purchase_month variable.
#' @param geom character variable, the type of ggplot2 geom to use. it can be
#' any of 'bar', 'line' or 'area'.
#'
#' @return a ggplot object if interactive = FALSE else a plotly object.
#' @export
#'
#' @examples monthly_intake(dt, 'area', TRUE)
#'
monthly_intake <- function(dt, geom = "line") {
  f_dt <- dt[, .(count = .N), keyby = .(first_purchase_month)][]
  data.table::setnames(f_dt, old = "first_purchase_month", new = "First Purchase Month")

  f_plt <- echarts4r::e_charts(data = f_dt, x = `First Purchase Month`)

  if (geom == "line") {
    f_plt <- echarts4r::e_line(f_plt, serie = count, name = "Count", legend = FALSE)

  } else if (geom == "bar") {
    f_plt <- echarts4r::e_bar(f_plt, serie = count, name = "Count", legend = FALSE)

  } else if (geom == "area") {
    f_plt <- echarts4r::e_area(f_plt, serie = count, name = "Count", legend = FALSE)
  }

  f_plt |>
    echarts4r::e_color(line_color) |>
    echarts4r::e_tooltip(trigger = "axis",
                         backgroundColor = "#FAFAFA") |>
    echarts4r::e_theme("macarons")
}


#' Quarterly customer intake plot.
#'
#' @param dt a data.table containing quarter, year variables
#' @param geom character variable, the type of ggplot2 geom to use. it can be
#' any of 'bar', 'line' or 'area'
#'
#' @return a ggplot object if interactive = FALSE else a plotly object.
#' @export
#'
#' @examples quarterly_intake(dt, 'line', TRUE)
#'
quarterly_intake <- function(dt, geom = "line") {
  f_dt <- data.table::copy(dt)[, quarter := data.table::fcase(
    quarter == 1, "First",
    quarter == 2, "Second",
    quarter == 3, "Third",
    quarter == 4, "Fourth")][]

  q_levels <- c("First-2021", "Second-2021", "Third-2021", "Fourth-2021",
                "First-2022")

  f_dt <- f_dt[, .(count = .N), keyby = .(quarter, year)
  ][, quarter_year := paste0(quarter, "-", as.character(year))
  ][, quarter_year := factor(quarter_year, levels = q_levels)][] |>
    data.table::setorder(quarter_year)

  f_plt <- echarts4r::e_charts(f_dt, x = quarter_year)

  if (geom == "bar") {
    f_plt <- echarts4r::e_bar(f_plt, serie = count, name = "Count", legend = FALSE)

  } else if (geom == "line") {
    f_plt <- echarts4r::e_line(f_plt, serie = count, name = "Count", legend = FALSE)

  } else if (geom == "area") {
    f_plt <- echarts4r::e_area(f_plt, serie = count, name = "Count", legend = FALSE)
  }

  f_plt |>
    echarts4r::e_color(line_color) |>
    echarts4r::e_tooltip(backgroundColor = "#FAFAFA") |>
    echarts4r::e_theme("macarons")
}
