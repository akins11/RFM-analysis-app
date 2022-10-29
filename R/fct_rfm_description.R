
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
#' @param interactive logical, whether to return an interactive plot or a static
#' plot
#'
#' @return a ggplot object if interactive = FALSE else a plotly object.
#' @export
#'
#' @examples rfm_boxplot(dt, 'recency', FALSE, TRUE)
rfm_boxplot <- function(dt, var, zoom, log_var = FALSE, interactive = FALSE) {
  if (log_var) {
    dt <- log_transform(dt = dt, var = var)
  }

  f_plt <- dt |>
    ggplot2::ggplot(ggplot2::aes(y = .data[[var]])) +
    ggplot2::geom_boxplot(color = line_color) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank())

  if (!missing(zoom)) {
    zoom <- ifelse(length(zoom) == 1, list(c(0, zoom)), list(zoom))[[1]]

    f_plt <- f_plt + ggplot2::coord_cartesian(ylim = zoom)
  } else {
    f_plt
  }
  if (interactive) {
    plotly::ggplotly(f_plt) |>
      plotly::config(displayModeBar = FALSE)
  } else {
    f_plt
  }
}



#' Distribution plot
#'
#' @param dt a data.table
#' @param var a variable to plot.
#' @param zoom a vector of to values to limit the number of observations that
#' fall within the generated axis.
#' @param bins number of bins see ?ggplot2::geom_histogram function.
#' @param log_var logical, whether to log transform the variable in var.
#' @param interactive logical, whether to return an interactive plot or a static
#' plot.
#'
#' @return a ggplot object if interactive = FALSE else a plotly object.
#' @export
#'
#' @examples rfm_distribution_plot(dt, 'frequency' zoom = c(0, 150),
#' interactive = TRUE)
rfm_distribution_plot <- function(dt, var,
                                  zoom, bins = 30, log_var = FALSE,
                                  interactive = FALSE) {
  if (log_var) {
    dt <- log_transform(dt = dt, var = var)
  }

  x_label <- clean_label(var)

  f_plt <- dt |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[var]])) +
    ggplot2::geom_histogram(bins = bins, fill = bar_color) +
    ggplot2::scale_x_continuous(labels = scales::comma_format()) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::labs(x = NULL, y = "Count") +
    ggplot2::theme_minimal()

  if (!missing(zoom)) {
    zoom <- ifelse(length(zoom) == 1, list(c(0, zoom)), list(zoom))[[1]]

    f_plt <- f_plt + ggplot2::coord_cartesian(xlim = zoom)
  } else {
    f_plt
  }
  if (interactive) {
    plotly::ggplotly(f_plt) |>
      plotly::config(displayModeBar = FALSE)
  } else {
    f_plt
  }
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
#' any of 'bar', 'line' or 'area'
#' @param interactive logical, whether to returned an interactive plot or a
#' static plot.
#'
#' @return a ggplot object if interactive = FALSE else a plotly object.
#' @export
#'
#' @examples monthly_intake(dt, 'area', TRUE)
monthly_intake <- function(dt, geom = "line", interactive = FALSE) {
  f_dt <- dt[, .(count = .N), keyby = .(first_purchase_month)][]
  data.table::setnames(f_dt, old = "first_purchase_month", new = "First Purchase Month")

  f_plt <- f_dt |>
    ggplot2::ggplot(ggplot2::aes(x = `First Purchase Month`, y = count))

  if (geom == "line") {
    f_plt <- f_plt + ggplot2::geom_line(color = line_color)

  } else if (geom == "area") {
    f_plt <- f_plt +
      ggplot2::geom_line(color = line_color) +
      ggplot2::geom_area(fill = line_color, alpha = 0.2)

  } else if (geom == "bar") {
    f_plt <- f_plt + ggplot2::geom_col(fill = bar_color)

  } else {
    stop("`geom` must be any of 'line', 'bar' or 'area'")
  }

  f_plt <- f_plt +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::labs(x = "Inital Cutomer Purchase Month", y = "Count") +
    ggplot2::theme_minimal()

  if (interactive) {
    plotly::ggplotly(f_plt) |>
      plotly::config(displayModeBar = FALSE)
  } else {
    f_plt
  }
}



#' Quarterly customer intake plot.
#'
#' @param dt a data.table containing quarter, year variables
#' @param geom character variable, the type of ggplot2 geom to use. it can be
#' any of 'bar', 'line' or 'area'
#' @param interactive logical, whether to returned an interactive plot or a
#' static plot.
#'
#' @return a ggplot object if interactive = FALSE else a plotly object.
#' @export
#'
#' @examples quarterly_intake(dt, 'line', TRUE)
quarterly_intake <- function(dt, geom = "line", interactive = FALSE) {
  f_dt <- data.table::copy(dt)[, quarter := data.table::fcase(
    quarter == 1, "First",
    quarter == 2, "Second",
    quarter == 3, "Third",
    quarter == 4, "Fourth")][]

  q_levels <- c("First-2021", "Second-2021", "Third-2021", "Fourth-2021",
                "First-2022")

  f_plt <- f_dt[, .(count = .N), keyby = .(quarter, year)
  ][, quarter_year := paste0(quarter, "-", as.character(year))
  ][, quarter_year := factor(quarter_year, levels = q_levels)][] |>

    ggplot2::ggplot(ggplot2::aes(x = quarter_year, y = count))

  if (geom == "line") {
    f_plt <- f_plt + ggplot2::geom_line(ggplot2::aes(text = paste0(quarter, " Quarter ",
                                                                   year, "<br>",
                                                                   "Count: ",
                                                                   scales::comma(count)),
                                            group = 1), color = line_color)
  } else if (geom == "bar") {
    f_plt <- f_plt + ggplot2::geom_col(fill = bar_color)

  } else if (geom == "area") {
    f_plt <- f_plt +
      ggplot2::geom_line(ggplot2::aes(text = paste0(quarter, " Quarter ",
                                                    year, "<br>",
                                                    "Count: ", scales::comma(count)),
                             group = 1), color = line_color) +
      ggplot2::geom_area(ggplot2::aes(group = 1), fill = line_color, alpha = 0.2)
  }

  f_plt <- f_plt +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::labs(x = "Initial Customer Purchase Quarter", y = "Count") +
    ggplot2::theme_minimal()

  if (interactive) {
    plotly::ggplotly(
      f_plt,
      tooltip = "text",
    ) |>
      plotly::config(displayModeBar = FALSE)
  } else {
    f_plt
  }
}
