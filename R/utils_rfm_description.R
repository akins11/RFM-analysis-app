#' UI value card
#'
#' @param value card value.
#' @param label value label.
#' @param icon card icon.
#'
#' @return
#' @export
#'
value_card <- function(value, label, icon) {

  bs4Dash::box(
    class = "p-3 pb-4",

    shiny::div(
      class = "d-flex justify-content-center",

      shiny::div(
        class = "pr-4",
        style = "border-right: solid 1px #F0F0F0;",

        shiny::p(value, class = "h4"),
        shiny::p(label, class = "h6", style = "color: #FCFCFC;")
      ),

      shiny::div(
        class = "align-self-end pl-1",

        fontawesome::fa(name = glue::glue("fas fa-{icon}"),
                        fill = "#FCFCFC",
                        fill_opacity = 8,
                        height = "5em",
                        margin_left = "0.2em")
      )
    ),

    width = 12,
    collapsible = FALSE,
    headerBorder = FALSE,
    background = "indigo",
    gradient= TRUE
  )
}


#' description distribution input options
#'
#' @param sidebar_id box side bar id.
#' @param slider_id slider input id.
#' @param radio_id radio button input id.
#' @param numeric_id numeric input id.
#' @param check_id check box input id.
#'
#' @return
#' @export
#'
distibution_options <- function(sidebar_id, slider_id, radio_id, numeric_id, check_id) {
  bs4Dash::boxSidebar(
    startOpen = FALSE,
    id = sidebar_id,
    background = box_sidebar_bg,
    icon = fontawesome::fa_i("fas fa-angles-left", verify_fa = FALSE),

    shiny::sliderInput(inputId = slider_id, label = "Zoom",
                       value = c(0, 1),
                       min = 0, max = 1, step = 0.5),

    shiny::tags$br(),

    shinyWidgets::prettyRadioButtons(
      inputId = radio_id,
      label = "Type of Plot",
      choices = c("Histogram" = "hist", "Density" = "dens", "Boxplot" = "box"),
      selected = "hist",
      status = "info",
      shape = "curve",
      thick = TRUE,
      bigger = TRUE,
      animation = "pulse"
    ),

    tags$br(),

    shiny::numericInput(inputId = numeric_id, label = "Number of bins",
                        value = 20,
                        min = 20, max = 50, step = 5),

    tags$br(),

    shinyWidgets::prettyCheckbox(
      inputId = check_id,
      label = "Log",
      value = FALSE,
      status = "info",
      shape = "curve",
      thick = TRUE,
      bigger = TRUE,
      animation = "pulse"
    )
  )
}



#' description date input options.
#'
#' @param sidebar_id box side bar id.
#' @param radio_id radio button input id.
#' @param wd side bar width.
#'
#' @return
#' @export
#'
date_plot_option <- function(sidebar_id, radio_id, wd = 50) {
  bs4Dash::boxSidebar(
    startOpen = FALSE,
    id = sidebar_id,
    width = wd,
    background = box_sidebar_bg,
    icon = fontawesome::fa_i("fas fa-angles-left", verify_fa = FALSE),

    shinyWidgets::prettyRadioButtons(
      inputId = radio_id,
      label = "Type of Plot",
      choices = c("Bar" = "bar", "Line" = "line", "Area" = "area"),
      selected = "area",
      status = "info",
      shape = "curve",
      thick = TRUE,
      bigger = TRUE,
      animation = "pulse"
    )
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
# rfm_boxplot <- function(dt, var, zoom, log_var = FALSE, interactive = FALSE) {
#   if (log_var) {
#     dt <- log_transform(dt = dt, var = var)
#   }
#
#   f_plt <- dt |>
#     ggplot2::ggplot(ggplot2::aes(y = .data[[var]])) +
#     ggplot2::geom_boxplot(color = line_color) +
#     ggplot2::theme_minimal() +
#     ggplot2::labs(x = NULL, y = NULL) +
#     ggplot2::scale_y_continuous(labels = scales::comma_format()) +
#     ggplot2::theme(axis.text.x = ggplot2::element_blank(),
#                    axis.ticks.x = ggplot2::element_blank(),
#                    panel.grid.major.x = ggplot2::element_blank(),
#                    panel.grid.minor.x = ggplot2::element_blank())
#
#   if (!missing(zoom)) {
#     zoom <- ifelse(length(zoom) == 1, list(c(0, zoom)), list(zoom))[[1]]
#
#     f_plt <- f_plt + ggplot2::coord_cartesian(ylim = zoom)
#   } else {
#     f_plt
#   }
#   if (interactive) {
#     plotly::ggplotly(f_plt) |>
#       plotly::config(displayModeBar = FALSE)
#   } else {
#     f_plt
#   }
# }


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
# rfm_distribution_plot <- function(dt, var,
#                                   zoom, bins = 30, log_var = FALSE,
#                                   interactive = FALSE) {
#   if (log_var) {
#     dt <- log_transform(dt = dt, var = var)
#   }
#
#   x_label <- clean_label(var)
#
#   f_plt <- dt |>
#     ggplot2::ggplot(ggplot2::aes(x = .data[[var]])) +
#     ggplot2::geom_histogram(bins = bins, fill = bar_color) +
#     ggplot2::scale_x_continuous(labels = scales::comma_format()) +
#     ggplot2::scale_y_continuous(labels = scales::comma_format()) +
#     ggplot2::labs(x = NULL, y = "Count") +
#     ggplot2::theme_minimal()
#
#   if (!missing(zoom)) {
#     zoom <- ifelse(length(zoom) == 1, list(c(0, zoom)), list(zoom))[[1]]
#
#     f_plt <- f_plt + ggplot2::coord_cartesian(xlim = zoom)
#   } else {
#     f_plt
#   }
#   if (interactive) {
#     plotly::ggplotly(f_plt) |>
#       plotly::config(displayModeBar = FALSE)
#   } else {
#     f_plt
#   }
# }


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
# monthly_intake <- function(dt, geom = "line", interactive = FALSE) {
#   f_dt <- dt[, .(count = .N), keyby = .(first_purchase_month)][]
#   data.table::setnames(f_dt, old = "first_purchase_month", new = "First Purchase Month")
#
#   f_plt <- f_dt |>
#     ggplot2::ggplot(ggplot2::aes(x = `First Purchase Month`, y = count))
#
#   if (geom == "line") {
#     f_plt <- f_plt + ggplot2::geom_line(color = line_color)
#
#   } else if (geom == "area") {
#     f_plt <- f_plt +
#       ggplot2::geom_line(color = line_color) +
#       ggplot2::geom_area(fill = line_color, alpha = 0.2)
#
#   } else if (geom == "bar") {
#     f_plt <- f_plt + ggplot2::geom_col(fill = bar_color)
#
#   } else {
#     stop("`geom` must be any of 'line', 'bar' or 'area'")
#   }
#
#   f_plt <- f_plt +
#     ggplot2::scale_y_continuous(labels = scales::comma_format()) +
#     ggplot2::labs(x = "Inital Cutomer Purchase Month", y = "Count") +
#     ggplot2::theme_minimal()
#
#   if (interactive) {
#     plotly::ggplotly(f_plt) |>
#       plotly::config(displayModeBar = FALSE)
#   } else {
#     f_plt
#   }
# }

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
# quarterly_intake <- function(dt, geom = "line", interactive = FALSE) {
#   f_dt <- data.table::copy(dt)[, quarter := data.table::fcase(
#     quarter == 1, "First",
#     quarter == 2, "Second",
#     quarter == 3, "Third",
#     quarter == 4, "Fourth")][]
#
#   q_levels <- c("First-2021", "Second-2021", "Third-2021", "Fourth-2021",
#                 "First-2022")
#
#   f_plt <- f_dt[, .(count = .N), keyby = .(quarter, year)
#   ][, quarter_year := paste0(quarter, "-", as.character(year))
#   ][, quarter_year := factor(quarter_year, levels = q_levels)][] |>
#
#     ggplot2::ggplot(ggplot2::aes(x = quarter_year, y = count))
#
#   if (geom == "line") {
#     f_plt <- f_plt + ggplot2::geom_line(ggplot2::aes(text = paste0(quarter, " Quarter ",
#                                                                    year, "<br>",
#                                                                    "Count: ",
#                                                                    scales::comma(count)),
#                                             group = 1), color = line_color)
#   } else if (geom == "bar") {
#     f_plt <- f_plt + ggplot2::geom_col(fill = bar_color)
#
#   } else if (geom == "area") {
#     f_plt <- f_plt +
#       ggplot2::geom_line(ggplot2::aes(text = paste0(quarter, " Quarter ",
#                                                     year, "<br>",
#                                                     "Count: ", scales::comma(count)),
#                              group = 1), color = line_color) +
#       ggplot2::geom_area(ggplot2::aes(group = 1), fill = line_color, alpha = 0.2)
#   }
#
#   f_plt <- f_plt +
#     ggplot2::scale_y_continuous(labels = scales::comma_format()) +
#     ggplot2::labs(x = "Initial Customer Purchase Quarter", y = "Count") +
#     ggplot2::theme_minimal()
#
#   if (interactive) {
#     plotly::ggplotly(
#       f_plt,
#       tooltip = "text",
#     ) |>
#       plotly::config(displayModeBar = FALSE)
#   } else {
#     f_plt
#   }
# }
