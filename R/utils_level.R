#' top % bottom product box side bar
#'
#' @param sidebar_id side bar id
#' @param numeric_id numeric input id.
#' @param numeric_label numeric input label.
#' @param select_id select input id.
#'
#' @return
#' @export
#'
top_bottom_sidebar <- function(sidebar_id,
                               numeric_id,
                               numeric_label,
                               select_id) {
  bs4Dash::boxSidebar(
    id = sidebar_id,
    startOpen = FALSE,
    width = 30,
    background = box_sidebar_bg,
    icon = fontawesome::fa_i("fas fa-angles-left", verify_fa = FALSE),

    shinyWidgets::numericInputIcon(
      inputId = numeric_id,
      label = numeric_label,
      min = 5, max = 20, value = 10, step = 1),

    shiny::tags$br(),

    shinyWidgets::pickerInput(
      inputId = select_id,
      label = "Aggregate Function",
      choices = c("Minimum" = "min", "Average" = "mean",
                  "Median" = "median", "Maximum" = "max",
                  "Total" = "sum"),
      selected = "sum",
      options = shinyWidgets::pickerOptions(style = "btn-default")
    )
  )
}

#' Summary of products within a particular segment
#'
#' @param dt data.table
#' @param agg_fun an aggregate function. it can be any of 'min', 'max', 'mean',
#'  'median', 'sum'
#' @param direction 'top' or 'bottom'
#' @param n_categories number of products to add to the plot.
#' @param by the summarized variable.
#' @param segment the name of the segment.
#'
#' @return a ggplot object
#' @export
#'
#' @examples product_by_segment(dt, 'median', 'top', 10, 'Revenue', 'Champions')
# product_by_segment <- function(dt,
#                                agg_fun, direction, n_categories = 10,
#                                by, segment) {
#   plt_title <- list(
#     tl = paste(
#       clean_label(direction), n_categories,
#       "Products From Customers In <span style = 'color:#9600FF;'>",
#       clean_label(segment), "</span> Segment"
#     ),
#     sub = paste("By", agg_label[[agg_fun]], clean_label(by))
#   )
#
#   names(dt) <- tolower(names(dt))
#
#   if (all(c("minimum", "maximum") %in% names(dt))) {
#     data.table::setnames(
#       dt,
#       old = c("minimum", "maximum"),
#       new = c("min", "max")
#     )
#   }
#
#   if (direction == "top") {
#     f_dt <- head(dt, n_categories)
#
#   } else if (direction == "bottom") {
#     f_dt <- tail(dt, n_categories)
#   } else {
#     stop("argument `direction` can only be either 'top' or 'bottom'")
#   }
#
#   template <- ggplot2::ggplot(f_dt) +
#     ggplot2::geom_col(show.legend = FALSE) +
#     ggplot2::scale_x_continuous(labels = scales::comma_format()) +
#     ggplot2::theme_minimal() +
#     ggplot2::scale_fill_gradient2(low = heatmap$low, mid = heatmap$mid, high = heatmap$high,
#                                   midpoint = mean(f_dt[[agg_fun]])) +
#     ggplot2::theme(plot.title.position = "plot",
#                    plot.title = ggtext::element_markdown())
#
#   if (direction == "top") {
#     template %+%
#       ggplot2::aes(x = .data[[agg_fun]],
#                    y = reorder(product, .data[[agg_fun]]),
#                    fill = .data[[agg_fun]]) +
#       ggplot2::labs(x = NULL, y = NULL,
#                     title = plt_title$tl, subtitle = plt_title$sub)
#
#   } else if (direction == "bottom") {
#     template %+%
#       ggplot2::aes(x = .data[[agg_fun]],
#                    y = reorder(product, .data[[agg_fun]], decreasing = TRUE),
#                    fill = .data[[agg_fun]]) +
#       ggplot2::labs(x = NULL, y = NULL,
#                     title = plt_title$tl, subtitle = plt_title$sub)
#   }
# }


#' Minimum and maximum plot
#'
#' @param dt data.table
#' @param by the summarized variable
#' @param segment the name of the segment.
#' @param n number of products to add to the plot.
#' @param interactive logical, whether to returned an interactive plot or a
#' static plot.
#'
#' @return a ggplot object if interactive = FALSE else a plotly object.
#' @export
#'
#' @examples min_max_summary(dt, 'quantity', 'Champions', 10, TRUE)
# min_max_summary <- function(dt, by, segment, n, interactive = FALSE) {
#   if (all(c("min", "max", "product") %in% names(dt))) {
#
#     data.table::setnames(
#       dt,
#       old = c("min", "max", "product"),
#       new = c("Minimum", "Maximum", "Product")
#       )
#   }
#   plt_title <- paste("Minimum & Maximum", clean_label(by), "In",
#                      clean_label(segment), "Segment")
#
#   f_plt <- dt[order(-Maximum)] |>
#     head(n) |>
#     ggplot2::ggplot() +
#     ggplot2::geom_segment(ggplot2::aes(x = Minimum, xend = Maximum,
#                                        y = Product, yend = Product),
#                           size = 3, alpha = 0.3, color = bar_color) +
#     ggplot2::geom_point(ggplot2::aes(x = Minimum, y = Product), color = "#4900FF", size = 6.5) +
#     ggplot2::geom_point(ggplot2::aes(x = Maximum, y = Product), color = "#00FFF9", size = 8.5) +
#     ggplot2::scale_x_continuous(labels = scales::comma_format()) +
#     ggplot2::labs(x = NULL, y = NULL, title = plt_title) +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(plot.title.position = "plot")
#
#   if (interactive) {
#     plotly::ggplotly(f_plt) |>
#       plotly::config(displayModeBar = FALSE)
#   } else {
#     f_plt
#   }
# }
