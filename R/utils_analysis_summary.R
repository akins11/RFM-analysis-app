#' RFM select input
#'
#' @param select_id select input id.
#' @param label select input label.
#' @param sel default selected choice.
#'
#' @return
#' @export
#'
rfm_score_picker_input <- function(select_id, label, sel) {
  shinyWidgets::pickerInput(
    inputId = select_id,
    label = label,
    choices = c("Recency Score" = "recency_score",
                "Frequency Score" = "frequency_score",
                "Monetary Score" = "monetary_score"),
    selected = sel,
    options = shinyWidgets::pickerOptions(title = "Nothing Selected",
                                          style = "btn-default")
  )
}



#' RFM score count plot
#'
#' @param dt data.table
#' @param x_var score variable any of recency_score, frequency_score or monetary_score
#' @param y_varscore variable any of recency_score, frequency_score or monetary_score
#' @param interactive logical, whether to returned an interactive plot or a
#' static plot.
#'
#' @return a ggplot object if interactive = FALSE else a plotly object.
#' @export
#'
#' @examples score_count(dt, 'recency_score', 'monetary_score', TRUE)
# score_count <- function(dt, x_var, y_var, interactive = FALSE) {
#   x_label <- clean_label(x_var, str_remove = "score")
#   y_label <- clean_label(y_var, str_remove = "score")
#   plt_title <- paste("Percentage Of Customers By",
#                      x_label, "&", y_label, "Score")
#
#   x_var_c <- clean_label(x_var)
#   y_var_c <- clean_label(y_var)
#
#   f_dt <- data.table::copy(dt)[, .(count = .N), keyby = c(x_var, y_var)
#   ][, `:=`(proportion = round(proportions(count)*100, 2))][]
#
#   data.table::setnames(f_dt,
#                        old = c(x_var, y_var),
#                        new = c("var_1", "var_2"))
#
#   f_dt <- f_dt[, .(var_1 = factor(var_1),
#                    var_2 = factor(var_2),
#                    count = as.integer(count),
#                    proportion = as.double(proportion))][]
#
#
#   data.table::setnames(f_dt,
#                        old = c("var_1", "var_2", "count"),
#                        new = c(x_var_c, y_var_c, "Count"))
#
#
#   x_lab <- clean_label(x_var, str_remove = "score")
#   y_lab <- clean_label(y_var, str_remove = "score")
#
#   f_plt <- f_dt |>
#     ggplot2::ggplot(ggplot2::aes(x = .data[[x_var_c]],
#                                  y = .data[[y_var_c]], fill = Count)) +
#     ggplot2::geom_tile() +
#     ggplot2::geom_text(ggplot2::aes(label = paste0(proportion, "%")),
#                        alpha = 0.5, size = 4) +
#     ggplot2::scale_fill_gradient2(low = heatmap$low, mid = heatmap$mid, high = heatmap$high,
#                                   midpoint = mean(f_dt$Count),
#                                   labels = scales::comma_format()) +
#     ggplot2::labs(x = x_lab, y = y_lab, fill = "Count", title = plt_title) +
#     ggplot2::theme(panel.background = ggplot2::element_blank(),
#                    axis.ticks = ggplot2::element_blank())
#
#   if (interactive) {
#     plotly::ggplotly(f_plt, tooltip = c("x", "y", "fill")) |>
#       plotly::config(displayModeBar = FALSE)
#   } else {
#     f_plt
#   }
# }



#' RFM score heat map plot
#'
#' @param dt data.table
#' @param agg_fun as aggregate function, it can be any of 'min', 'max', 'average',
#' 'sum', or 'median'
#' @param interactive logical, whether to returned an interactive plot or a
#' static plot.
#'
#' @return a ggplot object if interactive = FALSE else a plotly object.
#' @export
#'
#' @examples scores_amount_heatmap(dt, "sum" TRUE)
# scores_amount_heatmap <- function(dt, agg_fun, interactive = FALSE) {
#   fun <- rlang::as_closure(agg_fun)
#
#   f_dt <- data.table::copy(dt)[,
#                                .(aggregate = fun(amount)),
#                                keyby = .(recency_score, frequency_score) ]
#   f_dt <- f_dt[, .(recency_score = factor(recency_score),
#                    frequency_score = factor(frequency_score),
#                    aggregate = as.double(aggregate))][]
#
#   data.table::setnames(f_dt,
#                        old = c("recency_score", "frequency_score", "aggregate"),
#                        new = c("Recency Score", "Frequency Score", "aggregate"))
#
#   plt_title <- paste(agg_label[[agg_fun]],
#                      "Revenue By Frequency & Recency Score")
#
#   f_plt <- f_dt |>
#     ggplot2::ggplot(ggplot2::aes(x = `Recency Score`,
#                                  y = `Frequency Score`, fill = aggregate)) +
#     ggplot2::geom_tile(show.legend = FALSE) +
#     ggplot2::geom_text(ggplot2::aes(label = scales::comma(aggregate, 1)),
#                        size = 3.5, alpha = 0.5) +
#     ggplot2::scale_fill_gradient2(low = heatmap$low, mid = heatmap$mid, high = heatmap$high,
#                                   midpoint = mean(f_dt$aggregate)) +
#     ggplot2::labs(x = "Recency", y = "Frequency", title = plt_title) +
#     ggplot2::theme(panel.background = ggplot2::element_blank(),
#                    axis.ticks = ggplot2::element_blank())
#
#   if (interactive) {
#     plotly::ggplotly(f_plt, tooltip = c("x", "y")) |>
#       plotly::config(displayModeBar = FALSE)
#   } else {
#     f_plt
#   }
# }


#' Unique RFM score plot
#'
#' @param dt data.table
#' @param n number of scores to include.
#' @param sort sort scores by their count if TRUE else by the scores in descending
#' order
#'
#' @return a ggplot object if interactive = FALSE else a plotly object.
#' @export
#'
#' @examples rfm_score_plot_count(dt, 10, FALSE)
# rfm_score_plot_count <- function(dt, n = 15, sort = TRUE) {
#   f_dt <- data.table::copy(dt)[, .(count = .N), keyby = .(rfm_score)
#   ][order(-count)
#   ][, `:=`(proportion = round(proportions(count)*100, 2),
#            rfm_score = factor(rfm_score))][]
#
#   if (sort) {
#     f_plt <- head(f_dt, n) |>
#       ggplot2::ggplot(ggplot2::aes(y = reorder(rfm_score, count), x = count))
#
#   } else {
#     f_plt <- head(f_dt, n) |>
#       ggplot2::ggplot(ggplot2::aes(y = rfm_score, x = count))
#   }
#   f_plt <- f_plt +
#     ggplot2::geom_col(ggplot2::aes(text = paste0(proportion, "%"), fill = count)) +
#     ggplot2::scale_fill_gradient2(low = heatmap$low, mid = heatmap$mid, high = heatmap$high,
#                                   midpoint = 1600, labels = scales::comma_format()) +
#     ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(axis.text.x = ggplot2::element_blank(),
#                    axis.text.y = ggplot2::element_text(face = "bold", size = 12, color = "#7A7A7A"),
#                    panel.grid.major.x = ggplot2::element_blank(),
#                    panel.grid.minor.x = ggplot2::element_blank(),
#                    legend.position = "bottom",
#                    legend.key.width = ggplot2::unit(2, "cm"),
#                    legend.text = ggplot2::element_text(face = "bold", size = 10, color = "#7A7A7A"))
#
#   plotly::ggplotly(f_plt, tooltip = "text")|>
#     plotly::config(displayModeBar = FALSE)
# }
