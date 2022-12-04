seg_name_ph <- "Champions,
                Loyal Customers,
                Potential Loyalist,
                New Customers,
                Promising,
                Need Attention,
                About To Sleep,
                At Risk,
                Can't Lose Them, Lost"



segment_names_val <- c("Champions",
                       "Loyal Customers",
                       "Potential Loyalist",
                       "New Customers",
                       "Promising",
                       "Need Attention",
                       "About To Sleep",
                       "At Risk",
                       "Can't Lose Them",
                       "Lost")

recency_val <- list(c(4, 5), c(2, 5), c(3, 5), c(4, 5), c(3, 4),
                    c(2, 3), c(2, 3), c(1, 2), c(1, 1), c(1, 2))

f_m_val <- list(c(4, 5), c(3, 5), c(1, 3), c(1, 1), c(1, 1),
                c(2, 3), c(1, 2), c(2, 5), c(4, 5), c(1, 2))

# recency_val[[1]][1]
# recency_val[[1]][2]
#
# xx <- vector("numeric", 10)
#
# for (i in seq_len(10)) {
#   xx <- c(f_m_val[[i]][2], xx)
# }
#
# r_l <- xx[1:10]
# r_u <- xx[1:10]
#
# fm_l <- xx[1:10]
# fm_u <- xx[1:10]


#' Segment count summary
#'
#' @param dt data.table
#' @param sort logical whether to sort by segment count.
#' @param output_type the type of output, either 'plot' or 'table'
#' @param interactive logical, whether to returned an interactive plot or a
#' static plot.
#'
#' @return if output_type = "table" a data.table, a ggplot object if
#' interactive = FALSE else a plotly object.
#' @export
#'
#' @examples segment_count(dt, TRUE, 'plot' TRUE)
# segment_count <- function(dt, sort = TRUE, output_type = "plot",
#                           interactive = FALSE) {
#   f_dt <- data.table::copy(dt)
#   f_dt <- f_dt[, .(count = .N), keyby = .(segment)
#   ][order(-count)
#   ][,
#     `:=`(proportion = round(proportions(count)*100, 2))][]
#
#   if (output_type == "table") {
#     f_dt
#
#   } else if (output_type == "plot") {
#     template <- f_dt |>
#       ggplot2::ggplot() +
#       ggplot2::geom_col() +
#       ggplot2::geom_text(ggplot2::aes(label = paste0(proportion, "%"),
#                                       x = max(count)-1000)) +
#       ggplot2::labs(x = NULL, y = NULL) +
#       ggplot2::scale_x_continuous(labels = scales::comma_format()) +
#       ggplot2::scale_fill_gradient2(low = heatmap$low, mid = heatmap$mid, high = heatmap$high) +
#       ggplot2::theme_minimal() +
#       ggplot2::theme(plot.title.position = "plot",
#                      legend.position = "none")
#
#     # plt_title <- "Number of Customers In Each Segment"
#
#     if (sort) {
#       f_plt <- template %+%
#         ggplot2::aes(x = count, y = reorder(segment, count), fill = count) +
#         ggplot2::labs(x = NULL, y = NULL)
#
#     } else {
#       f_plt <- template %+%
#         ggplot2::aes(x = count, y = segment, fill = count) +
#         ggplot2::scale_y_discrete(limits = rev) +
#         ggplot2::labs(x = NULL, y = NULL)
#     }
#
#     if (interactive) {
#       plotly::ggplotly(f_plt, tooltip = c("x")) |>
#         plotly::config(displayModeBar = FALSE)
#     } else{
#       f_plt
#     }
#
#   } else {
#     stop("argument `output_type` must be either 'table' or 'plot'")
#   }
# }


#' Segment Aggregate tree map
#'
#' @param dt data.table with a segment variable.
#' @param agg_fun an aggregate function. it can be any of 'min', 'max', 'sum',
#' 'mean', 'median'
#' @param round logical, whether to round the summarized values.
#'
#' @return a plotly treemap
#' @export
#'
#' @examples segment_agg_treemap(dt, 'sum', FALSE)
# segment_agg_treemap <- function(dt, agg_fun, round = TRUE) {
#   fun <- rlang::as_closure(agg_fun)
#
#   if (round)  {
#     f_dt <- data.table::copy(dt)[,
#                                  .(recency = round(fun(recency_days), 2),
#                                    frequency = round(fun(transaction_count), 2),
#                                    monetary = round(fun(amount), 2),
#                                    count = .N),
#                                  keyby = .(segment)]
#   } else {
#     f_dt <- data.table::copy(df)[,
#                                  .(recency = fun(recency_days),
#                                    frequency = fun(transaction_count),
#                                    monetary = fun(amount),
#                                    count = .N),
#                                  keyby = .(segment)]
#   }
#   fun_label <- agg_label[[agg_fun]]
#   f_dt[,
#        `:=`(label_txt = paste0("|", segment," |<br>",
#                                fun_label,":<br> ",
#                                "Recency | ", scales::comma(recency), "<br> ",
#                                "Frequency | ", scales::comma(frequency), "<br> ",
#                                "Monetray | ", scales::comma(monetary)))][] |>
#     plotly::plot_ly(
#       labels = ~label_txt,
#       parents = NA,
#       values = ~monetary,
#       type = "treemap",
#       hovertext = ~paste0("Segment:: ", segment, "<br>",
#                           "Number of Customers:: ", scales::comma(count)),
#       hovertemplate = "%{hovertext}<extra></extra>",
#       marker = list(colors = treemap_color)
#     ) |>
#     plotly::config(displayModeBar = FALSE)
# }

# Not Used
#' Segment count tree map
#'
#' @param dt data.table with a segment variable.
#'
#' @return a plotly treemap
#' @export
#'
#' @examples
# segment_count_treemap <- function(dt) {
#   data.table::copy(dt)[,
#                        .(count = .N,
#                          recency = round(mean(recency_days), 2),
#                          frequency = round(mean(transaction_count), 2),
#                          monetary = round(mean(amount), 2)), keyby = .(segment)
#   ][, `:=`(proportion = round(proportions(count)*100, 2))
#   ][,
#     `:=`(label_txt = paste0("Segment | ", segment, "<br>",
#                             "Number of Customers | ", scales::comma(count),
#                             "<br>", "proportion | ", proportion, "%"))][] |>
#     plotly::plot_ly(
#       labels = ~label_txt,
#       parents = NA,
#       values = ~count,
#       type = "treemap",
#       hovertext = ~paste0("Average:<br>",
#                           "Recency: ", scales::comma(recency), "<br>",
#                           "Frequency: ", scales::comma(frequency), "<br>",
#                           "Monetary: ", scales::comma(monetary)),
#       hovertemplate = "%{hovertext}<extra></extra>",
#       marker = list(colors = treemap_color)
#     ) |>
#     plotly::config(displayModeBar = FALSE)
# }
