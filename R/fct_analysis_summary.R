#' RFM score count plot
#'
#' @param dt data.table
#' @param x_var score variable any of recency_score, frequency_score or monetary_score
#' @param y_var score variable any of recency_score, frequency_score or monetary_score
#' @return echarts4r htmlwidget object.
#' @export
#'
#' @examples score_count(dt, 'recency_score', 'monetary_score', TRUE)
#'
score_count <- function(dt, x_var, y_var) {
  x_lab <- clean_label(x_var, str_remove = "score")
  y_lab <- clean_label(y_var, str_remove = "score")
  plt_title <- paste("Percentage Of Customers By", x_lab, "&", y_lab, "Score")

  x_var_c <- clean_label(x_var)
  y_var_c <- clean_label(y_var)

  f_dt <- data.table::copy(dt)[, .(count = .N), keyby = c(x_var, y_var)
  ][, `:=`(proportion = round(proportions(count)*100, 2))][]

  data.table::setnames(f_dt,
                       old = c(x_var, y_var),
                       new = c("var_1", "var_2"))

  f_dt <- f_dt[, .(var_1 = factor(var_1),
                   var_2 = factor(var_2),
                   count = as.integer(count),
                   proportion = as.double(proportion))][]


  data.table::setnames(f_dt,
                       old = c("var_1", "var_2", "count"),
                       new = c(x_var_c, y_var_c, "Count"))

  f_dt |>
    echarts4r::e_charts_(x = x_var_c) |>
    echarts4r::e_heatmap_(y = y_var_c,
                          z = "proportion", label = list(show = TRUE)) |>
    echarts4r::e_heatmap_(y = y_var_c,
                          z = "Count",
                          itemStyle = list(emphasis = list(shadowBlur = 10))) |>
    echarts4r::e_x_axis(axisLabel = list(color = "#0F0F0F"),
                        name = x_lab,
                        nameLocation = "center",
                        nameGap = 35) |>
    echarts4r::e_y_axis(axisLabel = list(color = "#0F0F0F"),
                        name = y_lab,
                        nameLocation = "center",
                        nameGap = 35) |>
    echarts4r::e_title(text = plt_title,
                       textStyle = list(color = "#000000", fontWeight = "normal")) |>
    echarts4r::e_visual_map(Count,
                            color = c(heatmap$low, heatmap$mid, heatmap$high),
                            right = 10, top = 100) |>
    echarts4r::e_tooltip(trigger = "item",
                         formatter = htmlwidgets::JS(glue::glue(
                           "
          function(params) {
          return(
          '<<x_lab>> Score: ' + params.value[0] + '<br>' +
          '<<y_lab>> Score: ' + params.value[1] + '<br>' +
          'Count: ' + '<strong>' + echarts.format.addCommas(params.value[2]) + '</strong>'
          )
          }
          ",
          .open = "<<", .close = ">>"
                         )))
}



#' RFM score heat map plot
#'
#' @param dt data.table
#' @param agg_fun as aggregate function, it can be any of 'min', 'max', 'average',
#' 'sum', or 'median'
#'
#' @return echarts4r htmlwidget object.
#' @export
#'
#' @examples scores_amount_heatmap(dt, "sum" )
#'
scores_amount_heatmap <- function(dt, agg_fun) {
  fun <- rlang::as_closure(agg_fun)

  f_dt <- data.table::copy(dt)[,
                               .(aggregate = fun(amount)),
                               keyby = .(recency_score, frequency_score) ]
  f_dt <- f_dt[, .(recency_score = factor(recency_score),
                   frequency_score = factor(frequency_score),
                   aggregate = round( as.double(aggregate), 2))][]

  data.table::setnames(f_dt,
                       old = c("recency_score", "frequency_score", "aggregate"),
                       new = c("Recency Score", "Frequency Score", "aggregate"))

  plt_title <- paste(agg_label[[agg_fun]],
                     "Revenue By Frequency & Recency Score")

  f_dt |>
    echarts4r::e_charts_(x = "Recency Score") |>
    echarts4r::e_heatmap_(y = "Frequency Score",
                          z = "aggregate",
                          itemStyle = list(emphasis = list(shadowBlur = 10))) |>
    echarts4r::e_visual_map(aggregate,
                            color = c(heatmap$high, heatmap$low, heatmap$mid),
                            right = 10, top = 100) |>
    echarts4r::e_x_axis(axisLabel = list(color = "#0F0F0F"),
                        name = "Recency",
                        nameLocation = "center",
                        nameGap = 35) |>
    echarts4r::e_y_axis(axisLabel = list(color = "#0F0F0F"),
                        name = "Frequency",
                        nameLocation = "center",
                        nameGap = 35) |>
    echarts4r::e_title(text = plt_title,
                       textStyle = list(color = "#000000", fontWeight = "normal")) |>
    echarts4r::e_tooltip(trigger = "item",
                         formatter = htmlwidgets::JS(
                           "
          function(params) {
          return(
          'Recency Score: ' + params.value[0] + '<br>' +
          'Frequancy Score: ' + params.value[1] + '<br>' +
          'Count: ' + '<strong>' + echarts.format.addCommas(params.value[2]) + '</strong>'
          )
          }
          "
                         ))
}



#' Unique RFM score plot
#'
#' @param dt data.table
#' @param n number of scores to include.
#' @param sort sort scores by their count if TRUE else by the scores in descending
#' order
#'
#' @return echarts4r htmlwidget object.
#' @export
#'
#' @examples rfm_score_plot_count(dt, 10, FALSE)
#'
rfm_score_plot_count <- function(dt, n = 15, sort = TRUE) {
  if (sort) {
    f_dt <- data.table::copy(dt)[, .(count = .N), keyby = .(rfm_score)
          ][order(count)
            ][, `:=`(proportion = round(proportions(count)*100, 2),
                     rfm_score = factor(rfm_score))][]
  } else {
    f_dt <- data.table::copy(dt)[, .(count = .N), keyby = .(rfm_score)
          ][order(-rfm_score)
            ][, `:=`(proportion = round(proportions(count)*100, 2),
                     rfm_score = factor(rfm_score))][]
  }

  f_dt <- head(f_dt, n)

  if (nrow(f_dt) <= 20) {
    f_dt$color <- pal_pc[1:nrow(f_dt)]

  } else {
    pal_pc <- c(pal_pc, pal_pc)
    f_dt$color <- pal_pc[1:nrow(f_dt)]
  }

  f_dt |>
    echarts4r::e_charts(x = rfm_score) |>
    echarts4r::e_bar(serie = count, name = "Customers", legend = FALSE) |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_add_nested("itemStyle", color) |>
    echarts4r::e_tooltip(backgroundColor = "#FAFAFA") |>
    echarts4r::e_theme("macarons")
}


#' RFM score count table
#'
#' @param dt data.table
#'
#' @return summarized data.table
#' @export
#'
#' @examples
rfm_score_table_count <- function(dt) {
  data.table::copy(dt)[, .(count = .N),
           keyby = .(recency_score, frequency_score, monetary_score, rfm_score)
        ][order(-rfm_score)
          ][, `:=`(proportion = round(proportions(count)*100, 2))][]
}



#' Clean RFM score table names
#'
#' @param dt data.table names to clean.
#'
#' @return a data.table with cleaned names.
#' @export
#'
#' @examples
clean_rfm_score_name <- function(dt) {
  names(dt) <- clean_label(names(dt))
  data.table::setnames(
    dt,
    old = c("Rfm Score", "Count", "Proportion"),
    new = c("RFM Score", "Number of Customers", "Overall Precentage")
  )

  return(dt)
}



#' RFM score reactable
#'
#' @param dt data.table with cleaned names.
#'
#' @return an html table widget
#' @export
#'
#' @examples
rfm_score_reactable_count <- function(dt) {
  max_val <- max(dt[["Overall Precentage"]])
  min_val <- min(dt[["Overall Precentage"]])

  reactable::reactable(
    dt,
    defaultColDef = reactable::colDef(
      format = reactable::colFormat(separators = TRUE), na = "–"
    ),
    columns = list(
      `Number of Customers` = reactable::colDef(
        cell = reactablefmtr::pill_buttons(dt,
                                           box_shadow = TRUE,
                                           bold_text = TRUE,
                                           number_fmt = scales::comma_format(),
                                           colors = tbl_ki_bg_color_scale,
                                           opacity = 1)
      ),
      `Overall Precentage` = reactable::colDef(
        filterable = FALSE,
        cell = reactablefmtr::data_bars(dt,
                                        text_position = "above",
                                        fill_color = bar_color,
                                        background = tbl_header_bg_color,
                                        max_value = max_val,
                                        min_value = min_val,
                                        align_bars = "left",
                                        text_color = "black",
                                        bold_text = TRUE,
                                        box_shadow = TRUE,
                                        round_edges = TRUE)
      )
    ),
    highlight = TRUE,
    outlined = TRUE,
    compact = TRUE,
    fullWidth = TRUE,
    resizable = TRUE,
    filterable = TRUE,
    paginationType = "jump",
    showSortable = TRUE,

    theme = reactable::reactableTheme(
      borderColor = tbl_border_color,
      highlightColor = tbl_highlight_color,
      headerStyle = list(borderColor = tbl_header_border_color,
                         backgroundColor = tbl_header_bg_color,
                         fontSize = tbl_header_text_size),
      style = list(fontFamily = tbl_font_family)),

    language = reactable::reactableLang(pageInfo = "{rows} entries",
                                        pagePrevious = "\u276e",
                                        pageNext = "\u276f")
  )
}
