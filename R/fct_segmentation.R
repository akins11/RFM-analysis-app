
#' Extract bins
#'
#' @param lst a nested list containing the bins.
#' @param len number of levels.
#' @param pos the index of the extracted vector to return.
#'
#' @return
#' @export
#'
#' @examples ext_bins(lst, 2, 1)
ext_bins <- function(lst, len, pos) {
  get_lvl <- function(l, x, p) {
    l[[x]][[pos]]
  }

  lapply(seq_len(len), \(.x) get_lvl(lst, .x, pos)) |> unlist()
}



#' Get the minimum, maximum and step value of each RFM score
#'
#' @param dt data.table
#'
#' @return a list with names such as R, F, and M containing the minimum, maximum
#' and step values
#' @export
#'
#' @examples
rfm_min_max <- function(dt) {
  get_min_max_value <- function(dt, var) {
    c(round(min(dt[[var]]), 1),
      round(max(dt[[var]]), 1),
      round(max(dt[[var]])*0.1, 1))
  }
  output <- lapply(c("recency_score", "frequency_score", "monetary_score"),
                   \(.x) get_min_max_value(dt, .x))
  names(output) <- c("R", "F", "M")
  return(output)
}



#' Assign segment to RFM table
#'
#' @param dt data.table containing each RFM score.
#' @param segment_names a character vector containing the unique names of each
#' segments.
#' @param recency_lower a numeric vector containing the lower and upper range
#' @param recency_upper value of recency scores for each segment.
#'
#' @param frequency_lower a numeric vector containing the lower and upper range
#' @param frequency_upper value of frequency scores for each segment.
#'
#' @param monetary_lower a numeric vector containing the lower and upper range
#' @param monetary_upper value of monetary scores for each segment.
#'
#' @details the number of segments must correspond to the number of each lower
#' and upper values.
#'
#' @return a data.table with customer segments.
#' @export
#'
#' @examples assign_segment(dt, c("Champions", "Loyal Customers", "Potential Loyalist"),
#' c(4, 2, 3), c(5, 5, 5,), c(4, 3, 1), c(5, 5, 3,), c(4, 3, 1), c(5, 5, 3))
assign_segment <- function (dt, segment_names = NULL,
                            recency_lower = NULL, recency_upper = NULL,
                            frequency_lower = NULL, frequency_upper = NULL,
                            monetary_lower = NULL, monetary_upper = NULL)  {

  len <- lapply(list(recency_lower, recency_upper, frequency_lower,
                      frequency_upper, monetary_lower, monetary_upper,
                      segment_names),
                 \(.x) length(.x)) |>
    unlist()

  if (length(unique(len)) != 1) {
    stop("length of segment names & RFM lower and upper bound must be the same")
  }

  if (length(segment_names))
    f_dt <- data.table::copy(dt)[, `:=`(segment = 1)]

  for (i in seq_len(length(segment_names))) {
    f_dt$segment[(
      (data.table::between(f_dt$recency_score, recency_lower[i], recency_upper[i])) &
        (data.table::between(f_dt$frequency_score, frequency_lower[i], frequency_upper[i])) &
        (data.table::between(f_dt$monetary_score, monetary_lower[i], monetary_upper[i])) &
        !f_dt$segment %in% segment_names
    )] <- segment_names[i]
  }

  f_dt[is.na(segment), segment := "Others"]
  f_dt[segment == 1, segment := "Others"]
  f_dt[, `:=`(segment = factor(segment, levels = c(..segment_names, "Others")))]
  f_dt[, .(customer_id, segment, rfm_score,
           recency_score, frequency_score, monetary_score,
           recency_days, transaction_count, amount)]

}



#' Segment count summary
#'
#' @param dt data.table
#' @param sort logical whether to sort by segment count.
#' @param output_type the type of output, either 'plot' or 'table'
#'
#' @return if output_type = "table" a data.table, echarts4r htmlwidget object.
#' @export
#'
#' @examples segment_count(dt, TRUE, 'plot' TRUE)
#'
segment_count <- function(dt, sort = TRUE) {
  f_dt <- data.table::copy(dt)

  if (sort) {
    f_dt <- f_dt[, .(count = .N), keyby = .(segment)
          ][order(count)
            ][,
              `:=`(proportion = round(proportions(count)*100, 2))][]

  } else {
    f_dt <- f_dt[, .(count = .N), keyby = .(segment)
          ][order(-segment)
            ][,
              `:=`(proportion = round(proportions(count)*100, 2))][]
  }

  if (nrow(f_dt) <= 20) {
    f_dt$color <- pal_pc[1:nrow(f_dt)]

  } else {
    pal_pc <- c(pal_pc, pal_pc)
    f_dt$color <- pal_pc[1:nrow(f_dt)]
  }

  f_dt |>
    echarts4r::e_charts(x = segment) |>
    echarts4r::e_bar(serie = count, name = "Count", legend = FALSE, stack = "grp") |>
    echarts4r::e_bar(serie = proportion,
                     name = "Proportion",
                     label = list(show = TRUE, offset = c(20, 0)),
                     stack = "grp",
                     legend = FALSE) |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_add_nested("itemStyle", color) |>
    echarts4r::e_tooltip(backgroundColor = "#FAFAFA") |>
    echarts4r::e_grid(left = 125) |>
    echarts4r::e_color(c("#00FFFF", "#FFFFFF")) |>
    echarts4r::e_theme("macarons")
}





#' Segment count table
#'
#' @param dt data.table with a segment variable.
#'
#' @return a summarized data.table.
#' @export
#'
#' @examples
segment_count_table <- function(dt) {
  f_dt <- data.table::copy(dt)[, .(count = .N), keyby = .(segment, rfm_score)]
  segments <- as.character(unique(f_dt$segment))

  f_split_dt <- split(f_dt, by = "segment")
  f_split_dt <- lapply(segments, function(.x) {
    if (is.data.frame(f_split_dt[[.x]])) {
      f_split_dt[[.x]][, `:=`(within_segment = round(proportions(count)*100, 2))][]
    } else {
      f_split_dt[[.x]]
    }
  })
  names(f_split_dt) <- segments

  f_dt <- do.call("rbind", f_split_dt)

  f_dt[, `:=`(overall_percentage = round( proportions(count)*100, 2))][]
}



#' Segment Aggregate tree map
#'
#' @param dt data.table with a segment variable.
#' @param agg_fun an aggregate function. it can be any of 'min', 'max', 'sum',
#' 'mean', 'median'
#' @param round logical, whether to round the summarized values.
#'
#' @return a eacharts4r treemap
#' @export
#'
#' @examples segment_agg_treemap(dt, 'sum', FALSE)
#'
segment_agg_treemap <- function(dt, by = "count", agg_fun, round = TRUE) {
  fun <- rlang::as_closure(agg_fun)

  if (round)  {
    f_dt <- data.table::copy(dt)[,
                                 .(recency = round(fun(recency_days), 2),
                                   frequency = round(fun(transaction_count), 2),
                                   monetary = round(fun(amount), 2),
                                   count = .N),
                                 keyby = .(segment)]
  } else {
    f_dt <- data.table::copy(df)[,
                                 .(recency = fun(recency_days),
                                   frequency = fun(transaction_count),
                                   monetary = fun(amount),
                                   count = .N),
                                 keyby = .(segment)]
  }

  data.table::setnames(f_dt,
                       old = c("segment", by),
                       new = c("name", "value"))

  f_dt |>
    echarts4r::e_charts() |>
    echarts4r::e_treemap(itemStyle = list(emphasis = list(shadowBlur = 10))) |>
    echarts4r::e_tooltip() |>
    echarts4r::e_color(treemap_color)
}




# Not Used
#' Gather unique segment values.
#'
#' @param values a string containing segment names
#'
#' @return a character vector
#' @export
#'
#' @examples get_segment_names("Champions", "Loyal Customers", "Potential Loyalist")
get_segment_names <- function(values) {
  strsplit(values, split = ",") |>
    unlist() |>
    trimws() |>
    as.character()
}



#' Clean segment table names
#'
#' @param dt data.table
#'
#' @return a data.table with cleaned names
#' @export
#'
#' @examples
clean_seg_data_name <- function(dt) {
  names(dt) <- clean_label(names(dt))

  data.table::setnames(
    dt,
    old = c("Customer Id", "Rfm Score", "Transaction Count", "Amount"),
    new = c("Customer ID", "RFM Score", "Number Of Transaction", "Revenue")
    )
  return(dt)
}



#' Segment reactable
#'
#' @param dt a data.table with cleaned names
#'
#' @return an html table widget
#' @export
#'
#' @examples
segment_reactable <- function(dt) {
  reactable::reactable(
    dt,
    defaultColDef = reactable::colDef(
      format = reactable::colFormat(separators = TRUE), na = "???"
    ),
    columns = list(
      `Customer ID` = reactable::colDef(
        align = "center",
        format = reactable::colFormat(),
        filterable = FALSE,
        sortable = FALSE,
        style = list(background = tbl_header_bg_color,
                     color = "black")
      ),
      Segment = reactable::colDef(
        align = "center",
        headerStyle = list(background = tbl_ki_bg_color,
                           borderBottomColor = "#707070"),
        style = list(background = tbl_ki_bg_color,
                     color = "black",
                     textSize = tbl_ki_text_size,
                     fontWeight = "bold",
                     borderRight = paste("1px solid", tbl_ki_bg_color),
                     borderLeft = paste("1px solid", tbl_ki_bg_color)
                     # border = paste("1px solid", tbl_ki_bg_color)
        )
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



#' Clean segment count table names
#'
#' @param dt data.table
#'
#' @return a data.table with cleaned names
#' @export
#'
#' @examples
clean_seg_count_name <- function(dt) {
  names(dt) <- clean_label(names(dt))

  data.table::setnames(dt,
                       old = c("Count", "Rfm Score", "Within Segment",
                               "Overall Percentage"),
                       new = c("Number Of Customers", "RFM Score",
                               "Within Segment(%)", "Overall(%)"))
  return(dt)
}



#' Segment count reactable
#'
#' @param dt a data.table with cleaned names
#'
#' @return an html table widget
#' @export
#'
#' @examples
seg_count_reactable <- function(dt) {
  min_ws <- min(dt$`Within Segment(%)`)
  max_ws <- max(dt$`Within Segment(%)`)
  min_oa <- min(dt$`Overall(%)`)
  max_oa <- max(dt$`Overall(%)`)

  reactable::reactable(
    data = dt,
    defaultColDef = reactable::colDef(
      format = reactable::colFormat(separators = TRUE), na = "???"
    ),
    columns = list(
      Segment = reactable::colDef(
        align = "left",
        style = list(background = tbl_ki_bg_color,
                     color = "black",
                     textSize = "8px",
                     borderRight = paste("1px solid", tbl_ki_bg_color),
                     position = "sticky", left = 0, zIndex = 1),
        headerStyle = list(position = "sticky", left = 0, zIndex = 1,
                           background = tbl_ki_bg_color,
                           borderBottomColor = "#707070")
      ),

      `Within Segment(%)` = reactable::colDef(
        cell = reactablefmtr::data_bars(data = dt,
                                        text_position = "inside-base",
                                        fill_color = bar_color,
                                        bold_text = TRUE,
                                        background = tbl_header_bg_color,
                                        min_value = min_ws,
                                        max_value = max_ws,
                                        box_shadow = TRUE)
      ),

      `Overall(%)` = reactable::colDef(
        cell = reactablefmtr::data_bars(data = dt,
                                        text_position = "inside-base",
                                        fill_color = bar_color,
                                        bold_text = TRUE,
                                        background = tbl_header_bg_color,
                                        min_value = min_oa,
                                        max_value = max_oa,
                                        box_shadow = TRUE)
      )
    ),
    # defaultPageSize = 5,
    highlight = TRUE,
    outlined = TRUE,
    compact = TRUE,
    fullWidth = TRUE,
    resizable = TRUE,
    filterable = TRUE,
    showSortable = TRUE,
    paginationType = "jump",

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
