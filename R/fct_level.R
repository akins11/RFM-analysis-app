# copy

#' collect unique customer ids
#'
#' @param dt data.table
#' @param c_segment a segment name
#'
#' @return a vector of unique customer ids
#' @export
#'
#' @examples get_customer_id(dt, "Champions")
get_customer_id <- function(dt, c_segment) {
  data.table::copy(dt)[segment == c_segment, customer_id]
  # data.table::copy(dt)[c_segment, on = "segment"]
}



#' Extract customers from a particular segment
#'
#' @param dt data.table
#' @param selected_customer_id a vector of unique customer ids
#'
#' @return a data.table of a particular segment
#' @export
#'
#' @examples extract_customers(dt, c(1, 2587, 3697, 1247, 15, 258, 78, 658))
extract_customers <- function(dt, selected_customer_id) {
  if (length(selected_customer_id) != 0) {
    dt <- janitor::clean_names(dt)

    data.table::copy(dt)[customer_id %in% selected_customer_id]

  } else {
    stop("selected_customer_id must have at least one value.")
  }
}



#' Count variables within a particular segment
#'
#' @param dt data.table
#' @param by variable to count. it can either be 'product' or 'customer_id'
#' @param n number of unique values to return sorted in descending order
#'
#' @return a summarized data.table
#' @export
#'
#' @examples within_segment_count(dt, 'product' 10)
within_segment_count <- function(dt, by, n) {
  if (by == "product") {
    f_dt <- data.table::copy(dt)[, .(count = .N), keyby = .(product)]

  } else if (by == "customer_id") {
    f_dt <- data.table::copy(dt)[, .(count = .N), keyby = .(customer_id)]

  } else {
    stop("argument `by` must be either 'product' or 'customer_id'")
  }

  f_dt <- f_dt[, `:=`(proportion = round(proportions(count)*100, 2))
  ][order(-count)]

  if (!missing(n)) head(f_dt, n) else f_dt
}



#' Numeric summary variables within a particular segment
#'
#' @param dt data.table
#' @param variable variable to summarise.
#' @param gp_var a variable to group by.
#' @param order_var a variable to arrange the data by. it can be any of 'min',
#' 'max', 'mean', 'median', 'sum'
#'
#' @return a summarise data.table
#' @export
#'
#' @examples within_segment_summary(dt, 'product' 'segment' 'mean')
within_segment_summary <- function(dt, variable, gp_var, order_var) {
  f_dt <- data.table::copy(dt)

  f_dt <- f_dt[, .(min  = min(.SD[[1]]),
                   mean = mean(.SD[[1]]),
                   median = median(.SD[[1]]),
                   max  = max(.SD[[1]]),
                   sum  = sum(.SD[[1]])),
               keyby = c(gp_var), .SDcols = c(variable) ]

  data.table::setorderv(f_dt, order_var, -1)

  return(f_dt)
}



#' Summary of products within a particular segment
#'
#' @param dt data.table
#' @param agg_fun an aggregate function. it can be any of 'min', 'max', 'mean',
#'  'median', 'sum'
#' @param position 'top' or 'bottom'
#' @param n_categories number of products to add to the plot.
#' @param by the summarized variable.
#' @param segment the name of the segment.
#'
#' @return a ggplot object
#' @export
#'
#' @examples product_by_segment(dt, 'median', 'top', 10, 'Revenue', 'Champions')
#'
product_by_segment <- function(dt,
                               agg_fun, position, n_categories = 10,
                               by, segment) {

  plt_title <- glue::glue("{clean_label(position)} {n_categories} Purchased Products In `{clean_label(segment)}` Segment")
  sub_title <- glue::glue("By {agg_label[[agg_fun]]} {clean_label(by)}")

  names(dt) <- tolower(names(dt))

  if (all(c("minimum", "maximum") %in% names(dt))) {
    data.table::setnames(
      dt,
      old = c("minimum", "maximum"),
      new = c("min", "max")
    )
  }

  if (position == "top") {
    f_dt <- head(dt, n_categories)
    f_dt <- f_dt[order(get(agg_fun))]

  } else if (position == "bottom") {
    f_dt <- tail(dt, n_categories)

  } else {
    stop("argument `direction` can only be either 'top' or 'bottom'")
  }

  if (nrow(f_dt) <= 20) {
    f_dt$color <- pal_pc[1:nrow(f_dt)]

  } else {
    pal_pc <- c(pal_pc, pal_pc)
    f_dt$color <- pal_pc[1:nrow(f_dt)]
  }

  f_dt |>
    echarts4r::e_charts(x = product) |>
    echarts4r::e_bar(serie = sum, name = "Total", legend = FALSE) |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_add_nested("itemStyle", color) |>
    echarts4r::e_grid(left = 140) |>
    echarts4r::e_title(text = plt_title, subtext = sub_title,
                       textStyle = list(fontWeight = "normal"),
                       subtextStyle = list(fontWeight = "lighter")) |>
    echarts4r::e_tooltip(backgroundColor = "#FAFAFA") |>
    echarts4r::e_theme("macarons")
}


#' Minimum and maximum plot
#'
#' @param dt data.table
#' @param by the summarized variable
#' @param segment the name of the segment.
#' @param n number of products to add to the plot.
#' @param max Boolean, create a max summary if true else a minimum summary
#'
#' @return echarts4r htmlwidget object.
#' @export
#'
#' @examples min_max_summary(dt, 'quantity', 'Champions', 10, TRUE)
#'
min_max_summary <- function(dt, by = NULL, segment = NULL, n = 10, max = TRUE) {

  f_tbl <- dt[order(-max)] |>
    head(n) |>
    echarts4r::e_charts(x = product)

  if (max) {
    f_tbl <- f_tbl |>
      echarts4r::e_bar(serie = max, name = "Maximum") |>
      echarts4r::e_grid(left = "25%") |>
      echarts4r::e_flip_coords() |>
      echarts4r::e_y_axis(inverse = TRUE) |>
      echarts4r::e_legend(left = 20, top = 25) |>
      echarts4r::e_color("#9400D3")

  } else {

    f_tbl <- f_tbl |>
      echarts4r::e_bar(serie = min, name = "Minimum") |>
      echarts4r::e_grid(right = "25%") |>
      echarts4r::e_flip_coords() |>
      echarts4r::e_y_axis(position = "right", inverse = TRUE) |>
      echarts4r::e_x_axis(inverse = TRUE) |>
      echarts4r::e_legend(right = 20, top = 25) |>
      echarts4r::e_color("#00FFFF")
  }

  f_tbl |>
    echarts4r::e_tooltip(backgroundColor = "#FAFAFA") |>
    echarts4r::e_theme("macarons")
}



# /!\ Not Optimized using best data.table features, still slow
dt_customer_top_purchase <- function(dt, n) {
  f_dt <- dt[, .(customer_id, product, revenue, quantity)]

  f_dt <- dt[dt[, .I[data.table::frank(dplyr::desc(revenue),
                                       ties.method = "min",
                                       na.last = "keep") <= min(1L, .N)],
                by = .(customer_id)]$V1
  ][order(customer_id, -revenue)
  ][order(-revenue)]

  if(!missing(n)) head(f_dt, n) else f_dt
}


#' Top customer purchase table.
#'
#' @param dt data.table
#' @param n number of unique customers to plot based on the sorted revenue in
#' descending order
#'
#' @return a tibble
#' @export
#'
#' @examples dp_customer_top_purchase(dt, 100)
dp_customer_top_purchase <- function(dt, n) {
  f_tbl <- dt |>
    dplyr::group_by(customer_id) |>
    dplyr::slice_max(order_by = revenue) |>
    dplyr::select(customer_id, product, quantity, revenue) |>
    dplyr::arrange(dplyr::desc(revenue)) |>
    dplyr::ungroup()

  if(!missing(n)) head(f_tbl, n) else f_tbl
}



#' A single product summary
#'
#' @param dt data.table
#' @param s_product the selected product
#' @param agg_fun an aggregate function. it can be any of 'min', 'max', 'mean',
#' 'median' 'sum'
#'
#' @return a summarise data.table
#' @export
#'
#' @examples one_product_summary(dt, 'dog food' 'min')
one_product_summary <- function(dt, s_product, agg_fun) {
  fun <- rlang::as_closure(agg_fun)
  fun_lab <- agg_label[[agg_fun]]
  tbl_title <- paste(fun_lab, "Sales Amount & Quantity Ordered")

  data.table::copy(dt)[product == s_product, .(customer_id, quantity, revenue)
                      ][, .(transaction_count = .N,
                            revenue = fun(revenue),
                            quantity = fun(quantity)), keyby = .(customer_id)
                        ][order(-revenue)]
}



#' collect the unique segments
#'
#' @param dt data.table with a segment variable
#'
#' @return a character vector of unique segments in the table.
#' @export
#'
#' @examples
get_segments <- function(dt) {
  as.character(unique(dt[["segment"]]))
}



#' Clean withing segment names
#'
#' @param dt data.table
#' @param by either 'customer_id' or 'product'
#'
#' @return a data.table with cleaned names
#' @export
#'
#' @examples clean_within_segment_names(dt, 'product')
clean_within_segment_names <- function(dt, by) {
  names(dt) <- clean_label(names(dt))

  if (by == "customer_id") {
    data.table::setnames(
      dt,
      old = c("Customer Id", "Count", "Proportion"),
      new = c("Customer ID", "Number Of Purchase", "Percentage")
      )

  } else if (by == "product") {
    data.table::setnames(
      dt,
      old = c("Product", "Count", "Proportion"),
      new = c("Product", "Number Of Purchase", "Percentage")
      )
  } else {
    stop("`by` must be 'customer_id' or 'product'")
  }

  return(dt)
}



#' Within segment count reactable
#'
#' @param dt data.table with cleaned names
#'
#' @return an html table widget
#' @export
#'
#' @examples
within_seg_count_reactable <- function(dt) {
  reactable::reactable(
    data = dt,
    defaultColDef = reactable::colDef(
      format = reactable::colFormat(separators = TRUE), na = "–"
    ),
    columns = list(
      `Customer ID` = reactable::colDef(
        align = "center",
        filterable = FALSE,
        sortable = FALSE,
        format = reactable::colFormat()
      ),
      `Number Of Purchase` = reactable::colDef(
        align = "center",
        cell = reactablefmtr::pill_buttons(data =  dt,
                                           box_shadow = TRUE,
                                           bold_text = TRUE,
                                           number_fmt = scales::comma_format(),
                                           colors = tbl_ki_bg_color_scale,
                                           opacity = 0.8)
      ),
      Percentage = reactable::colDef(
        filterable = FALSE,
        cell = reactablefmtr::data_bars(data = dt,
                                        text_position = "inside-base",
                                        fill_color = bar_color,
                                        text_color = "#FFFFFF",
                                        background = tbl_header_bg_color,
                                        box_shadow = TRUE,
                                        max_value = max(dt$Percentage),
                                        min_value = min(dt$Percentage))
      )
    ),
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
                         fontSize = "15px"),
      style = list(fontFamily = tbl_font_family)),

    language = reactable::reactableLang(pageInfo = "{rows} entries",
                                        pagePrevious = "\u276e",
                                        pageNext = "\u276f")
  )
}



#' Clean top product table names
#'
#' @param dt data.table
#'
#' @return a data.table with cleaned names
#' @export
#'
#' @examples
clean_top_prod_names <- function(dt) {
  names(dt) <- clean_label(names(dt))

  dplyr::rename(dt,
                `Customer ID` = `Customer Id`)
}



#' Top customer Purchase reactable
#'
#' @param dt a data.table with cleaned names
#'
#' @return an html table widget
#' @export
#'
#' @examples
cus_top_purchase_reactable <- function(dt) {
  reactable::reactable(
    data = dt,
    defaultColDef = reactable::colDef(
      format = reactable::colFormat(separators = TRUE), na = "–"
    ),
    columns = list(
      `Customer ID` = reactable::colDef(
        align = "center",
        filterable = FALSE,
        sortable = FALSE,
        format = reactable::colFormat(),
        style = list(background = tbl_header_bg_color,
                     color = tbl_ki_text_color,
                     textSize = tbl_ki_text_size)
      ),
      Revenue = reactable::colDef(
        cell = reactablefmtr::data_bars(data = dt,
                                        text_position = "inside-base",
                                        fill_color = bar_color,
                                        text_color = "#FFFFFF",
                                        background = tbl_header_bg_color,
                                        number_fmt = scales::comma_format(2),
                                        max_value = max(dt$Revenue),
                                        min_value = min(dt$Revenue),
                                        box_shadow = TRUE)
      ),
      Quantity = reactable::colDef(
        cell = reactablefmtr::pill_buttons(data = dt,
                                           colors = tbl_ki_bg_color_scale,
                                           opacity = 0.8,
                                           bold_text = TRUE,
                                           box_shadow = TRUE)
      )
    ),
    filterable = TRUE,
    highlight = TRUE,
    outlined = TRUE,
    compact = TRUE,
    fullWidth = TRUE,
    resizable = TRUE,
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



#' Get unique top revenue products
#'
#' @param dt data.table
#' @param n number of products
#'
#' @return a character vector of unique top products based on revenue generated
#' @export
#'
#' @examples get_unique_top_revenue_product(dt, 15)
get_unique_top_revenue_product <- function(dt, n = 20) {
  sumry_prod <- dt[, .(revenue = sum(revenue)),  by = .(product)
                  ][order(-revenue)] |>
                    head(n)

  return(sumry_prod$product)
}



#' Clean one product table names
#'
#' @param dt data.table
#'
#' @return a data.table with cleaned names
#' @export
#'
#' @examples
clean_one_prod_name <- function(dt) {
  names(dt) <- clean_label(names(dt))

  data.table::setnames(dt,
                       old = c("Customer Id", "Transaction Count"),
                       new = c("Customer ID", "Number Of Transcation"))
  return(dt)
}



#' One product reactable
#'
#' @param dt a data.table with cleaned names
#'
#' @return an html table widget
#' @export
#'
#' @examples
one_product_reactable <- function(dt) {
  reactable::reactable(
    data = dt,
    defaultColDef = reactable::colDef(
      format = reactable::colFormat(separators = TRUE), na = "–"
    ),
    columns = list(
      `Customer ID` = reactable::colDef(
        align = "left",
        format = reactable::colFormat(),
        filterable = FALSE,
        sortable = FALSE,
        style = list(background = tbl_header_bg_color)
      ),
      `Number Of Transcation` = reactable::colDef(
        align = "center",
        cell = reactablefmtr::pill_buttons(data = dt,
                                           colors =  tbl_ki_bg_second_color_scale,
                                           opacity = 0.9,
                                           bold_text = TRUE,
                                           box_shadow = TRUE)
      ),
      Revenue = reactable::colDef(
        align = "center",
        cell = reactablefmtr::pill_buttons(data = dt,
                                           colors = tbl_ki_bg_color_scale,
                                           opacity = 0.9,
                                           number_fmt = scales::comma_format(2),
                                           bold_text = TRUE,
                                           box_shadow = TRUE)
      ),
      Quantity = reactable::colDef(
        align = "center",
        cell = reactablefmtr::pill_buttons(data = dt,
                                           colors = tbl_ki_bg_second_color_scale,
                                           opacity = 0.9,
                                           number_fmt = scales::comma_format(2),
                                           bold_text = TRUE,
                                           box_shadow = TRUE)
      )
    ),
    filterable = TRUE,
    highlight = TRUE,
    outlined = TRUE,
    compact = TRUE,
    fullWidth = TRUE,
    resizable = TRUE,
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

