#' Change a string of value to a numeric vector
#'
#' @param bin character value.
#'
#' @return numeric vector
#' @export
#'
#' @examples get_user_bins("12, 15, 47, 18")
get_user_bins <- function(bin) {
  f_num <- strsplit(bin, "(,)|(\\s)") |>
    unlist() |>
    trimws() |>
    as.numeric()

  f_num[!is.na(f_num)]
}



#' Check if bins are within the minimum and maximum values
#'
#' @param dt data.table
#' @param var either 'recency', 'frequency' or 'monetary' variable
#' @param bins the bins to check.
#' @param type the type of output, it can be either 'message' or 'value'.
#'
#' @return if check is true and type = message, a message is returned, if type =
#' 'value' the outlier value(s). if check is false no value is returned
#' @export
#'
#' @examples check_within_range(dt, 'monetary' c(0.14, 50, 148, 200), 'message')
check_within_range <- function(dt, var, bins, type = "message") {
  var_min <- min(dt[[var]])
  var_max <- max(dt[[var]])

  if (min(bins) < var_min) {
    invalid_min <- bins[bins < var_min]
    if (type == "message") {
      s_p_min <- ifelse(length(invalid_min) == 1,
                        "is a minimum value",
                        "are minimum values")
      message(
        paste("There", s_p_min, "of", paste(invalid_min, collapse = ", "),
              "lower than the minimum value", var_min, "of", var)
      )
    } else if (type == "value") {
      min_output <- list(position = "min", value = invalid_min)
    }
  }

  if (max(bins) > var_max) {
    invalid_max <- bins[bins > var_max]
    if (type == "message") {
      s_p_max <- ifelse(length(invalid_max) == 1,
                        "is a maximum value",
                        "are maximum values")
      message(
        paste("There", s_p_max, "of", paste(invalid_max, collapse = ", "),
              "higher than the maximum value", var_max, "of", var)
      )
    } else if (type == "value") {
      max_output <- list(position = "max", value = invalid_max)
    }
  }

  if (exists("min_output") && exists("max_output")) {
    return(list(min_output, max_output))
  } else if (exists("min_output") && !exists("max_output")) {
    return(min_output)
  } else if (!exists("min_output") && exists("max_output")) {
    return(max_output)
  }
}



#' Check if bins have the minimum and maximum values
#'
#' @param dt data.table
#' @param var either 'recency', 'frequency' or 'monetary' variable
#' @param bins the bins to check.
#' @param type the type of output, it can be either 'error' or 'logical'
#'
#' @details if check is true and type = 'error' an error is returned, if type =
#' 'logical' a Boolean indicating if the check is TRUE or FALSE.
#'
#' @return if check is true and type = 'logical' a list containing the bool,
#' error type, the min, max or min & max value
#' @export
#'
#' @examples check_include_range(dt, 'frequency' c(1, 5, 18, 36, 77), 'error')
check_include_range <- function(dt, var, bins, type = "error") {
  var_min <- min(dt[[var]])
  var_max <- max(dt[[var]])

  if (min(bins) > var_min && max(bins) < var_max) {
    if (type == "error") {
      stop(
        paste(var, "bins does not include minimum of", var_min, "and maximum value of",
              var_max, ", returned NA")
      )
    } else if (type == "logical") {
      list(logical = TRUE, error = "min-max", value = c(var_min, var_max))
    }
  } else if (min(bins) > var_min) {
    if (type == "error") {
      stop(paste(var, "bins dose not include minimum value of", var_min, ", returned NA"))
    } else if (type == "logical") {
      list(logical = TRUE, error = "min", value = var_min)
    }
  } else if (max(bins) < var_max) {
    if (type == "error") {
      stop(paste(var, "bins dose not include maximum value of", var_max, ", returned NA"))
    } else if (type == "logical") {
      list(logical = TRUE, error = "max", value = var_max)
    }
  } else {
    if (type == "logical") {
      list(logical = FALSE)
    }
  }
}



#' App min max error message
#'
#' @param df a data.table with recency, frequency & monetary variables
#' @param bin_list a list containing values such as r, f, m
#'
#' @return a list containing a boolean value the name of the metric and the
#' error message.
#' @export
#'
#' @examples m_include_range(df, list(r = c(2, 10, 35), f = c(1, 23, 45),
#' m = c(0.14, 78, 189)))
m_include_range <- function(df,  bin_list) {
  r_l <- check_include_range(df, "recency",  bin_list$r, "logical")
  f_l <- check_include_range(df, "frequency",  bin_list$f, "logical")
  m_l <- check_include_range(df, "monetary", bin_list$m, "logical")

  if (r_l$logical) {
    msg <- ifelse(r_l$error == "min-max",
                  paste("Recency bins does not include the minimum value",
                        r_l[["value"]][1], "and the maximum value", r_l[["value"]][2]),
                  paste("Recency bins does not include the",
                        agg_label[[r_l$error]], "value", r_l$value))
    list(logical = TRUE, name = "Recency bins", text = msg)

  } else if (f_l$logical) {
    msg <- ifelse(f_l$error == "min-max",
                  paste("Frequency bins does not include the minimum value",
                        f_l[["value"]][1], "and the maximum value", f_l[["value"]][2]),
                  paste("Frequency bins does not include the",
                        agg_label[[f_l$error]], "value", f_l$value))
    list(logical = TRUE, name = "Frequency bins", text = msg)

  } else if (m_l$logical) {
    msg <- ifelse(m_l$error == "min-max",
                  paste("Monetary bins does not include the minimum value",
                        m_l[["value"]][1], "and the maximum value", m_l[["value"]][2]),
                  paste("Monetary bins does not include the",
                        agg_label[[m_l$error]], "value", m_l$value))
    list(logical = TRUE, name = "Monetary bins", text = msg)

  } else {
    list(logical = FALSE)
  }
}



#' Bin RFM metric into RFM scores
#'
#' @param dt data.table
#' @param var either 'recency', 'frequency' or 'monetary' variable
#' @param n_bins bins to group by
#' @param bin_labels the score label
#' @param rank logical, whether to rank the variable first before cutting them
#' into bins.
#' @param fct logical, whether to return the output as a factor data type.
#'
#' @return a vector with the same length as the variable
#' @export
#'
#' @examples bin_variable(dt, 'recency' 5, c(5, 4, 3, 2, 1))
bin_variable <- function(dt, var, n_bins, bin_labels, rank = FALSE, fct = FALSE) {
  if (length(n_bins) > 1) {
    if (length(n_bins) != length(bin_labels)+1) {
      stop("Number of bins supplied must be greater than bin label by 1")
    }
    check_within_range(dt = dt, var = var, bins = n_bins, type = "message")

    check_include_range(dt = dt, var = var, bins = n_bins)
  }
  if (length(n_bins) == 1 && n_bins < 2) {
    stop("`n_bins` must be greater than or equal to 2")
  }

  if (rank) {
    vect <- data.table::frank(dt[[var]], ties.method = "first")
  } else {
    vect <- dt[[var]]
  }
  if (length(n_bins) == 1) {
    n_bins <- n_bins + 1

    bins <- quantile(vect,
                     probs = seq(0, 1, length.out = n_bins),
                     names = FALSE)
  } else {
    bins <- sort(n_bins, decreasing = FALSE)
  }
  cut_out <- cut(x = vect, breaks = bins, labels = bin_labels, include.lowest = TRUE)

  if (fct) cut_out else as.numeric(as.character(cut_out))
}




check_bin_length <- function(bin_list) {
  val <- lapply(bin_list, \(.x) length(.x)) |>
    unlist() |>
    unique() |>
    length()

  if (val == 1) FALSE else TRUE
}



#' Check if bin value is below the threshold.
#'
#' @param bin_list a list
#'
#' @return TRUE if the check is true else FALSE
#' @export
#'
#' @examples check_bin_value(list(r = 5, f = 5, m = 5))
check_bin_value <- function(bin_list) {
  suit <- lapply(bin_list, \(.x) length(.x)) |> unlist()

  if (all(suit == 1)) {
    any(bin_list <= 2)
  } else {
    FALSE
  }
}



#' Check is bins are unique.
#'
#' @param bin_list a list
#'
#' @return a list containing a Boolean value and the name of the value when the
#' check is true.
#' @export
#'
#' @examples check_unique_bins(list(r = c(2, 10, 35), f = c(1, 23, 45),
#' m = c(0.14, 78, 189)))
check_unique_bins <- function(bin_list) {
  unique_len <- lapply(bin_list, \(.x) unique(.x) |> length())
  len <- lapply(bin_list, \(.x) length(.x))

  if (unique_len[[1]] != len[[1]]) {
    list(logical = TRUE, name = names(bin_list)[1])
  } else if (unique_len[[2]] != len[[2]]) {
    list(logical = TRUE, name = names(bin_list)[2])
  } else if (unique_len[[3]] != len[[3]]) {
    list(logical = TRUE, name = names(bin_list)[3])
  } else {
    list(logical = FALSE, name = "no name")
  }
}

alert_metric <- list(
  r = "Recency bins",
  f = "Freqeuncy bins",
  m = "Monetary bins"
)




#' Create an RFM table with RFM scores
#'
#' @param dt data.frame
#' @param n_transactions Frequency variable
#' @param recency_days Recency variable
#' @param total_revenue Monetary variable
#' @param recency_bins Recency bin
#' @param frequency_bins Frequency bin
#' @param monetary_bins Monetary bin
#'
#' @return a data.table containing RFM & RFM scores
#' @export
#'
#' @examples rfm_table(dt, 'frequency', 'recency', 'monetary' 5, 5, 5)
rfm_table <- function(dt,
                      n_transactions, recency_days, total_revenue,
                      recency_bins = 5, frequency_bins = 5, monetary_bins = 5) {

  if (length(recency_bins) != length(frequency_bins) ||
      length(recency_bins) != length(monetary_bins)) {
    stop("`recency_bins, frequency_bins, monetary_bins` must have the same length")
  }

  f_dt <- data.table::copy(dt)
  f_dt <- data.table::setnames(
    f_dt,
    old = c(recency_days, n_transactions, total_revenue),
    new = c("recency_days", "transaction_count", "amount"))
  f_dt[, `:=`(recency_score   = NA,
              frequency_score = NA,
              monetary_score  = NA)]

  # Recency ------------------------------------------------------------------|
  if (length(recency_bins) == 1) {
    r_score <- rev(seq_len(recency_bins))
  } else {
    r_score <- rev(seq_len((length(recency_bins) - 1)))
  }
  f_dt$recency_score <- bin_variable(dt = f_dt,
                                     var = "recency_days",
                                     n_bins = recency_bins,
                                     bin_labels = r_score)

  # Frequency ----------------------------------------------------------------|
  if (length(frequency_bins) == 1) {
    f_score <- seq_len(frequency_bins)
  } else {
    f_score <- seq_len((length(frequency_bins) - 1))
  }
  rank_d <- ifelse(length(frequency_bins) == 1, TRUE, FALSE)
  f_dt$frequency_score <- bin_variable(dt = f_dt,
                                       var = "transaction_count",
                                       n_bins = frequency_bins,
                                       bin_labels = f_score,
                                       rank = rank_d)

  # Monetary -----------------------------------------------------------------|
  if (length(monetary_bins) == 1) {
    m_score <- seq_len(monetary_bins)
  } else {
    m_score <- seq_len((length(monetary_bins) - 1))
  }
  f_dt$monetary_score <- bin_variable(dt = f_dt,
                                      var = "amount",
                                      n_bins = monetary_bins,
                                      bin_labels = m_score)

  # RFM_Score ----------------------------------------------------------------|
  f_dt[, `:=`(rfm = recency_score * 100 + frequency_score * 10 + monetary_score,
              transaction_count = as.numeric(transaction_count))]
  f_dt[, `:=`(rfm_score = recency_score+frequency_score+monetary_score)]

  # output -------------------------------------------------------------------|
  f_dt <- f_dt[, .(customer_id, recency_days, transaction_count, amount,
                   recency_score, frequency_score, monetary_score, rfm,
                   rfm_score)]

  return(f_dt)
}



#' Cluster RFM metric
#'
#' @param dt data.table
#' @param variable either 'recency', 'frequency' or 'monetary' variable
#' @param n_centers number of clusters
#' @param rev logical, whether to reveres the scores
#' @param seed numeric value for analysis reproduction.
#'
#' @return a data.table
#' @export
#'
#' @examples single_kmeans_bins(dt, 'frequency' 5)
single_kmeans_bins <- function(dt, variable, n_centers = 5, rev = FALSE, seed = 11) {
  # fit model ----------------------------------------------------|
  set.seed(seed)
  km_mdl <- kmeans(dt[[variable]], centers = n_centers)

  dt$unsorted_metric <- km_mdl$cluster

  # Group and Sort scores ----------------------------------------|
  dt_sort <- data.table::copy(dt)[,
                                  .(var_name = round(mean(.SD[[1]]), 2)),
                                  keyby = .(unsorted_metric),
                                  .SDcols = variable]
  dt_sort <- dt_sort[order(-var_name)]

  if (rev) {
    dt_sort$mt_score <- data.table::frank(dt_sort$var_name,
                                          ties.method = "max") |> rev()
  } else {
    dt_sort$mt_score <- data.table::frank(dt_sort$var_name,
                                          ties.method = "max")
  }

  # Merge data and drop redundant columns -------------------------|
  dt <- data.table::merge.data.table(dt, dt_sort, by = "unsorted_metric")
  dt[, c("unsorted_metric", "var_name") := NULL]
  data.table::setnames(dt,
                       old = c("mt_score"),
                       new = c(paste0(variable, "_score")))
  return(dt[])
}



#' Create RFM score using kmeans algorithm
#'
#' @param dt data.table
#' @param n_centers number of centers.
#' @param seed numeric value for analysis reproduction.
#'
#' @return a data.table with RFM & RFM scores of all RFM metric.
#' @export
#'
#' @examples rfm_kmean_score(dt, 5, 121)
rfm_kmean_score <- function(dt, n_centers = 5, seed = 11) {
  f_dt <- data.table::copy(dt)[, .(customer_id, recency, frequency, monetary)]

  # RFM Scores -----------------------------------------------------|
  f_dt <- single_kmeans_bins(dt = f_dt,
                             variable = "recency",
                             n_centers = n_centers,
                             rev = TRUE,
                             seed = seed)
  f_dt <- single_kmeans_bins(dt = f_dt,
                             variable = "frequency",
                             n_centers = n_centers,
                             rev = FALSE,
                             seed = seed)
  f_dt <- single_kmeans_bins(dt = f_dt,
                             variable = "monetary",
                             n_centers = n_centers,
                             rev = FALSE,
                             seed = seed)

  # clean names -----------------------------------------------------|
  data.table::setnames(f_dt,
                       old = c("recency", "frequency", "monetary"),
                       new = c("recency_days", "transaction_count", "amount"))

  f_dt[, `:=`(rfm = recency_score * 100 + frequency_score * 10 + monetary_score)]
  f_dt[, `:=`(rfm_score = recency_score+frequency_score+monetary_score)]

  return(f_dt[])
}



#' Clean rfm reactable variable names
#'
#' @param dt data.table
#'
#' @return a data.table with cleaned names
#' @export
#'
#' @examples
clean_rfm_names <- function(dt) {
  names(dt) <- clean_label(names(dt))

  data.table::setnames(dt,
                       old = c("Amount", "Rfm Score", "Rfm"),
                       new = c("Total Amount", "RFM Score", "RFM"))
  return(dt)
}



#' RFM Score Reactable
#'
#' @param dt a data.table with cleaned names.
#'
#' @return an html table widget
#' @export
#'
#' @examples
rfm_reactable_output <- function(dt) {
  reactable::reactable(
    data = dt,
    highlight = TRUE,
    outlined = TRUE,
    compact = TRUE,
    filterable = TRUE,
    showSortable = TRUE,
    defaultColDef = reactable::colDef(format = reactable::colFormat(separators = TRUE),
                                      na = "–"),
    columns = list(
      `Customer Id` = reactable::colDef(sortable = FALSE),
      `Total Amount` = reactable::colDef(format = reactable::colFormat(digits = 1)),
      `Customer Id` = reactable::colDef(align = "center",
                                        filterable = FALSE,
                                        format = reactable::colFormat(),
                                        style = list(background = tbl_header_bg_color,
                                                     color = tbl_ki_text_color,
                                                     textSize = tbl_ki_text_size,
                                                     borderRight = "1px solid #555")),

      RFM = reactable::colDef(align = "center",
                              headerStyle = list(background = tbl_second_ki_bg_color,
                                                 borderBottomColor = "#707070"),
                              style = list(background = tbl_second_ki_bg_color,
                                           color = tbl_ki_text_color,
                                           textSize = tbl_ki_text_size)),

      `RFM Score` = reactable::colDef(align = "center",
                                      headerStyle = list(background = tbl_ki_bg_color,
                                                         borderBottomColor = "#707070"),
                                      style = list(background = tbl_ki_bg_color,
                                                   color = tbl_ki_text_color,
                                                   textSize = tbl_ki_text_size,
                                                   fontWeight = "bold",
                                                   borderBottom = paste("1px solid", tbl_ki_bg_border_color) ))
    ),
    fullWidth = TRUE,
    resizable = TRUE,
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



#' Manual Discretization
#'
#' @param min_val a metric minimum value
#' @param max_val a metric maximum value
#'
#' @return a shiny markdown
#' @export
#'
#' @examples
md_info <- function(min_val, max_val) {
  min_val <- round(min_val, 2)
  max_val <- round(max_val, 2)
  shiny::tagList(
    shiny::markdown(
      paste(
        "Bins supplied must be **between & include**<br>the minimum value of",
        paste0("**", min_val, "**"), "and maximum<br>value of",
        paste0("**", max_val, "**."), "And values must be seperated<br>with comma."
      )
    )
  )
}

