info_button <- function(info_id) {
  shinyWidgets::actionBttn(
    inputId = info_id,
    label = "",
    icon = icon("info"),
    style = "material-circle"
  )
}




info_message <- list(
  MD = "This incorporate all the supplied set of recency, frequency and monetary bin,
        after that each set of bins is utilized to manually group each RFM metric,
        which create the respective metric score",

  QBD = "This involves obtaining the quantile values of each RFM metric based on the
         supplied bin value below. Then all metric values that fall within each
         quantile will be grouped together to create each RFM score.",

  kmeans = "Clusters persent in the recency, frequency and monetary metric will be
           extracted using the k-means clustering algorithm based on the number of
           centers supplied below."
)


alert_metric <- list(
  r = "Recency bins",
  f = "Freqeuncy bins",
  m = "Monetary bins"
)

qbd_value_info <- function() {
  shiny::markdown("A **numeric** value greater then 2")
}
