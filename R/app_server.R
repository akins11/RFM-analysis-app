#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  rs_output <- mod_rfm_description_server(id = "rfm_description_out")

  rfm_dt <- mod_rfm_analysis_server(id = "rfm_analysis_out", dash_lst = rs_output)

  mod_rfm_analysis_summary_server(id = "rfm_analysis_summary_out", rfm_data = rfm_dt)

  seg_data <- mod_segment_assignment_server(id = "segment_assignment_out", rfm_data = rfm_dt)

  mod_segment_summary_server(id = "segment_summary_out", seg_data = seg_data)

  mod_product_level_server(id = "product_level_summary_out",
                           dash_lst = rs_output, seg_data = seg_data)

  mod_customer_level_server(id = "customer_level_summary_out",
                            dash_lst = rs_output, seg_data = seg_data)
}
