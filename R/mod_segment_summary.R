#' segment_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_segment_summary_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::fluidRow(
      bs4Dash::box(
        plotly::plotlyOutput(outputId = ns("segment_count_plot")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        width = 10,
        title = "Number Of Customers In Each Segment"
      ),

      shiny::column(
        width = 2,

        shinyWidgets::panel(
          shinyWidgets::prettyRadioButtons(
            inputId = ns("sort_segment_rb"),
            label = "Sort Segment",
            choices = c("Yes" = TRUE, "NO" = FALSE),
            selected = FALSE,
            status = "info",
            shape = "curve",
            thick = TRUE,
            bigger = TRUE,
            animation = "pulse"
          )
        )
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        reactable::reactableOutput(outputId = ns("segment_count_table")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        width = 10,
        title = "Segment & RFM Score Count"
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        plotly::plotlyOutput(outputId = ns("segment_agg_rfm_plot")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        width = 10,
        title = "Aggregate Summary Of Each Segments By RFM"
      ),

      shiny::column(
        width = 2,

        shinyWidgets::panel(
          shinyWidgets::prettyRadioButtons(
            inputId = ns("segment_agg_rfm_rb"),
            label = "Aggregate Function",
            choices = c("Minimum" = "min", "Average" = "mean",
                        "Median" = "median", "Maximum" = "max",
                        "Total" = "sum"),
            selected = "sum",
            status = "info",
            shape = "curve",
            thick = TRUE,
            bigger = TRUE,
            animation = "pulse"
            )
        )
      )

      # box(
      #   plotly::plotlyOutput(outputId = ns("segment_tm_count_plot")) |>
      #     shinycssloaders::withSpinner(type = 4, color = spinner_color),
      #
      #   width = 5,
      #   height = 420,
      #   title = "Segment Count"
      # )
    )
  )
}

#' segment_summary Server Functions
#'
#' @noRd
mod_segment_summary_server <- function(id, seg_data, parent_session) {
  stopifnot(shiny::is.reactive(seg_data))

  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      # Bar Count plot -------------------------------------------------|
      output$segment_count_plot <- plotly::renderPlotly({
        shiny::req(seg_data(), input$sort_segment_rb)

        segment_count(dt = seg_data(),
                      sort = input$sort_segment_rb,
                      output_type = "plot",
                      interactive = TRUE)
      })

      # Count Table ----------------------------------------------------|
      output$segment_count_table <- reactable::renderReactable({
        shiny::req(seg_data())

        segment_count_table(dt = seg_data()) |>
          clean_seg_count_name() |>
          seg_count_reactable()
      })

      # Aggregate Segment Summary --------------------------------------|
      output$segment_agg_rfm_plot <- plotly::renderPlotly({
        shiny::req(seg_data(), input$segment_agg_rfm_rb)

        segment_agg_treemap(dt = seg_data(),
                            agg_fun = input$segment_agg_rfm_rb,
                            round = TRUE)
      })

      # Tree Map Count -------------------------------------------------|
      # output$segment_tm_count_plot <- plotly::renderPlotly({
      #   req(seg_data())
      #
      #   segment_count_treemap(dt = seg_data())
      # })
    }
  )
}

## To be copied in the UI
# mod_segment_summary_ui("segment_summary_1")

## To be copied in the server
# mod_segment_summary_server("segment_summary_1")
