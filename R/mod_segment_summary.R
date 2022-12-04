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
        echarts4r::echarts4rOutput(outputId = ns("segment_count_plot")) |>
          ui_spinner(),

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
          ui_spinner(),

        width = 10,
        title = "Segment & RFM Score Count"
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        echarts4r::echarts4rOutput(outputId = ns("segment_agg_rfm_plot")) |>
          ui_spinner(),

        width = 10,
        title = shiny::textOutput(outputId = ns("segment_agg_summary_title"))
      ),

      shiny::column(
        width = 2,

        shinyWidgets::panel(
          shinyWidgets::pickerInput(inputId = ns("seg_tm_by"),
                                    choices = c("Count" = "count",
                                                "Recency Days" = "recency",
                                                "Number of Transaction" = "frequency",
                                                "Amount" = "monetary"),
                                    selected = "monetary",
                                    options = shinyWidgets::pickerOptions(style = "btn-default")),

          shiny::div(
            id = ns("segment_agg_rfm_rb_sh"),

            shiny::br(),

            shinyWidgets::prettyRadioButtons(inputId = ns("segment_agg_rfm_rb"),
                                             label = "Aggregate Function",
                                             choices = c("Minimum" = "min",
                                                         "Average" = "mean",
                                                         "Median"  = "median",
                                                         "Maximum" = "max",
                                                         "Total" = "sum"),
                                             selected = "mean",
                                             status = "info",
                                             shape = "curve",
                                             thick = TRUE,
                                             bigger = TRUE,
                                             animation = "pulse")
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
#' @param id
#' @param seg_data
#' @param parent_session
#'
#' @noRd
mod_segment_summary_server <- function(id, seg_data, parent_session) {
  stopifnot(shiny::is.reactive(seg_data))

  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      # Bar Count plot -------------------------------------------------|
      output$segment_count_plot <- echarts4r::renderEcharts4r({
        shiny::req(seg_data(), input$sort_segment_rb)

        segment_count(dt = seg_data(),
                      sort = input$sort_segment_rb)
      })

      # Count Table ----------------------------------------------------|
      output$segment_count_table <- reactable::renderReactable({
        shiny::req(seg_data())

        segment_count_table(dt = seg_data()) |>
          clean_seg_count_name() |>
          seg_count_reactable()
      })

      # Aggregate Segment Summary --------------------------------------|
      shiny::observe({
        if (input$seg_tm_by == "count") {
          shinyjs::hide(id = "segment_agg_rfm_rb_sh",
                        anim = TRUE)

        } else {
          shinyjs::show(id = "segment_agg_rfm_rb_sh",
                        anim = TRUE)
        }
      })

      shiny::observe({
        if (input$seg_tm_by %in% c("recency", "frequency")) {
          CN <- c("Minimum" = "min",
                  "Average" = "mean",
                  "Median"  = "median",
                  "Maximum" = "max")

        } else { #if (input$seg_tm_by == "monetary")
          CN <- c("Minimum" = "min",
                  "Average" = "mean",
                  "Median"  = "median",
                  "Maximum" = "max",
                  "Total" = "sum")
        }

        shinyWidgets::updatePrettyRadioButtons(session = session,
                                               inputId = "segment_agg_rfm_rb",
                                               choices = CN,
                                               prettyOptions = list(selected = "mean",
                                                                    status = "info",
                                                                    shape = "curve",
                                                                    thick = TRUE,
                                                                    bigger = TRUE,
                                                                    animation = "pulse"))
      })


      output$segment_agg_summary_title <-  shiny::renderText({
        if (input$seg_tm_by != "count") {
          agg_lab <- agg_label[[input$segment_agg_rfm_rb]]
          rfm_lab <- list(recency = "Recent Days",
                          frequency = "Number of Transactions",
                          monetary = "Amount")

          glue::glue("{agg_lab} {rfm_lab[[input$seg_tm_by]]} By Customer Segment")

        } else {
          "Number Of Customers In Each Segment"
        }

      })

      output$segment_agg_rfm_plot <- echarts4r::renderEcharts4r({
        shiny::req(seg_data(), input$segment_agg_rfm_rb)

        segment_agg_treemap(dt = seg_data(),
                            by = input$seg_tm_by,
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
