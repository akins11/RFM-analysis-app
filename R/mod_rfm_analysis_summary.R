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



#' rfm_analysis_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rfm_analysis_summary_ui <- function(id){
  ns <- NS(id)

  shiny::tagList(
    shiny::fluidRow(
      bs4Dash::box(
        plotly::plotlyOutput(outputId = ns("rfm_score_hm")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        sidebar = bs4Dash::boxSidebar(
          id = ns("rfm_score_count_sidebar"),
          startOpen = FALSE,
          width = 25,
          background = box_sidebar_bg,
          icon = fontawesome::fa_i("fas fa-cogs"),

          rfm_score_picker_input(ns("x_variable"), "X variable", "recency_score"),

          shiny::tags$br(),

          rfm_score_picker_input(ns("y_variable"), "Y variable", "frequency_score")
        ),

        width = 12
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      bs4Dash::box(
        plotly::plotlyOutput(outputId = ns("rfm_amount_hm")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        sidebar = bs4Dash::boxSidebar(
          id = ns("rfm_score_amount_sidebar"),
          startOpen = FALSE,
          width = 25,
          background = box_sidebar_bg,
          icon = fontawesome::fa_i("fas fa-cogs"),

          shinyWidgets::prettyRadioButtons(
            inputId = ns("rfm_amount_agg_fun"),
            label = "",
            choices = c("Minimum" = "min", "Average" = "mean",
                        "Median" = "median", "Maximum" = "max",
                        "Total" = "sum"),
            selected = "mean",
            status = "info",
            shape = "curve",
            thick = TRUE,
            bigger = TRUE,
            animation = "pulse"
            )
        ),
        width = 12
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      bs4Dash::box(
        reactable::reactableOutput(outputId = ns("score_count_table")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        width = 12
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        plotly::plotlyOutput(outputId = ns("score_count_plot")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        sidebar = bs4Dash::boxSidebar(
          id = ns("score_count_plot_sidebar"),
          startOpen = FALSE,
          width = 25,
          background = box_sidebar_bg,
          icon = fontawesome::fa_i("fas fa-cogs"),

          shiny::numericInput(inputId = ns("n_rfm_scores"), label = "Top",
                              min = 5, max = 20, step = 1, value = 15),

          shiny::tags$br(),
          shinyWidgets::prettyRadioButtons(
            inputId = ns("score_count_plot_sort"),
            label = "Sort By RFM",
            choices = c("Score Count" = TRUE, "Score" = FALSE),
            selected = TRUE,
            status = "info",
            shape = "curve",
            thick = TRUE,
            bigger = TRUE,
            animation = "pulse"
            ),

          shiny::tags$br(),

          shinyWidgets::actionBttn(
            inputId = ns("restore"),
            label = "Restore Default value",
            style = "bordered",
            color = "default",
            size = "sm"
            )
        ),

        title = "RFM Score Count",
        width = 12
      )
    )
  )
}





#' rfm_analysis_summary Server Functions
#'
#' @noRd
mod_rfm_analysis_summary_server <- function(id, rfm_data, parent_session) {
  stopifnot(shiny::is.reactive(rfm_data))

  shiny::moduleServer(
    id = id,

    module = function(input, output, session) {
    ns <- session$ns

    # Score Heat map ----------------------------------------------|
    output$rfm_score_hm <- plotly::renderPlotly({
      shiny::req(rfm_data(), input$x_variable, input$y_variable)

      if (input$x_variable != input$y_variable) {
        score_count(dt = rfm_data(),
                    x_var = input$x_variable,
                    y_var = input$y_variable,
                    interactive = TRUE)
      } else {
        duplicate_val <- clean_label(input$x_variable)

        shinyWidgets::show_alert(
          title = "Duplicate Inputs",
          text = paste("You have Selected", duplicate_val, "Twice"),
          type = "error"
        )
      }
    })

    # Sales amount and rfm score ----------------------------------|
    output$rfm_amount_hm <- plotly::renderPlotly({
      shiny::req(rfm_data(),  input$rfm_amount_agg_fun)

      scores_amount_heatmap(dt = rfm_data(),
                            agg_fun = input$rfm_amount_agg_fun,
                            interactive = TRUE)
    })


    # Score count table -------------------------------------------|
    output$score_count_table <- reactable::renderReactable({
      shiny::req(rfm_data())

      a_dt <- rfm_score_table_count(dt = rfm_data()) |>
        clean_rfm_score_name()

      rfm_score_reactable_count(dt = a_dt)
    })

    # Score count plot ---------------------------------------------|
    shiny::observe({
      shiny::updateNumericInput(session = session,
                                inputId = "n_rfm_scores",
                                value = 15, min = 5, max = 20, step = 1)
    }) |>
      shiny::bindEvent(input$restore)

    output$score_count_plot <- plotly::renderPlotly({
      shiny::req(rfm_data(), input$n_rfm_scores, input$score_count_plot_sort)

      shinyWidgets::execute_safely(
        rfm_score_plot_count(dt = rfm_data(),
                             n = input$n_rfm_scores,
                             sort = input$score_count_plot_sort),
        title = "No Value",
        message = "Number of top score count cannot be empty",
        include_error = FALSE
      )
    })
   }
  )
}

## To be copied in the UI
# mod_rfm_analysis_summary_ui("rfm_analysis_summary_1")

## To be copied in the server
# mod_rfm_analysis_summary_server("rfm_analysis_summary_1")
