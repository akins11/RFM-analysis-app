top_bottom_sidebar <- function(sidebar_id,
                               numeric_id,
                               numeric_label,
                               select_id) {
  bs4Dash::boxSidebar(
    id = sidebar_id,
    startOpen = FALSE,
    width = 30,
    background = box_sidebar_bg,
    icon = fontawesome::fa_i("fas fa-cogs", verify_fa = FALSE),

    shinyWidgets::numericInputIcon(
      inputId = numeric_id,
      label = numeric_label,
      min = 5, max = 20, value = 10, step = 1),

    shiny::tags$br(),

    shinyWidgets::pickerInput(
      inputId = select_id,
      label = "Aggregate Function",
      choices = c("Minimum" = "min", "Average" = "mean",
                  "Median" = "median", "Maximum" = "max",
                  "Total" = "sum"),
      selected = "sum",
      options = shinyWidgets::pickerOptions(style = "btn-default")
      )
  )
}


#' product_level UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_product_level_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shinyWidgets::panel(
      shiny::fluidRow(
        shiny::column(
          width = 6,
          align = "center",

          shinyWidgets::pickerInput(
            inputId = ns("prod_level_select_segment"),
            label = "Select a segment",
            choices = NULL,
            options = shinyWidgets::pickerOptions(style = "btn-default")
            )
        ),

        shiny::column(
          width = 6,
          align = "center",

          shinyWidgets::prettyRadioButtons(
            inputId = ns("summary_by"),
            label = "Summary based on",
            choices = c("Revenue" = "revenue",
                        "Quantity" = "quantity"),
            selected = "revenue",
            status = "info",
            shape = "curve",
            thick = TRUE,
            bigger = TRUE,
            inline = TRUE,
            animation = "pulse"
            )
        )
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        shiny::plotOutput(outputId = ns("top_product_plot")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        sidebar = top_bottom_sidebar(ns("t_product_sidebar"), ns("top_numeric"),
                                     "Top", ns("top_agg_fun")),

        width = 6
      ),


      bs4Dash::box(
        shiny::plotOutput(outputId = ns("bottom_product_plot")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        sidebar = top_bottom_sidebar(ns("b_product_sidebar"), ns("bottom_numeric"),
                                     "Bottom", ns("bottom_agg_fun")),

        width = 6
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      bs4Dash::box(
        plotly::plotlyOutput(outputId = ns("min_max_plot")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        sidebar = bs4Dash::boxSidebar(
          id = "min_max_sidebar",
          startOpen = FALSE,
          width = 25,
          background = box_sidebar_bg,
          icon = fontawesome::fa_i("fas fa-cogs", verify_fa = FALSE),

          shiny::numericInput(
            inputId = ns("min_max_numeric"),
            label = "Number of products",
            min = 5, max = 20, value = 10, step = 1
            ),

          shiny::tags$br(),

          shinyWidgets::actionBttn(
            inputId = ns("min_max_default"),
            label = "default",
            style = "bordered",
            color = "default"

          )
        ),
        width = 12
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      bs4Dash::box(
        reactable::reactableOutput(outputId = ns("within_seg_product_table")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        width = 12,
        title = shiny::textOutput(outputId = ns("seg_product_table_title"))
      )
    )
  )
}







#' product_level Server Functions
#'
#' @noRd
mod_product_level_server <- function(id, dash_lst, seg_data, parent_session) {
  stopifnot(shiny::is.reactive(seg_data))
  stopifnot(shiny::is.reactive(dash_lst))

  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      segments <- shiny::reactive({
        shiny::req(seg_data())

        get_segments(seg_data())
      })

      # Update Segment choice --------------------------------------------|
      shiny::observe({
        shiny::req(segments())

        shinyWidgets::updatePickerInput(session = session,
                                        inputId = "prod_level_select_segment",
                                        choices = segments())
      })

      # Summary Data -----------------------------------------------------|
      prod_one_seg_data <- shiny::reactive({
        shiny::req(seg_data(), input$prod_level_select_segment, dash_lst())

        get_customer_id(seg_data(), input$prod_level_select_segment) |>
          extract_customers(dt = dash_lst()$main_data, selected_customer_id = _)
      })

      prod_data <- shiny::reactive({
        shiny::req(prod_one_seg_data(), input$summary_by)

        within_segment_summary(dt = prod_one_seg_data(),
                               variable = input$summary_by,
                               gp_var = "product",
                               order_var = "sum")
      })

      # Top Products ------------------------------------------------------|
      output$top_product_plot <- shiny::renderPlot({
        shiny::req(prod_data(),
                   input$prod_level_select_segment,
                   input$top_agg_fun,
                   input$summary_by,
                   input$top_numeric)

        product_by_segment(dt = prod_data(),
                           agg_fun = input$top_agg_fun,
                           direction = "top",
                           n_categories = input$top_numeric,
                           by = input$summary_by,
                           segment = input$prod_level_select_segment)
      })

      # Bottom Products ---------------------------------------------------|
      output$bottom_product_plot <- shiny::renderPlot({
        shiny::req(prod_data(),
                   input$prod_level_select_segment,
                   input$bottom_agg_fun,
                   input$summary_by,
                   input$bottom_numeric)

        product_by_segment(dt = prod_data(),
                           agg_fun = input$bottom_agg_fun,
                           direction = "bottom",
                           n_categories = input$bottom_numeric,
                           by = input$summary_by,
                           segment = input$prod_level_select_segment)

      })

      # Min-Max plot ------------------------------------------------------|
      shiny::observe({
        shiny::req(input$min_max_numeric)

        shiny::updateNumericInput(
          session = session,
          inputId = "min_max_numeric",
          min = 5, max = 20, value = 10, step = 1
        )
      }) |>
        shiny::bindEvent(input$min_max_default)

      output$min_max_plot <- plotly::renderPlotly({
        shiny::req(prod_data(),
                   input$prod_level_select_segment,
                   input$summary_by,
                   input$min_max_numeric)

        min_max_val <- ifelse(input$min_max_numeric == 0, 1, input$min_max_numeric)

        min_max_summary(dt = prod_data(),
                        by = input$summary_by,
                        segment = input$prod_level_select_segment,
                        n = min_max_val,
                        interactive = TRUE)
      })

      # Number of Purchase ------------------------------------------------|
      output$within_seg_product_table <- reactable::renderReactable({
        shiny::req(prod_one_seg_data())

        n_pur <- within_segment_count(dt = prod_one_seg_data(), by = "product") |>
          clean_within_segment_names(by = "product")

        within_seg_count_reactable(n_pur)
      })
      output$seg_product_table_title <- renderText({
        shiny::req(input$prod_level_select_segment)
        paste("Products Purchased In", clean_label(input$prod_level_select_segment),
              "Segment")
      })
    }
  )
}
