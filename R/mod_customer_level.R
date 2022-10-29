#' customer_level UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_customer_level_ui <- function(id){
  ns <- NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 5,

        shinyWidgets::panel(
          shinyWidgets::pickerInput(
            inputId = ns("cus_level_select_segment"),
            label = "Select a segment",
            choices = NULL,
            options = shinyWidgets::pickerOptions(style = "btn-default")
            )
        )
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        reactable::reactableOutput(outputId = ns("cus_count")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        width = 10,
        title = shiny::textOutput(ns("n_top_customer_title"))
      ),

      shiny::column(
        width = 2,

        shinyWidgets::panel(
          shiny::numericInput(
            inputId = ns("n_top_customer"),
            label = "Number of customers",
            min = 100, max = 1000, step = 50,
            value = 100
            )
        )
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      bs4Dash::box(
        reactable::reactableOutput(outputId = ns("cus_top_products")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        width = 10,
        # title = textOutput(ns("top_product_title"))
        title = "Top Products Purchased By Customers Based On Sales"
      ),

      shiny::column(
        width = 2,

        shinyWidgets::panel(
          shiny::numericInput(
            inputId = ns("n_top_products"),
            label = "Number of products",
            min = 100, max = 1000, step = 50,
            value = 100
            )
        )
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      bs4Dash::box(
        reactable::reactableOutput(outputId = ns("cus_product_agg")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        width = 10,
        title = shiny::textOutput(ns("single_product_agg_title"))
      ),

      shiny::column(
        width = 2,

        shinyWidgets::panel(
          shinyWidgets::pickerInput(
            inputId = ns("product_type"),
            label = "Product",
            choices = NULL,
            options = shinyWidgets::pickerOptions(style = "btn-default",
                                                  liveSearch = TRUE) ),

          shinyWidgets::prettyRadioButtons(
            inputId = ns("cus_product_agg_fun"),
            label = "Plot By",
            choices = c("Minimum" = "min", "Average" = "mean",
                        "Median" = "median", "Maximum" = "max",
                        "Total" = "sum"),
            selected = "median",
            status = "info",
            shape = "curve",
            thick = TRUE,
            bigger = TRUE,
            animation = "pulse"
            )
        )
      )
    )
  )
}






#' customer_level Server Functions
#'
#' @noRd
mod_customer_level_server <- function(id, dash_lst, seg_data, parent_session) {
  stopifnot(shiny::is.reactive(seg_data))
  stopifnot(shiny::is.reactive(dash_lst))

  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      cus_segments <- shiny::reactive({
        shiny::req(seg_data())

        get_segments(seg_data())
      })

      # Update Segment choice --------------------------------------------|
      shiny::observe({
        shiny::req(cus_segments())

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "cus_level_select_segment",
          choices = cus_segments()
          )
      })

      # Summary Data -----------------------------------------------------|
      cus_one_seg_data <- shiny::reactive({
        shiny::req(seg_data(), input$cus_level_select_segment, dash_lst())

        get_customer_id(seg_data(), input$cus_level_select_segment) |>
          extract_customers(dt = dash_lst()$main_data, selected_customer_id = _)
      })

      # wait toast --------------------------------------------------------|
      # observe({
      #   show_toast(
      #     title = "",
      #     text = "Please wait...",
      #     type = "info",
      #     position = "bottom-end"
      #   )
      # }) |>
      #   bindEvent(cus_one_seg_data())

      # customer count ----------------------------------------------------|
      output$cus_count <- reactable::renderReactable({
        shiny::req(cus_one_seg_data(), input$n_top_customer)

        within_segment_count(dt = cus_one_seg_data(),
                             by = "customer_id",
                             n = input$n_top_customer) |>
          clean_within_segment_names("customer_id") |>
          within_seg_count_reactable()
      })
      output$n_top_customer_title <- shiny::renderText({
        shiny::req(input$n_top_customer, input$cus_level_select_segment)
        paste("Top", input$n_top_customer,
              "Customers By Number Of Purchases In",
              clean_label(input$cus_level_select_segment), "Segment")
      })


      # Customer top sales ----------------------------------------------|
      output$cus_top_products <- reactable::renderReactable({
        shiny::req(cus_one_seg_data(), input$n_top_products)

        dp_customer_top_purchase(cus_one_seg_data(), input$n_top_products) |>
          clean_top_prod_names() |>
          cus_top_purchase_reactable()
      })
      # output$top_product_title <- renderText({
      #   req(cus_segments())
      #   paste("Top Products Purchased By Customers Based On Sales")
      # })

      # Single Product --------------------------------------------------|
      shiny::observe({
        shiny::req(cus_one_seg_data())

        products <- get_unique_top_revenue_product(dt = cus_one_seg_data())

        shinyWidgets::updatePickerInput(session = session,
                                        inputId = "product_type",
                                        choices = products)
      })

      output$cus_product_agg <- reactable::renderReactable({
        shiny::req(cus_one_seg_data(),
                   input$product_type,
                   input$cus_product_agg_fun)

        one_product_summary(dt = cus_one_seg_data(),
                            s_product = input$product_type,
                            agg_fun = input$cus_product_agg_fun) |>
          clean_one_prod_name() |>
          one_product_reactable()
      })
      output$single_product_agg_title <- renderText({
        shiny::req(input$product_type,
                   input$cus_product_agg_fun,
                   input$cus_level_select_segment)

        agg_fun <- agg_label[[input$cus_product_agg_fun]]

        paste(agg_fun, "Revenue & Quantity Of", clean_label(input$product_type),
              "For Customers In", input$cus_level_select_segment, "Segment.")
      })
    }
  )
}


