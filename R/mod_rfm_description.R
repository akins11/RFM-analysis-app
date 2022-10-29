distibution_options <- function(sidebar_id, slider_id, radio_id, numeric_id, check_id) {
  bs4Dash::boxSidebar(
    startOpen = FALSE,
    id = sidebar_id,
    background = box_sidebar_bg,
    icon = fontawesome::fa_i("fas fa-cogs", verify_fa = FALSE),

    shiny::sliderInput(inputId = slider_id, label = "Zoom",
                       value = c(0, 1),
                       min = 0, max = 1, step = 0.5),

    shiny::tags$br(),

    shinyWidgets::prettyRadioButtons(
      inputId = radio_id,
      label = "Type of Plot",
      choices = c("Histogram" = "hist", "Boxplot" = "box"),
      selected = "hist",
      status = "info",
      shape = "curve",
      thick = TRUE,
      bigger = TRUE,
      animation = "pulse"
    ),

    tags$br(),

    shiny::numericInput(inputId = numeric_id, label = "Number of bins",
                        value = 30,
                        min = 20, max = 60, step = 5),

    tags$br(),

    shinyWidgets::prettyCheckbox(
      inputId = check_id,
      label = "Log",
      value = FALSE,
      status = "info",
      shape = "curve",
      thick = TRUE,
      bigger = TRUE,
      animation = "pulse"
      )
  )
}

date_plot_option <- function(sidebar_id, radio_id, wd = 50) {
  bs4Dash::boxSidebar(
    startOpen = FALSE,
    id = sidebar_id,
    width = wd,
    background = box_sidebar_bg,
    icon = fontawesome::fa_i("fas fa-cogs", verify_fa = FALSE),

    shinyWidgets::prettyRadioButtons(
      inputId = radio_id,
      label = "Type of Plot",
      choices = c("Bar" = "bar", "Line" = "line", "Area" = "area"),
      selected = "area",
      status = "info",
      shape = "curve",
      thick = TRUE,
      bigger = TRUE,
      animation = "pulse"
    )
  )
}



#' rfm_description UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rfm_description_ui <- function(id){
  ns <- NS(id)

  shiny::tagList(
    shiny::fluidRow(
      bs4Dash::valueBoxOutput(outputId = ns("n_customer_vbox"), width = 3),

      bs4Dash::valueBoxOutput(outputId = ns("n_product_vbox"), width = 3),

      bs4Dash::valueBoxOutput(outputId = ns("total_sales_vbox"), width = 3),

      bs4Dash::valueBoxOutput(outputId = ns("average_lt_vbox"), width = 3)
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      bs4Dash::box(
        plotly::plotlyOutput(outputId = ns("recency_dis_out")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        sidebar = distibution_options(ns("r_sidebar"),
                                      ns("r_slider"),
                                      ns("r_radio"),
                                      ns("r_bins"),
                                      ns("r_log")),

        title = shiny::textOutput(outputId = ns("r_dist_title"))
      ),

      bs4Dash::box(
        plotly::plotlyOutput(outputId = ns("frequency_dis_out")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        sidebar = distibution_options(ns("f_sidebar"),
                                      ns("f_slider"),
                                      ns("f_radio"),
                                      ns("f_bins"),
                                      ns("f_log")),

        title = shiny::textOutput(outputId = ns("f_dist_title"))
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        plotly::plotlyOutput(outputId = ns("monetary_dis_out")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        sidebar = distibution_options(ns("m_sidebar"),
                                      ns("m_slider"),
                                      ns("m_radio"),
                                      ns("m_bins"),
                                      ns("m_log")),

        title = shiny::textOutput(outputId = ns("m_dist_title"))
      ),

      bs4Dash::box(
        plotly::plotlyOutput(outputId = ns("quarterly_intake")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        sidebar = date_plot_option(ns("qtr_sidebar"), ns("qtr_radio")),

        title = "Quarterly Customer Intake"
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        plotly::plotlyOutput(outputId = ns("monthly_intake")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        sidebar = date_plot_option(ns("mty_sidebar"), ns("mty_radio"), 25),

        width = 12,
        title = "Monthly Customer Intake"
      )
    )
  )
}




#' rfm_description Server Functions
#'
#' @noRd
mod_rfm_description_server <- function(id){
  shiny::moduleServer(
    id = id,

    module = function(input, output, session) {
      # Load Data ---------------------------------------------------|
      dash_data <- shiny::reactive({
        data.table::fread(file = "data/a_retail.csv") |>
          clean_date(which = "date")
      })

      dash_rfm <- shiny::reactive({ #dash_us_rfm
        data.table::fread(file = "data/a_rfm_dt.csv") |>
          clean_date(which = "first_last")
      })

      analysis_date <- reactive({
        shiny::req(dash_data())
        lubridate::date(max(dash_data()$date)) + lubridate::days(2)
      })

      # Update value box -----------------------------------------------|
      output$n_customer_vbox <- bs4Dash::renderValueBox({
        get_value(dt = dash_data(), what = "n_customers") |>
          bs4Dash::valueBox(
            subtitle = "Unique Customers",
            color = "pink",
            # icon = shiny::icon("users")
            icon = fontawesome::fa_i("fas fa-users", verify_fa = FALSE)
          )

      })
      output$n_product_vbox <- bs4Dash::renderValueBox({
        get_value(dt = dash_data(), what = "n_product") |>
          bs4Dash::valueBox(
            subtitle = "Unique Products",
            color = "indigo",
            # icon = icon("shopping-cart")
            icon = fontawesome::fa_i("fas fa-shopping-cart", verify_fa = FALSE)
          )
      })
      output$total_sales_vbox <- bs4Dash::renderValueBox({
        get_value(dt = dash_data(), what = "total_revenue") |>
          bs4Dash::valueBox(
            subtitle = "Total Revenue",
            color = "primary",
            # icon = shiny::icon("coins")
            icon = fontawesome::fa_i("fas fa-coins", verify_fa = FALSE)
          )
      })
      output$average_lt_vbox <- bs4Dash::renderValueBox({
        get_value(rfm_dt = dash_rfm(), what = "average_lt") |>
          bs4Dash::valueBox(
            subtitle = "Average Customer Life Time",
            color = "lime",
            # icon = shiny::icon("history")
            icon = fontawesome::fa_i("fas fa-history", verify_fa = FALSE)
          )
      })



      # Distribution Plots --------------------------------------------|
      # Recency -----------------------------------------------------|
      shiny::observe({
        if (input$r_log) {
          update_dt <- log_transform(dash_rfm(), "recency")
        }  else {
          update_dt <- dash_rfm()
        }
        zoom <- distribution_zoom_value(update_dt, "recency")
        shiny::freezeReactiveValue(input, "r_slider")
        shiny::updateSliderInput(session = session,
                                 inputId = "r_slider",
                                 value = c(0, zoom$max),
                                 min = 0, max = zoom$max, step = zoom$stp)
      })

      output$recency_dis_out <- plotly::renderPlotly({
        shiny::req(dash_rfm(), input$r_radio)

        if (input$r_radio == "hist") {
          rfm_distribution_plot(dt = dash_rfm(),
                                var = "recency",
                                zoom = input$r_slider,
                                bins = input$r_bins,
                                log_var = input$r_log,
                                interactive = TRUE)
        } else if (input$r_radio == "box") {
          rfm_boxplot(dt = dash_rfm(),
                      var = "recency",
                      zoom = input$r_slider,
                      log_var = input$r_log,
                      interactive = TRUE)
        }
      })

      output$r_dist_title <- shiny::renderText({
        if (input$r_log) {
          "Recency Distribution (Log)"
        } else {
          "Recency Distribution"
        }
      })

      # Frequency ---------------------------------------------------|
      shiny::observe({
        if (input$f_log) {
          update_dt <- log_transform(dash_rfm(), "frequency")
        }  else {
          update_dt <- dash_rfm()
        }
        zoom <- distribution_zoom_value(update_dt, "frequency")
        shiny::freezeReactiveValue(input, "f_slider")
        shiny::updateSliderInput(session = session,
                                 inputId = "f_slider",
                                 value = c(0, zoom$max),
                                 min = 0, max = zoom$max, step = zoom$stp)
      })

      output$frequency_dis_out <- plotly::renderPlotly({
        shiny::req(dash_rfm(), input$f_radio)

        if (input$f_radio == "hist") {
          rfm_distribution_plot(dt = dash_rfm(),
                                var = "frequency",
                                zoom = input$f_slider,
                                bins = input$f_bins,
                                log_var = input$f_log,
                                interactive = TRUE)
        } else if (input$f_radio == "box") {
          rfm_boxplot(dt = dash_rfm(),
                      var = "frequency",
                      zoom = input$f_slider,
                      log_var = input$f_log,
                      interactive = TRUE)
        }
      })

      output$f_dist_title <- shiny::renderText({
        if (input$f_log) {
          "Frequency Distribution (Log)"
        } else {
          "Frequency Distribution"
        }
      })

      # Monetary -----------------------------------------------------|
      shiny::observe({

        if (input$m_log) {
          update_dt <- log_transform(dash_rfm(), "monetary")
        }  else {
          update_dt <- dash_rfm()
        }
        zoom <- distribution_zoom_value(update_dt, "monetary")
        shiny::freezeReactiveValue(input, "m_slider")
        shiny::updateSliderInput(session = session,
                                 inputId = "m_slider",
                                 value = c(0, zoom$max),
                                 min = 0, max = zoom$max, step = zoom$stp)
      })

      output$monetary_dis_out <- plotly::renderPlotly({
        shiny::req(dash_rfm(), input$m_radio)

        if (input$m_radio == "hist") {
          rfm_distribution_plot(dt = dash_rfm(),
                                var = "monetary",
                                zoom = input$m_slider,
                                bins = input$m_bins,
                                log_var = input$m_log,
                                interactive = TRUE)
        } else if (input$m_radio == "box") {
          rfm_boxplot(dt = dash_rfm(),
                      var = "monetary",
                      zoom = input$m_slider,
                      log_var = input$m_log,
                      interactive = TRUE)
        }

      })

      output$m_dist_title <- shiny::renderText({
        if (input$m_log) {
          "Monetary Distribution (Log)"
        } else {
          "Monetary Distribution"
        }
      })

      # Date Data ------------------------------------------------------|
      date_dt <- shiny::reactive({
        create_date_dt(dash_rfm())
      })

      # quarterly ----------------------------------------------------|
      output$quarterly_intake <- plotly::renderPlotly({
        shiny::req(date_dt())
        quarterly_intake(dt = date_dt(),
                         geom = input$qtr_radio,
                         interactive = TRUE)
      })

      # Monthly ------------------------------------------------------|
      output$monthly_intake <- plotly::renderPlotly({
        shiny::req(date_dt())

        monthly_intake(dt = date_dt(),
                       geom = input$mty_radio,
                       interactive = TRUE)
      })

      # Push Data to RFM analysis --------------------------------------|
      module_output <- shiny::reactive({
        list(main_data = dash_data(),
             rfm_data = dash_rfm(),          # <-> data
             analysis_date = analysis_date())
      })

      return(module_output)
    }
  )
}

## To be copied in the UI
# mod_rfm_description_ui("rfm_description_1")

## To be copied in the server
# mod_rfm_description_server("rfm_description_1")
