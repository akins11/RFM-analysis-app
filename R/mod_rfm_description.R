
#' rfm_description UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rfm_description_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::uiOutput(outputId = ns("n_customer_vbox"))
      ),

      shiny::column(
        width = 3,
        shiny::uiOutput(outputId = ns("n_product_vbox"))
      ),

      shiny::column(
        width = 3,
        shiny::uiOutput(outputId = ns("total_sales_vbox"))
      ),

      shiny::column(
        width = 3,
        shiny::uiOutput(outputId = ns("average_lt_vbox"))
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      bs4Dash::box(
        echarts4r::echarts4rOutput(outputId = ns("recency_dis_out")) |>
          ui_spinner(),

        sidebar = distibution_options(ns("r_sidebar"),
                                      ns("r_slider"),
                                      ns("r_radio"),
                                      ns("r_bins"),
                                      ns("r_log")),

        title = shiny::textOutput(outputId = ns("r_dist_title"), inline = TRUE),
        icon = fontawesome::fa("fas fa-hourglass-end",
                               fill = "#68228B", height = "1.5em")
      ),

      bs4Dash::box(
        echarts4r::echarts4rOutput(outputId = ns("frequency_dis_out")) |>
          ui_spinner(),

        sidebar = distibution_options(ns("f_sidebar"),
                                      ns("f_slider"),
                                      ns("f_radio"),
                                      ns("f_bins"),
                                      ns("f_log")),

        title = shiny::textOutput(outputId = ns("f_dist_title"), inline = TRUE),
        icon = fontawesome::fa("fas fa-rotate",
                               fill = "#68228B", height = "1.5em")
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        echarts4r::echarts4rOutput(outputId = ns("monetary_dis_out")) |>
          ui_spinner(),

        sidebar = distibution_options(ns("m_sidebar"),
                                      ns("m_slider"),
                                      ns("m_radio"),
                                      ns("m_bins"),
                                      ns("m_log")),

        title = shiny::textOutput(outputId = ns("m_dist_title"), inline = TRUE),
        icon = fontawesome::fa("fas fa-money-bills",
                               fill = "#68228B", height = "1.5em")
      ),

      bs4Dash::box(
        echarts4r::echarts4rOutput(outputId = ns("quarterly_intake")) |>
          ui_spinner(),

        sidebar = date_plot_option(ns("qtr_sidebar"), ns("qtr_radio")),

        title = "Quarterly Customer Intake",
        icon = fontawesome::fa("fas fa-calendar-day",
                               fill = "#68228B", height = "1.5em")
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        echarts4r::echarts4rOutput(outputId = ns("monthly_intake")) |>
          ui_spinner(),

        sidebar = date_plot_option(ns("mty_sidebar"), ns("mty_radio"), 25),

        width = 12,
        title = "Monthly Customer Intake",
        icon = fontawesome::fa("fas fa-calendar-days",
                               fill = "#68228B", height = "1.5em")
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

      # Load Data ----------------------------------------------------------|
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

      # Update value box ---------------------------------------------------|
      output$n_customer_vbox <- shiny::renderUI({
        value_card(
          value = get_value(dt = dash_data(), what = "n_customers"),
          label = "Unique Customers",
          icon = "users"
        )
      })

      output$n_product_vbox <- shiny::renderUI({
        value_card(
          value = get_value(dt = dash_data(), what = "n_product"),
          label = "Unique Products",
          icon = "cart-shopping"
        )
      })

      output$total_sales_vbox <- shiny::renderUI({
        value_card(
          value = get_value(dt = dash_data(), what = "total_revenue"),
          label = "Total Revenue",
          icon = "coins"
        )
      })

      output$average_lt_vbox <- shiny::renderUI({
        value_card(
          value = get_value(rfm_dt = dash_rfm(), what = "average_lt"),
          label = "Average Customer Life Time",
          icon = "hourglass-half"
        )
      })

      # Distribution Plots ------------------------------------------------|
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

      output$recency_dis_out <- echarts4r::renderEcharts4r({
        shiny::req(dash_rfm(), input$r_radio)

        if (input$r_radio %in% c("hist", "dens")) {
          rfm_distribution_plot(dt = dash_rfm(),
                                var = "recency",
                                type = input$r_radio,
                                zoom = input$r_slider,
                                bins = input$r_bins,
                                log_var = input$r_log)

        } else if (input$r_radio == "box") {
          rfm_boxplot(dt = dash_rfm(),
                      var = "recency",
                      zoom = input$r_slider,
                      log_var = input$r_log)
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

      output$frequency_dis_out <- echarts4r::renderEcharts4r({
        shiny::req(dash_rfm(), input$f_radio)

        if (input$f_radio %in% c("hist", "dens")) {
          rfm_distribution_plot(dt = dash_rfm(),
                                var = "frequency",
                                type = input$f_radio,
                                zoom = input$f_slider,
                                bins = input$f_bins,
                                log_var = input$f_log)

        } else if (input$f_radio == "box") {
          rfm_boxplot(dt = dash_rfm(),
                      var = "frequency",
                      zoom = input$f_slider,
                      log_var = input$f_log)
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

      output$monetary_dis_out <- echarts4r::renderEcharts4r({
        shiny::req(dash_rfm(), input$m_radio)

        if (input$m_radio %in% c("hist", "dens")) {
          rfm_distribution_plot(dt = dash_rfm(),
                                var = "monetary",
                                type = input$m_radio,
                                zoom = input$m_slider,
                                bins = input$m_bins,
                                log_var = input$m_log)

        } else if (input$m_radio == "box") {
          rfm_boxplot(dt = dash_rfm(),
                      var = "monetary",
                      zoom = input$m_slider,
                      log_var = input$m_log)
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
      output$quarterly_intake <- echarts4r::renderEcharts4r({
        shiny::req(date_dt())

        quarterly_intake(dt = date_dt(),
                         geom = input$qtr_radio)
      })

      # Monthly ------------------------------------------------------|
      output$monthly_intake <- echarts4r::renderEcharts4r({
        shiny::req(date_dt())

        monthly_intake(dt = date_dt(),
                       geom = input$mty_radio)
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
