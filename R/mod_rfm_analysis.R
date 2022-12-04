#' rfm_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rfm_analysis_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shinyWidgets::panel(
      shiny::fluidRow(
        shiny::column(
          width = 3,

          shinyWidgets::prettyRadioButtons(
            inputId = ns("how_assign_rfm_scores"),
            label = "How to assign RFM score",
            choiceNames = c("Quantile-based Discretization",
                            "K-means clustering",
                            "Manual Discretization"),
            choiceValues = c("QBD", "kmeans", "MD"),
            selected = "QBD",
            status = "info",
            shape = "curve",
            thick = TRUE,
            bigger = TRUE,
            animation = "pulse"
            ),
        ),

        shiny::column(
          width = 5,
          align = "center",

          shinyWidgets::actionBttn(
            inputId = ns("alg_info"),
            label = "",
            icon = icon("info"),
            style = "material-circle"
          )
        ),

        shiny::column(
          width = 3,
          align = "left",

          shinyWidgets::prettyRadioButtons(
            inputId = ns("log_trans_opt"),
            label = "Log Transform",
            choices = c("None" = "none", "All" = "all", "Some" = "some"),
            selected = "none",
            status = "info",
            shape = "curve",
            thick = TRUE,
            bigger = TRUE,
            animation = "pulse",
            inline = TRUE
          ),

          shinyWidgets::pickerInput(
            inputId = ns("some_log_trans_opt"),
            label = "",
            choices = c("Recency" = "recency",
                        "Frequency" = "frequency",
                        "Monetary" = "monetary"),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(style = "btn-default",
                                                  multipleSeparator = " | ")
          ) |>
            shinyjs::hidden()
        )
      )
    ),

    # Manual Discretization -----------------------------------------------|
    shiny::fluidRow(
      id = ns("MD_row"),

      bs4Dash::box(
        shiny::textInput(
          inputId = ns("MD_recency_bins"),
          value = "",
          placeholder = "2, 25, ...",
          label = ""
          ),

        dropdownMenu = bs4Dash::boxDropdown(
          bs4Dash::boxDropdownItem(uiOutput(outputId = ns("MD_r_bins_info"))),
          icon = shiny::icon("info")
        ),

        width = 4,
        title = "Recency bins"
      ),

      bs4Dash::box(
        shiny::textInput(inputId = ns("MD_frequency_bins"),
                         value = "",
                         placeholder = "1, 5, ...",
                         label = ""
                         ),

        dropdownMenu = bs4Dash::boxDropdown(
          bs4Dash::boxDropdownItem(uiOutput(outputId = ns("MD_f_bins_info"))),
          icon = shiny::icon("info")
        ),

        width = 4,
        title = "Frequency bins"
      ),

      bs4Dash::box(
        shiny::textInput(inputId = ns("MD_monetary_bins"),
                         value = "",
                         placeholder = "0.14, 50, ...",
                         label = ""),

        dropdownMenu = bs4Dash::boxDropdown(
          bs4Dash::boxDropdownItem(uiOutput(outputId = ns("MD_m_bins_info"))),
          icon = shiny::icon("info")
        ),

        width = 4,
        title = "Monetary bins"
      )
    ),

    # Quantile-based discretization ----------------------------------------|
    shiny::fluidRow(
      id = ns("QBD_row"),

      bs4Dash::box(
        shiny::numericInput(
          inputId = ns("QBD_recency_bins"),
          label = "",
          value = 5,
          min = 2, max = 10, step = 1
          ),

        dropdownMenu = bs4Dash::boxDropdown(
          bs4Dash::boxDropdownItem(qbd_value_info()),
          icon = shiny::icon("info")
        ),

        width = 4,
        title = "Recency bins"
      ),

      bs4Dash::box(
        shiny::numericInput(
          inputId = ns("QBD_frequency_bins"),
          label = "",
          value = 5,
          min = 2, max = 10, step = 1
          ),

        dropdownMenu = bs4Dash::boxDropdown(
          bs4Dash::boxDropdownItem(qbd_value_info()),
          icon = shiny::icon("info")
        ),

        width = 4,
        title = "Frequency bins"
      ),

      bs4Dash::box(
        shiny::numericInput(
          inputId = ns("QBD_monetary_bins"),
          label = "",
          value = 5,
          min = 2, max = 10, step = 1
          ),

        dropdownMenu = bs4Dash::boxDropdown(
          bs4Dash::boxDropdownItem(qbd_value_info()),
          icon = shiny::icon("info")
        ),

        width = 4,
        title = "Monetary bins"
      )
    ),

    # K-means clustering ---------------------------------------------------|
    shiny::fluidRow(
      id = ns("kmeans_row"),
      # align = "center",

      shiny::column(
        width = 4,

        shinyWidgets::panel(
          shiny::numericInput(
            inputId = ns("n_centers"),
            label = "Number Of Centers",
            min = 3, max = 10, step = 1, value = 5
            ),

          shinyWidgets::prettyToggle(
            inputId = ns("set_seed"),
            label_on = "Reproducible",
            label_off = "Random",
            value = TRUE,
            bigger = TRUE,
            animation = "pulse",
            shape = "curve"
            )
        )
      ),
    ),

    shiny::fluidRow(
      shiny::column(
        width = 4,

        shinyWidgets::actionBttn(
          inputId = ns("run_analysis"),
          label = "Run",
          style = "fill",
          color = "royal",
          size = "md",
          icon = fontawesome::fa_i("fas fa-arrows-spin")
        )
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      bs4Dash::box(
        reactable::reactableOutput(outputId = ns("rfm_analysis_table")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        width = 12,
        collapsible = FALSE
      )
    )
  )
}




#' rfm_analysis Server Functions
#'
#' @noRd
mod_rfm_analysis_server <- function(id, dash_lst, parent_session){
  stopifnot(shiny::is.reactive(dash_lst))
  shiny::moduleServer(
    id = id,

    module = function(input, output, session){
      ns <- session$ns


      dash_dt<- shiny::reactive({ dash_lst()$rfm_data })

      # toggle log transform 'some' options --------------------------|
      shiny::observe({
        if (input$log_trans_opt == "some") {
          shinyjs::show(
            id = "some_log_trans_opt",
            anim = TRUE,
            animType = "slide"
          )
        } else {
          shinyjs::hide(
            id = "some_log_trans_opt",
            anim = TRUE,
            animType = "slide"
          )
        }
      })

      # log transform -------------------------------------------------|
      dash_rfm <- shiny::reactive({
        shiny::req(input$log_trans_opt)

        if (input$log_trans_opt != "some") {
          log_transform(dt = dash_dt(), var = input$log_trans_opt)
        } else {
          if (!is.null(input$some_log_trans_opt)) {
            log_transform(dt = dash_dt(), var = input$some_log_trans_opt)
          } else {
            dash_dt()
          }
        }
      })

      # toggle between rfm algorithm -----------------------------------|
      shiny::observe({
        if (input$how_assign_rfm_scores == "QBD") {
          shinyjs::show(id = "QBD_row")
        } else {
          shinyjs::hide(id = "QBD_row")
        }
        if (input$how_assign_rfm_scores == "kmeans") {
          shinyjs::show(id = "kmeans_row")
        } else {
          shinyjs::hide(id = "kmeans_row")
        }
        if (input$how_assign_rfm_scores == "MD") {
          shinyjs::show(id = "MD_row")
        } else {
          shinyjs::hide(id = "MD_row")
        }
      })

      # Info -----------------------------------------------------------|
      shiny::observe({

        if (input$how_assign_rfm_scores == "QBD") {
          show_message <- info_message$QBD
          title <- "Quantile based Discretization"

        } else if (input$how_assign_rfm_scores == "MD") {
          show_message <- info_message$MD
          title <- "Manual Discretization"

        } else if (input$how_assign_rfm_scores == "kmeans") {
          show_message <- info_message$kmeans
          title <- "K-means clustering"
        }

        shinyWidgets::show_toast(
          title = title,
          text = show_message,
          type = "info",
          timer = 15000,
          position = "center",
          width = 600
        )
      }) |>
        bindEvent(input$alg_info)

      # MD Info --------------------------------------------------------|
      output$MD_r_bins_info <- shiny::renderUI({
        md_info(min(dash_rfm()$recency), max(dash_rfm()$recency))
      })
      output$MD_f_bins_info <- shiny::renderUI({
        md_info(min(dash_rfm()$frequency), max(dash_rfm()$frequency))
      })
      output$MD_m_bins_info <- shiny::renderUI({
        md_info(min(dash_rfm()$monetary), max(dash_rfm()$monetary))
      })

      # Get user bins --------------------------------------------------|
      user_bins <- shiny::reactive({
        if (input$how_assign_rfm_scores == "MD") {
          list(
            r = get_user_bins(input$MD_recency_bins),
            f = get_user_bins(input$MD_frequency_bins),
            m = get_user_bins(input$MD_monetary_bins)
          )
        } else if (input$how_assign_rfm_scores == "QBD") {
          list(
            r = input$QBD_recency_bins,
            f = input$QBD_frequency_bins,
            m = input$QBD_monetary_bins
          )
        }
      })

      # Bad Input alert --------------------------------------------------|
      shiny::observe({
        shiny::req(user_bins())

        if (input$how_assign_rfm_scores == "QBD") {
          if (check_bin_length(user_bins())) {
            shinyWidgets::show_alert(
              title = "Unequal Number Of Bins",
              text = "The number of bins must be the same for all metrics",
              type = "error"
            )

          } else if (check_bin_value(user_bins())) {
            shinyWidgets::show_alert(
              title = "Bad Bin Value",
              text = "Only a minimum bin value of 3 can be used in each metric when a single value is used.",
              type = "error"
            )
          }

        } else if (input$how_assign_rfm_scores == "MD") {
          missing_val <- m_include_range(dash_rfm(), user_bins())

          if (check_unique_bins(user_bins())$logical) {
            shinyWidgets::show_alert(
              title = "Unique Bins",
              text = paste(
                "All values in each metric must have unique values,",
                alert_metric[[check_unique_bins(user_bins())$name]],
                "values is not unique."
              ),
              type = "error"
            )

          } else if (missing_val$logical) {
            shinyWidgets::show_alert(
              title = "Missing values",
              text = missing_val$text,
              type = "error"
            )
          }
        }
      }) |>
        shiny::bindEvent(input$run_analysis)

      # valid values ---------------------------------------------------------|
      run_safe_analysis <- shiny::reactive({
        if (input$how_assign_rfm_scores == "QBD") {
          bin_length <- check_bin_length(user_bins())
          bin_value <- check_bin_value(user_bins())

          if (isFALSE(bin_length) && isFALSE(bin_value)) {
            TRUE
          } else {
            FALSE
          }

        } else if (input$how_assign_rfm_scores == "MD") {
          unique_bin <- check_unique_bins(user_bins())$logical
          missing_bin <- m_include_range(dash_rfm(), user_bins())$logical

          if (isFALSE(unique_bin) && isFALSE(missing_bin)) {
            TRUE
          } else {
            FALSE
          }
        }
      })

      # Run Analysis --------------------------------------------------------|
      rfm_dt <- shiny::reactive({
        if (input$how_assign_rfm_scores %in% c("MD", "QBD")) {
          shiny::req(user_bins(), run_safe_analysis())

          rfm_table(dt = dash_rfm(),
                    n_transactions = "frequency",
                    recency_days = "recency",
                    total_revenue = "monetary",
                    recency_bins = user_bins()$r,
                    frequency_bins = user_bins()$f,
                    monetary_bins = user_bins()$m)

        } else if (input$how_assign_rfm_scores == "kmeans") {
          u_seed <- ifelse(input$set_seed, 11, sample(1:1000, 1))

          rfm_kmean_score(dt = dash_rfm(),
                          n_centers = input$n_centers,
                          seed = u_seed)
        }
      }) |> shiny::bindEvent(input$run_analysis)


      output$rfm_analysis_table <- reactable::renderReactable({
        shiny::req(rfm_dt())

        clean_rfm_names(rfm_dt()) |>
          rfm_reactable_output()
      })

      # Output data.frame --------------------------------------------|
      return(rfm_dt)
    }
  )
}
