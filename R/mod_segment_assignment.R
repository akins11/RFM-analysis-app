#' segment_assignment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_segment_assignment_ui <- function(id){
  ns <- NS(id)

  shiny::tagList(
    shiny::fluidRow(
      align = "center",

      shiny::column(
        width = 4,
        offset = 4,

        shinyWidgets::panel(
          shiny::numericInput(
            inputId = ns("n_segment"),
            label = tags$h5("Number of Segment"),
            value = 5, min = 2, max = 10,
            width = "300px"
            ) # Change to 4
        )
      )
    ),

    shiny::fluidRow(
      bs4Dash::box(
        uiOutput(outputId = ns("segment_name_ui")),

        width = 3,
        title = "Segment Name"
      ),

      bs4Dash::box(
        shiny::uiOutput(outputId = ns("recency_bins_ui")),

        width = 3,
        title = "Recency bins",
        collapsed = TRUE
      ),

      bs4Dash::box(
        shiny::uiOutput(outputId = ns("frequency_bins_ui")),

        width = 3,
        title = "Frequency bins",
        collapsed = TRUE
      ),

      bs4Dash::box(
        shiny::uiOutput(outputId = ns("monetary_bins_ui")),

        width = 3,
        title = "Monetary bins",
        collapsed = TRUE
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 4,

        shinyWidgets::actionBttn(
          inputId = ns("create_segment"),
          label = "assign",
          style = "fill",
          color = "royal",
          size = "md",
          icon = fontawesome::fa_i("fas fa-layer-group", verify_fa = FALSE)
        )
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      bs4Dash::box(
        reactable::reactableOutput(outputId = ns("segment_table")) |>
          shinycssloaders::withSpinner(type = 4, color = spinner_color),

        width = 12,
        collapsible = FALSE

      )
    )
  )
}





#' segment_assignment Server Functions
#'
#' @param id
#' @param rfm_data
#' @param parent_session
#'
#' @noRd
mod_segment_assignment_server <- function(id, rfm_data, parent_session) {
  stopifnot(shiny::is.reactive(rfm_data))

  moduleServer(
    id = id,

    module = function(input, output, session) {
      ns <- session$ns

      n_seg <- shiny::reactive(as.integer(input$n_segment))

      range_value <- shiny::reactive({
        req(rfm_data())

        rfm_min_max(rfm_data())
      })

      output$segment_name_ui <- shiny::renderUI({
        shiny::req(input$n_segment)

        lapply(
          seq_len(n_seg()), function(.x) {
            shiny::tagList(
              shiny::textInput(
                inputId = session$ns(paste0("segment_", .x)),
                label = "",
                value = segment_names_val[.x],     ## Remove
                placeholder = paste("Segment", .x)
                ),
              shiny::tags$br()
            )
          }
        )
      })
      output$recency_bins_ui <- shiny::renderUI({
        shiny::req(input$n_segment)

        lapply(
          seq_len(n_seg()), function(.x) {
            shiny::tagList(
              # chooseSliderSkin("Round"),
              shiny::sliderInput(
                inputId = session$ns(paste0("recency_", .x)),
                label = "",
                # value = range_value()[["R"]][1:2],
                value = recency_val[[.x]],
                min  = range_value()[["R"]][1],
                max  = range_value()[["R"]][2],
                step = 1
                )
            )
          }
        )
      })
      output$frequency_bins_ui <- shiny::renderUI({
        shiny::req(input$n_segment)

        lapply(
          seq_len(n_seg()), function(.x) {
            shiny::tagList(
              # chooseSliderSkin("Round"),
              shiny::sliderInput(
                inputId = session$ns(paste0("frequency_", .x)),
                label = "",
                # value = range_value()[["F"]][1:2],
                value = f_m_val[[.x]],
                min  = range_value()[["F"]][1],
                max  = range_value()[["F"]][2],
                step = 1
                )
            )
          }
        )
      })
      output$monetary_bins_ui <- shiny::renderUI({
        shiny::req(input$n_segment)

        lapply(
          seq_len(n_seg()), function(.x) {
            shiny::tagList(
              # chooseSliderSkin("Round"),
              shiny::sliderInput(
                inputId = session$ns(paste0("monetary_", .x)),
                label = "",
                # value = range_value()[["M"]][1:2],
                value = f_m_val[[.x]],
                min  = range_value()[["M"]][1],
                max  = range_value()[["M"]][2],
                step = 1
                )
            )
          }
        )
      })

      # Collect values ----------------------------------------------------|
      segment_value <- shiny::reactive({
        list(lapply(seq_len(n_seg()), \(.x) input[[paste0("segment_", .x)]])) |>
          unlist()
      })

      recency <- shiny::reactive({
        shiny::req(input[[paste0("recency_", n_seg())]])
        collect_val <- list(
          lapply(seq_len(n_seg()), \(.x) input[[paste0("recency_", .x)]])
        )
        list(lower = ext_bins(collect_val[[1]], n_seg(), 1),
             upper = ext_bins(collect_val[[1]], n_seg(), 2))
      })
      frequency <- shiny::reactive({
        shiny::req(input[[paste0("frequency_", n_seg())]])
        collect_val <- list(
          lapply(seq_len(n_seg()), \(.x) input[[paste0("frequency_", .x)]])
        )
        list(lower = ext_bins(collect_val[[1]], n_seg(), 1),
             upper = ext_bins(collect_val[[1]], n_seg(), 2))
      })
      monetary <- shiny::reactive({
        shiny::req(input[[paste0("monetary_", n_seg())]])
        collect_val <- list(
          lapply(seq_len(n_seg()), \(.x) input[[paste0("monetary_", .x)]])
        )
        list(lower = ext_bins(collect_val[[1]], n_seg(), 1),
             upper = ext_bins(collect_val[[1]], n_seg(), 2))
      })

      seg_data <- shiny::reactive({
        shiny::req(rfm_data(), segment_value(), recency(), frequency(), monetary())

        assign_segment(dt = rfm_data(),
                       segment_names = segment_value(),
                       recency_lower = recency()$lower,
                       recency_upper = recency()$upper,
                       frequency_lower = frequency()$lower,
                       frequency_upper = frequency()$upper,
                       monetary_lower = monetary()$lower,
                       monetary_upper = monetary()$upper)
      }) |>
        shiny::bindEvent(input$create_segment)

      output$segment_table <- reactable::renderReactable({
        shiny::req(seg_data())

        clean_seg_data_name(seg_data()) |>
          segment_reactable()
      })

      return(seg_data)
    }
  )
}

