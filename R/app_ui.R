app_sidebar_theme <- fresh::use_theme(
  fresh::create_theme(
    fresh::bs4dash_sidebar_light(
      bg = "#9400D3",
      color = "#FFFFFF",
      hover_color = "#D6D6D6",
      submenu_color = "#FFFFFF",
      submenu_active_color = "#D6D6D6"
    ),

    fresh::bs4dash_color(
      blue = "#00FFFF",
      lightblue = "#01DF3A",
      purple = "#9400D3"
    ),

    fresh::bs4dash_status(
      light = "#272c30"
    )
  )
)



#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    bs4Dash::dashboardPage(
      dark = NULL,
      title = "rfm-analysis",

      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(title = "RFM Analysis", color = "purple"),
        status = "white",
        sidebarIcon = fontawesome::fa_i("fas fa-bars-staggered"),
        controlbarIcon = shiny::icon("bars")
      ),

      sidebar = bs4Dash::dashboardSidebar(
        skin = "light",
        inputId = "sidebarState",

        bs4Dash::sidebarMenu(
          id = "sidebar_menu",

          bs4Dash::menuItem(
            "Description",
            tabName = "rfm_description",
            icon = shiny::icon("house")
          ),


          bs4Dash::menuItem(
            text = "Run Analysis",
            icon = fontawesome::fa_i("braille"),
            startExpanded = FALSE,

            bs4Dash::menuSubItem(
              text = "Analysis",
              tabName = "run_rfm_analysis",
              icon = fontawesome::fa_i("circle-notch"),

            ),

            bs4Dash::menuSubItem(
              text = "Summary",
              tabName = "rfm_analysis_summary",
              icon = fontawesome::fa_i("far fa-circle"),
            )
          ),


          bs4Dash::menuItem(
            text = "Segmentation",
            icon = fontawesome::fa_i("braille"),
            startExpanded = FALSE,
            # flat = TRUE,

            bs4Dash::menuSubItem(
              text = "Assignment",
              tabName = "segment_assignment",
              icon = fontawesome::fa_i("circle-notch")
            ),

            bs4Dash::menuSubItem(
              text = "Summary",
              tabName = "segment_summary",
              icon = fontawesome::fa_i("far fa-circle")
            ),

            bs4Dash::menuSubItem(
              text = "Product Level",
              tabName = "product_level_summary",
              icon = fontawesome::fa_i("far fa-circle")
            ),

            bs4Dash::menuSubItem(
              text = "Customer Level",
              tabName = "customer_level_summary",
              icon = fontawesome::fa_i("far fa-circle"),
            )
          )
        )
      ),


      # controlbar = bs4Dash::dashboardControlbar(uiOutput("controlbar")),


      body = bs4Dash::dashboardBody(

        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "rfm_description",
            mod_rfm_description_ui(id = "rfm_description_out")
          ),

          bs4Dash::tabItem(
            tabName = "run_rfm_analysis",
            mod_rfm_analysis_ui(id = "rfm_analysis_out")
          ),

          bs4Dash::tabItem(
            tabName = "rfm_analysis_summary",
            mod_rfm_analysis_summary_ui(id = "rfm_analysis_summary_out")
          ),

          bs4Dash::tabItem(
            tabName = "segment_assignment",
            mod_segment_assignment_ui(id = "segment_assignment_out")
          ),

          bs4Dash::tabItem(
            tabName = "segment_summary",
            mod_segment_summary_ui(id = "segment_summary_out")
          ),

          bs4Dash::tabItem(
            tabName = "product_level_summary",
            mod_product_level_ui(id = "product_level_summary_out")
          ),

          bs4Dash::tabItem(
            tabName = "customer_level_summary",
            mod_customer_level_ui(id = "customer_level_summary_out")
          )
        )
      ),


      footer = bs4Dash::dashboardFooter(),
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ico = "dibcon"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "rfm"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(),
    waiter::useWaiter(),
    shinyWidgets::chooseSliderSkin("Flat", "#00FFFF"),
    app_sidebar_theme
  )
}
