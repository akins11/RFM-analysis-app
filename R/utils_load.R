ui_spinner <- function(ui_element, s_color = spinner_color) {
  shinycssloaders::withSpinner(ui_element = ui_element, type = 4, color = s_color)
}
