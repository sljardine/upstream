#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  r <- reactiveValues()
  r$plot_brush <- 0
  
  mod_Figures_server("Figures_1", r = r)
  mod_Tables_server("Tables_1", r = r)
  mod_Explore_server("Explore_1", r = r)
  mod_Suggest_server("Suggest_1", r = r)
  mod_Custom_server("Custom_1", r = r)
  mod_Welcome_server("Welcome_1", r = r)
  mod_Learn_server("Learn_1", r = r)
  
  observeEvent(r$tab_sel, {
    shinydashboard::updateTabItems(session, "tabset1", r$tab_sel)
  })
  
  observeEvent(input$tabset1, {
    r$tab_sel <- input$tabset1
  })
  
  # makes figures tab plot zoomable
  observeEvent(input$plot_brush, {
    r$plot_xmin <- input$plot_brush$xmin
    r$plot_xmax <- input$plot_brush$xmax
    r$plot_ymin <- input$plot_brush$ymin
    r$plot_ymax <- input$plot_brush$ymax
    r$plot_brush <- r$plot_brush + 1
  })
  
  # plot click event to identify culvert
  # observeEvent(input$plot_click, {
  #   r$x_axis_variable %>% print()
  #   r$y_axis_variable %>% print()
  #   r$area_sel %>% print()
  #   r$owner_sel %>% print()
  #   input$plot_click$x %>% print()
  #   input$plot_click$y %>% print()
  #   
  #   dfCulverts %>%
  #     dplyr::filter(wria_number == r$area_sel & owner_type_code == r$owner_sel) %>%
  #     dplyr::rename(X = r$x_axis_variable, Y = r$y_axis_variable) %>%
  #     dplyr::mutate(
  #       X = dplyr::case_when(r$x_axis_variable %in% c('cost', 'hmarg', 'hfull') ~ X / 1000, TRUE ~ X),
  #       Y = dplyr::case_when(r$y_axis_variable %in% c('cost', 'hmarg', 'hfull') ~ Y / 1000, TRUE ~ Y)
  #     ) %>%
  #     dplyr::mutate(Diff = sqrt((X - input$plot_click$x)^2 + (Y - input$plot_click$y)^2)) %>%
  #     dplyr::arrange(Diff) %>%
  #     dplyr::slice(1) %>%
  #     dplyr::select(site_id, X, Y, Diff) %>%
  #     print()
  # })
  
  observeEvent(input, input %>% names() %>% print())
}
