#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  r <- reactiveValues()
  
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

}
