#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  r <- reactiveValues()
  
  # Set the default tab to 'Welcome'
  r$tab_sel <- 'Welcome'

  mod_Figures_server("Figures_1", r = r)
  mod_Tables_server("Tables_1", r = r)
  mod_Explore_server("Explore_1", r = r)
  mod_Suggest_server("Suggest_1", r = r)
  mod_Custom_server("Custom_1", r = r)
  mod_Welcome_server("Welcome_1", r = r)
  mod_Learn_server("Learn_1", r = r)

  # Keep r$tab_sel and the visible tab in step.
  #
  # Server -> browser: switch tabs only when the server asked for a tab the
  # browser is not already on (the Welcome page buttons). ignoreInit matters:
  # the UI already opens on Welcome, and the startup run's "go to Welcome"
  # message queued behind the ~18 MB of initial map data. On a slow connection
  # it landed 30-70 s after load and threw users out of whatever tab they had
  # opened, mid-input. Not echoing browser-originated changes back also removes
  # the need for the old 1 s rate limit, which silently dropped fast tab hops
  # and left r$tab_sel pointing at the wrong tab.
  observeEvent(r$tab_sel, {
    if (!identical(r$tab_sel, input$tabset1)) {
      shinydashboard::updateTabItems(session, "tabset1", r$tab_sel)
    }
  }, ignoreInit = TRUE)

  # Browser -> server: record every tab change.
  observeEvent(input$tabset1, {
    r$tab_sel <- input$tabset1
  })

}
