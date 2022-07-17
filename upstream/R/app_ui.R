#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "Upstream",
      tags$li(class = "dropdown",
      tags$img(src = "www/no_logo.png", height = "50"))),
      shinydashboard::dashboardSidebar(disable = TRUE),
      shinydashboard::dashboardBody(
        shinydashboard::tabBox(
          title = "",
          id = "tabset1",
          side = "left", height = "750px",
          selected = "Explore",
          shiny::tabPanel("Explore", mod_Explore_ui("Explore_1")),
          shiny::tabPanel("Suggest", mod_Suggest_ui("Suggest_1")),
          shiny::tabPanel("Custom", mod_Custom_ui("Custom_1"))
        ),
      fluidRow(
        shinydashboard::tabBox(
          title = "",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset2", 
          side = "left", height = "750px",
          shiny::tabPanel("Figures", mod_Figures_ui("Figures_1")),
          shiny::tabPanel("Report", mod_Tables_ui("Tables_1"))
        )
       )
     )
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
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Upstream"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

