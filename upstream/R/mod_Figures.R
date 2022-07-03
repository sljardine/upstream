#' Figures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Figures_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        width = 11,
        plotOutput(ns("plot"))
      )
    )
  )
}



#' Figures Server Functions
#'
#' @noRd
mod_Figures_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    store_plot <- eventReactive(c(r$submit_explore,
      r$submit_suggest,
      r$submit_custom),
      {shinipsum::random_ggplot(type = "point")
      })

    output$plot <- renderPlot({store_plot()})
    })
}
