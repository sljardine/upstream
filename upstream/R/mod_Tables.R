#' Tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Tables_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        width = 6,
        DT::dataTableOutput(ns('data_table'))),
    )
  )
}

#' Tables Server Functions
#'
#' @noRd
mod_Tables_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    store_table <- eventReactive(c(r$submit_suggest,
      r$submit_custom),
      {shinipsum::random_DT(5, 3, "numeric")
     })

    output$data_table <- DT::renderDT({store_table()})
  })
}
