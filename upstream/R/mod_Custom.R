#' Custom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Custom_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        shinydashboard::box(
        textInput(
        inputId = ns("barrierIDs1"),
            label = "Enter IDs for Plan 1",
            placeholder = "Enter WDFW Barrier IDs")
          )
         ),
      fluidRow(
          shinydashboard::box(
            textInput(
            inputId = ns("barrierIDs2"),
            label = "Enter IDs for Plan 2",
            placeholder = "Enter WDFW Barrier IDs")
        )
      ),
      fluidRow(
        actionButton(ns("submit"), "Submit")
      )
    )
  )
}

#' Custom Server Functions
#'
#' @noRd
mod_Custom_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$submit, {
      r$submit_custom <- input$submit
    })
  })
}

