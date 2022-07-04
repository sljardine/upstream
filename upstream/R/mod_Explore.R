#' Explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Explore_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        selectizeInput(
          inputId = ns("area_sel"),
          label = "Select Area",
          # Hard coded for now, but we'll deal with this later!
          choices = c("Pilot", "Clallam County", "Grays Harbor County",
                      "Island County", "Jefferson County", "etc."),
          selected = NULL,
          width = '50%',
          multiple = TRUE)
      ),
      fluidRow(
        selectizeInput(
          inputId = ns("owner_sel"),
          label = "Select Ownership",
          # Hard coded for now, but we'll deal with this later!
          choices = c("All", "County", "City", "State", "Tribal", "etc."),
          selected = NULL,
          width = '50%',
          multiple = TRUE)
      ),
      fluidRow(
        textInput(
          inputId = ns("barrierIDs"),
          label = "Enter IDs",
          placeholder = "Enter WDFW Barrier IDs")
      ),
      fluidRow(
        actionButton(ns("submit"), "Submit")
      )
    )
  )
}

#' Explore Server Functions
#'
#' @noRd
mod_Explore_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$submit, {
      if(input$barrierIDs != "" && !is.null(input$owner_sel) && !is.null(input$area_sel))
      {r$submit_explore <- input$submit}
      else
      {showModal(modalDialog(title = "Warning!", 
        "Please fill all the fields before you click the Submit buttion."))}
    })
    
  })
}

