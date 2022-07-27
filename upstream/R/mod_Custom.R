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
        radioButtons(inputId = ns("plans"),
          label = "How Many Plans to Compare?",
          choices = list("1" = 1, "2" = 2, "3" = 3),
          selected = NULL)
        ),
        fluidRow(
          textInput(
          inputId = ns("barrier_ids1"),
          label = "Enter ID(s) for Plan 1",
          placeholder = "Enter WDFW Barrier IDs",
          width = "100%")
         ),
      fluidRow(
        conditionalPanel(
          condition = "input.plans == 2 || input.plans == 3",
          ns = ns,
            textInput(
            inputId = ns("barrier_ids2"),
            label = "Enter ID(s) for Plan 2",
            placeholder = "Enter WDFW Barrier IDs",
            width = "100%")
       )
      ),
      fluidRow(
        conditionalPanel(
          condition = "input.plans == 3",
          ns = ns,
            textInput(
              inputId = ns("barrier_ids3"),
              label = "Enter ID(s) for Plan 3",
              placeholder = "Enter WDFW Barrier IDs",
              width = "100%")
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
      if(input$plans == 1 && input$barrier_ids1 != "")
      {r$submit_custom <- input$submit}
      else
        if(input$plans == 2 && input$barrier_ids1 != "" && input$barrier_ids2 != "")
        {r$submit_custom <- input$submit}
      else
        if(input$plans == 3 && input$barrier_ids1 != "" && 
           input$barrier_ids2 != "" && input$barrier_ids3 != "")
        {r$submit_custom <- input$submit}
        else
      {showModal(modalDialog(title = "Warning!", 
      "Please enter at least one set of barrier IDs before you click the Submit button."))}
    })
  })
}

