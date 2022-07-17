#' Suggest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Suggest_ui <- function(id){
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
        radioButtons(inputId = "obj",
          label = "Objective",
          choices = list("Enter Weights" = 1, "Upload PI Scores" = 2)),
        conditionalPanel(
          condition = "input.obj == 1",
          column(2,  "Habitat Quantity"),
          column(4,
            numericInput(inputId = ns("w1"),
              min = 0,
              max = 1,
              step = 0.01,
              label = NULL,
              value = 0.5)),
          column(2, "Habitat Quality"),
          column(4,
            numericInput(inputId = ns("w2"),
            min = 0,
            max = 1,
            step = 0.01,
            label = NULL,
            value = 0.5))
        ),
        conditionalPanel(
          condition = "input.obj == 2",
          fileInput(inputId = ns("CustomScore"), 'Choose xlsx file',
                    accept = c(".xlsx")
          )
        )
      ),
      fluidRow(
        radioButtons(inputId = "cost",
                     label = "Cost",
                     choices = list("Default" = 1, "Upload Costs" = 2),
                     selected = NULL),
        conditionalPanel(
          condition = "input.cost == 2",
          fileInput(inputId = ns("CustomCost"), 'Choose xlsx file',
                    accept = c(".xlsx")
          )
        )
      ),
      fluidRow(
        numericInput(inputId = ns("budget"),
          label = "Enter Budget ($)",
          min = 0,
          value = NULL)
      ),
        fluidRow(
          actionButton(ns("submit"), "Submit")
      )
    )
  )
}

#' Suggest Server Functions
#'
#' @noRd
mod_Suggest_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$w1, {
      updateNumericInput(session, 'w2', 
                         value = 1 - input$w1)
    })
    
    observeEvent(input$submit, {
      if(!is.null(input$owner_sel) && !is.null(input$area_sel) && !is.na(input$budget))
      {r$submit_suggest <- input$submit}
      else
      {showModal(modalDialog(title = "Warning!",
      "Please fill all the fields before you click the Submit buttion."))}
    })
  })
}

