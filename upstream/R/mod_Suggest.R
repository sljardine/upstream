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
        radioButtons(inputId = "obj",
          label = "Objective",
          choices = list("Enter Weights" = 1, "Upload PI Scores" = 2),
          selected = NULL),
        conditionalPanel(
          condition = "input.obj == 1",
          column(2,  "Habitat Quantity"),
          column(4,
            numericInput(inputId = ns("hquantW"),
              min = 0,
              max = 1,
              step = 0.01,
              label = NULL,
              value = 0.5)),
          column(2, "Habitat Quality"),
          column(4,
            numericInput(inputId = ns("NewVal4"),
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
          value = 350000)
      ),
      fluidRow(
        selectizeInput(
          inputId = ns("area_sel"),
          label = "Select Area",
          # Hard coded for now, but we'll deal with this later!
          choices = c("Pilot", "Clallam County", "Grays Harbor County",
                      "Island County", "Jefferson County", "etc."),
          selected = "Pilot",
          width = '50%',
          multiple = FALSE)
      ),
      fluidRow(
        selectizeInput(
          inputId = ns("owner_sel"),
          label = "Select Ownership",
          # Hard coded for now, but we'll deal with this later!
          choices = c("All", "County", "City", "State", "Tribal", "etc."),
          selected = "All",
          width = '50%',
          multiple = FALSE)
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
    observeEvent(input$submit, {
      r$submit_suggest <- input$submit
    })
  })
}

