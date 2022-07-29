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
      # fluidRow(
      #   selectizeInput(inputId = ns("dat_source"),
      #   label = "Select Data Sources",
      #   choices = c("Use upstream data", "Provide benefit data, use upstream cost data", 
      #   "Provide cost data, use upstream benefit data", "Provide benefit & cost data"),
      #   options = list(
      #     placeholder = 'Please select an option below',
      #     onInitialize = I('function() { this.setValue(""); }')
      #     )
      #   )
      # ),
      fluidRow(
        radioButtons(inputId = ns("obj"),
          label = "Objective",
          choices = list("Habitat Quantity" = 1, "Weighted Attributes" = 2, 
            "Custom Barrier Scores" = 3)),
        conditionalPanel(
          condition = "input.obj == 2",
          ns = ns,
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
        )
      ),
      fluidRow(
        radioButtons(inputId = ns("cost"),
          label = "Cost",
          choices = list("Default Predictions" = 1, "Custom Cost Estimates" = 2),
          selected = NULL)
      ),
      fluidRow(
        numericInput(inputId = ns("budget"),
                     label = "Enter Budget ($)",
                     min = 0,
                     value = NULL)
      ),
      fluidRow(
        conditionalPanel(
          condition = "input.obj == 3 && input.cost == 1",
          ns = ns,
          fileInput(inputId = ns("cust_score"), 'Choose xlsx file with: IDs and Scores',
            accept = c(".xlsx")
          )
        ),
        conditionalPanel(
          condition = "input.obj != 3 && input.cost == 2",
          ns = ns,
          fileInput(inputId = ns("cust_cost"), 'Choose xlsx file with: IDs and Costs',
            accept = c(".xlsx")
          )
        ),
        conditionalPanel(
          condition = "input.obj == 3 && input.cost == 2",
          ns = ns,
          fileInput(inputId = ns("cust_score_cost"), 'Choose xlsx file with: IDs, Scores, and Costs',
                    accept = c(".xlsx")
          )
        )
      ),
      # fluidRow(
      #   numericInput(inputId = ns("budget"),
      #     label = "Enter Budget ($)",
      #     min = 0,
      #     value = NULL)
      # ),
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
      if(!is.null(input$owner_sel) && !is.null(input$area_sel) && 
         !is.na(input$budget) && input$obj != 3 && input$cost != 2)
      {r$submit_suggest <- input$submit}
      else
        if(!is.null(input$owner_sel) && !is.null(input$area_sel) && 
           !is.na(input$budget) && input$obj == 3 && 
           !is.null(input$cust_score) && input$cost != 2)
        {r$submit_suggest <- input$submit}
      else
        if(!is.null(input$owner_sel) && !is.null(input$area_sel) && 
           !is.na(input$budget) && input$obj != 3 && 
           input$cost == 2 && !is.null(input$cust_cost))
        {r$submit_suggest <- input$submit}
      else
        if(!is.null(input$owner_sel) && !is.null(input$area_sel) && 
           !is.na(input$budget) && input$obj == 3 && 
           input$cost == 2 && !is.null(input$cust_score_cost))
        {r$submit_suggest <- input$submit}
      else
      {showModal(modalDialog(title = "Warning!",
      "Please fill all the fields before you click the Submit button."))}
    })
  })
}

