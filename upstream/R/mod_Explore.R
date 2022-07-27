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
          label = "Select Ownership Group",
          # Hard coded for now, but we'll deal with this later!
          choices = c("All", "County", "City", "State", "Tribal", "etc."),
          selected = NULL,
          width = '50%',
          multiple = TRUE)
      ),
      fluidRow(
        radioButtons(inputId = ns("action"),
                     label = "Select Action",
                     choices = list("Inspect all barriers in area and ownership group" = 1,
                                    "Compare a set of barriers to all other barriers in area and ownership group" = 2),
                     width = '100%')
      ),
      fluidRow(
        conditionalPanel(
          condition = "input.action == 2",
          ns = ns,
            textInput(
              inputId = ns("barrier_ids"),
              label = "Enter ID(s) of Interest",
              placeholder = "Enter WDFW Barrier IDs",
              width = "100%")
        )
      ),
      fluidRow(
        column(6,
          selectizeInput(
          inputId = ns("x"),
          label = "Variable on X axis",
          # Hard coded for now, but we'll deal with this later!
          choices = c("Habitat Quantity", "Cost Estimate", "Downstream Barriers",
          "etc."),
          selected = "Cost Estimate",
          width = "100%"),
          offset = 0
        ),
        column(6,
          selectizeInput(
            inputId = ns("y"),
            label = "Variable on Y axis",
            # Hard coded for now, but we'll deal with this later!
            choices = c("Habitat Quantity", "Cost Estimate", "Downstream Barriers",
            "etc."),
            selected = "Habitat Quantity",
            width = "100%"),
          offset = 0
        )
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
      if(!is.null(input$owner_sel) && !is.null(input$area_sel) && 
         input$action == 1)
      {r$submit_explore <- input$submit}
      else
        if(!is.null(input$owner_sel) && !is.null(input$area_sel) && 
           input$action == 2 && input$barrier_ids != "")
        {r$submit_explore <- input$submit}
          else
      {showModal(modalDialog(title = "Warning!", 
        "Please fill all the fields before you click the Submit button."))}
    })
    
  })
}

