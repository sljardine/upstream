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
                      "Island County", "Jefferson County", "Species"),
          selected = NULL,
          width = '50%',
          multiple = TRUE)
      ),
      fluidRow(
        selectizeInput(
          inputId = ns("owner_sel"),
          label = "Select Ownership Group",
          # Hard coded for now, but we'll deal with this later!
          choices = c("All", "County", "City", "State", "Tribal", "Species"),
          selected = NULL,
          width = '50%',
          multiple = TRUE)
      ),
      fluidRow(
        selectInput(ns("plot_type"), 
          label = "Select Plot Type",
          choices = list("Scatterplot", 
          "Histogram"),
          selected = "Scatterplot")
      ),
      fluidRow(
        conditionalPanel(
          condition = "input.plot_type == 'Scatterplot'",
          ns = ns,
        column(6,
          selectizeInput(
          inputId = ns("x"),
          label = "Variable on X axis",
          # Hard coded for now, but we'll deal with this later!
          choices = c("Habitat Quantity", "Cost Estimate", "Downstream Barriers",
          "Species"),
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
            "Species"),
            selected = "Habitat Quantity",
            width = "100%"),
          offset = 0
          )
        )
      ),
      conditionalPanel("input.plot_type == 'Histogram'",
        ns = ns,
        selectInput(inputId = ns("var"),
        label = "Variable to display",
        choices = c("Habitat Quantity", "Cost Estimate", 
          "Downstream Barriers", "Species"),
        selected = "Habitat Quantity"),
        # number of bins for numerical vars
      conditionalPanel("input.var != 'Species'",
        sliderInput(inputId = ns("nbins"),
        label = "Number of bins",
        min = 1, max = 100, value = 30))),
      fluidRow(
        radioButtons(inputId = ns("highlight"),
          label = "Highlight Barrier(s)",
          choices = list("No" = 1, "Yes" = 2), 
          width = '100%')
      ),
      fluidRow(
        conditionalPanel(
          condition = "input.highlight == 2",
          ns = ns,
          textInput(
            inputId = ns("barrier_ids"),
            label = "Enter ID(s) to Highlight",
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

#' Explore Server Functions
#'
#' @noRd
mod_Explore_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$submit, {
      if(!is.null(input$owner_sel) && !is.null(input$area_sel) && 
         input$highlight == 1)
      {r$submit_explore <- input$submit}
      else
        if(!is.null(input$owner_sel) && !is.null(input$area_sel) && 
           input$highlight == 2 && input$barrier_ids != "")
        {r$submit_explore <- input$submit}
          else
      {showModal(modalDialog(title = "Warning!", 
        "Please fill all the fields before you click the Submit button."))}
    })
    
  })
}

