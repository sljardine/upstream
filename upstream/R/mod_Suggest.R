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
          choices = setNames(
            c(0, wrias %>% dplyr::arrange(WRIA_NM) %>% dplyr::pull(WRIA_NR)),
            nm = c('All WRIAs', wrias %>% dplyr::arrange(WRIA_NM) %>% dplyr::pull(WRIA_NM))
          ),
          selected = 0,
          width = '50%',
          multiple = TRUE
        )
      ),
      fluidRow(
        selectizeInput(
          inputId = ns("owner_sel"),
          label = "Select Ownership Type",
          choices = setNames(
            c(0:9, 11, 12),
            nm = c('All Ownership Types','City', 'County', 'Federal', 'Private', 
              'State', 'Tribal', 'Other', 'Port', 'Drainage District', 'Irrigation District', 'Unknown')
          ),
          selected = 0,
          width = '50%',
          multiple = TRUE
        )
      ),
      fluidRow(
        selectizeInput(
          inputId = ns("species_sel"),
          label = tags$span(style = "color:#c0c0c0", "Select Species of Interest"),
          choices = setNames(
            c(0 : 9),
            nm = c("All", "Bull trout", "Chinook", "Chum", "Coho", 
                   "Pink", "Resident trout", "Sockeye", 
                   "Steelhead", "SR Cutthroad")
          ),
          selected = 0,
          width = '50%',
          multiple = TRUE
        )
      ),
      fluidRow(
        radioButtons(inputId = ns("hq"),
          label = "Select Habitat Quantity Definition",
          choiceNames = list(
            "Length", 
            tags$span(style = "color:#c0c0c0", "Area"), 
            tags$span(style = "color:#c0c0c0", "Volume")
          ),
          choiceValues = c("length", "area", "volume"),
          inline = TRUE, 
          selected = NULL
        )
      ),
      hr(),
      fluidRow(
        radioButtons(inputId = ns("obj"),
          label = "Objective",
          choiceNames = list(
            "Habitat Quantity", 
            tags$span(style = "color:#c0c0c0", "Weighted Attributes")
          ),
          choiceValues = c(1 : 2)
          ),
        conditionalPanel(
          condition = "input.obj == 2",
          ns = ns,
          column(2,  "HQ_LC1"),
          column(4,
                 numericInput(inputId = ns("w1"),
                              min = 0,
                              max = 1,
                              step = 0.01,
                              label = NULL,
                              value = 0.25)),
          column(2,  "HQ_LC2"),
          column(4,
                 numericInput(inputId = ns("w2"),
                              min = 0,
                              max = 1,
                              step = 0.01,
                              label = NULL,
                              value = 0.25)),
          column(2,  "HQ_LC3"),
          column(4,
                 numericInput(inputId = ns("w3"),
                              min = 0,
                              max = 1,
                              step = 0.01,
                              label = NULL,
                              value = 0.25)),
          column(2,  "Temp"),
          column(4,
                 numericInput(inputId = ns("w4"),
                              min = -1,
                              max = 1,
                              step = 0.01,
                              label = NULL,
                              value = 0.25))
        )
      ),
      # fluidRow(
      #   radioButtons(inputId = ns("obj"),
      #     label = "Objective",
      #     choices = list("Habitat Quantity" = 1, "Weighted Attributes" = 2,
      #       "Custom Barrier Scores" = 3)),
      #   conditionalPanel(
      #     condition = "input.obj == 2",
      #     ns = ns,
      #     column(2,  "Habitat Quantity"),
      #     column(4,
      #       numericInput(inputId = ns("w1"),
      #         min = 0,
      #         max = 1,
      #         step = 0.01,
      #         label = NULL,
      #         value = 0.5)),
      #     column(2, "Habitat Quality"),
      #     column(4,
      #       numericInput(inputId = ns("w2"),
      #       min = 0,
      #       max = 1,
      #       step = 0.01,
      #       label = NULL,
      #       value = 0.5))
      #   )
      # ),
      hr(),
      fluidRow(
        radioButtons(inputId = ns("cost"),
          label = "Cost",
          choiceNames = list(
            "Default Predictions", 
            tags$span(style = "color:#c0c0c0", "Provide Mean Project Cost")
          ),
          choiceValues = c(1, 2),
          inline = TRUE,
          selected = NULL)
      ),
      fluidRow(
        conditionalPanel(
          condition = "input.cost == 2",
          ns = ns,
          column(6,
          numericInput(inputId = ns("mean_design_cost"),
            label = tags$span(style = "color:#c0c0c0", "Mean Design Cost ($)"),
            min = 0,
            value = NULL)
          ),
          column(6,
          numericInput(inputId = ns("mean_construction_cost"),
            label = tags$span(style = "color:#c0c0c0", "Mean Construction Cost ($)"),
            min = 0,
            value = NULL)
          )
        )
      ),
      hr(),
      fluidRow(
        numericInput(inputId = ns("budget"),
                     label = "Enter Budget ($)",
                     min = 0,
                     value = NULL)
      ),
        # conditionalPanel(
        #   condition = "input.obj == 3 && input.cost == 1",
        #   ns = ns,
        #   fileInput(inputId = ns("cust_score"), 'Choose xlsx file with: IDs and Scores',
        #     accept = c(".xlsx")
        #   )
        # ),
        # conditionalPanel(
        #   condition = "input.obj != 3 && input.cost == 2",
        #   ns = ns,
        #   fileInput(inputId = ns("cust_cost"), 'Choose xlsx file with: IDs and Costs',
        #     accept = c(".xlsx")
        #   )
        # ),
        # conditionalPanel(
        #   condition = "input.obj == 3 && input.cost == 2",
        #   ns = ns,
        #   fileInput(inputId = ns("cust_score_cost"), 'Choose xlsx file with: IDs, Scores, and Costs',
        #             accept = c(".xlsx")
        #   )
        # )
      #),
      hr(),
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
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    #ensure weights add to one
    observeEvent(input$w1, {
      remaining <- 1 - input$w1
      updateNumericInput(session, "w2", max = remaining)
      updateNumericInput(session, "w3", max = remaining)
      updateNumericInput(session, "w4", value = 1 - input$w1 - input$w2 - input$w3)
    })
    
    observeEvent(input$w2, {
      remaining <- 1 - input$w1 - input$w2
      updateNumericInput(session, "w3", max = remaining)
      updateNumericInput(session, "w4", value = 1 - input$w1 - input$w2 - input$w3)
    })
    
    observeEvent(input$w3, {
      remaining <- 1 - input$w1 - input$w2 - input$w3
      updateNumericInput(session, "w4", value = 1 - input$w1 - input$w2 - input$w3)
    })
    
    
    # update reactive values object with Submit inputs
    observeEvent(input$area_sel, r$area_sel_suggest <- input$area_sel)
    observeEvent(input$obj, r$obj_suggest <- input$obj)
    observeEvent(input$budget, r$budget_suggest <- input$budget)
    observeEvent(input$owner_sel, {
      if("0" %in% input$owner_sel){
        r$owner_sel_suggest <- c(1:9, 11, 12)
      } else {
        r$owner_sel_suggest <- input$owner_sel
      }
    })
    
    # render leaflet output (DO I NEED THIS?)
    output$base_map <- leaflet::renderLeaflet({
      get_leaflet_map()
    })
    
    # tab events
    observeEvent(r$tab_sel, {
      if(r$tab_sel == "Welcome"){
        reset_map(leaflet::leafletProxy(ns("base_map")))
        #user_plot(FALSE)
      } else if(r$tab_sel == "Explore"){
        reset_map(leaflet::leafletProxy(ns("base_map")))
        #user_plot(FALSE)
      } else if(r$tab_sel == "Suggest"){
        reset_map(leaflet::leafletProxy(ns("base_map")))
        #user_plot(FALSE)
      }
      else if(r$tab_sel == "Custom"){
        reset_map(leaflet::leafletProxy(ns("base_map")))
        #user_plot(FALSE)
      }
      else if(r$tab_sel == "Learn"){
        reset_map(leaflet::leafletProxy(ns("base_map")))
        #user_plot(FALSE)
      }
    })
    
    # Suggest tab submit event
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

