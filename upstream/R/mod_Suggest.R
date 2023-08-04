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
          inputId = ns("subarea_sel"),
          label = "Select Subarea",
          choices = NULL,
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
            "Area",
            "Volume"
          ),
          choiceValues = c(1 : 3),
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
            "Weighted Attributes"
          ),
          choiceValues = c(1 : 2)
          ),
        conditionalPanel(
          condition = "input.obj == 2",
          ns = ns,
          column(12,  "Select weights for each habitat quality attribute. Weights must sum to one.",
            style = 'padding-bottom:10px;'),
          column(3,  "Urban habitat quantity"),
          column(3,
                 numericInput(inputId = ns("w_urb"),
                              min = 0,
                              max = 1,
                              step = 0.01,
                              label = NULL,
                              value = 0.33)),
          column(3,  "Agricultural habitat quantity"),
          column(3,
                 numericInput(inputId = ns("w_ag"),
                              min = 0,
                              max = 1,
                              #step = 0.01,
                              label = NULL,
                              value = 0.33)),
          column(3,  "Natural habitat quantity"),
          column(3,
                 numericInput(inputId = ns("w_nat"),
                              min = 0,
                              max = 1,
                              #step = 0.01,
                              label = NULL,
                              value = 0.34)),
          column(3,  "Ideal habitat temperature"),
          column(3,
                 numericInput(inputId = ns("w_temp"),
                              min = -1,
                              max = 1,
                              #step = 0.01,
                              label = NULL,
                              value = 0))
        )
      ),
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



    # update huc options ----
    observeEvent(c(input$area_sel), {

      # get areas to filter by
      if("0" %in% input$area_sel){
        cWRIA_NR <- wrias %>% dplyr::pull(WRIA_NR)
      } else {
        cWRIA_NR <- as.integer(input$area_sel)
      }

      # filter hucs
      options_hucs <- huc12_wrias %>%
        dplyr::filter(wria_number %in% cWRIA_NR)

      # update select input choices
      updateSelectizeInput(
        session,
        inputId = "subarea_sel",
        choices = setNames(
          c(0, options_hucs %>% dplyr::pull(huc_number)),
          nm = c("All HUC 12s in selected WRIA(s)", options_hucs %>% dplyr::pull(huc_name))
        ),
        selected = 0,
        server = TRUE
      )
    })

    # update reactive values object with Submit inputs ----
    ##area_sel ----
    observeEvent(input$area_sel, r$area_sel_suggest <- input$area_sel)
    ##subarea_sel ----
    observeEvent(c(input$area_sel, input$subarea_sel), {

      # get areas to filter by
      if("0" %in% input$area_sel){
        cWRIA_NR <- wrias %>% dplyr::pull(WRIA_NR)
      } else {
        cWRIA_NR <- as.integer(input$area_sel)
      }

      if("0" %in% input$subarea_sel){
        r$subarea_sel_suggest <- huc12_wrias %>% dplyr::filter(wria_number %in% cWRIA_NR) %>% dplyr::pull(huc_number)
      } else {
        r$subarea_sel_suggest<- input$subarea_sel
      }
    })
    ##owner_sel ----
    observeEvent(input$owner_sel, {
      if("0" %in% input$owner_sel){
        r$owner_sel_suggest <- c(1:9, 11, 12)
      } else {
        r$owner_sel_suggest <- input$owner_sel
      }
    })
    ##obj (objective function) ----
    observeEvent(input$obj, r$obj_suggest <- input$obj)
    ##habitat quality definition ----
    observeEvent(input$hq, r$hq_suggest <- input$hq)
    ##weights ----
    observeEvent(input$w_urb, r$w_urb_suggest <- input$w_urb)
    observeEvent(input$w_ag, r$w_ag_suggest <- input$w_ag)
    observeEvent(input$w_nat, r$w_nat_suggest <- input$w_nat)
    observeEvent(input$w_temp, r$w_temp_suggest <- input$w_temp)
    ##budget ----
    observeEvent(input$budget, r$budget_suggest <- input$budget)


    # render leaflet output (DO I NEED THIS?) ----
    output$base_map <- leaflet::renderLeaflet({
      get_leaflet_map()
    })


    # tab events ----
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


    # Suggest tab submit event ----
    observeEvent(input$submit, {
      #selected an owner, selected an area, selected a subarea, entered a budget, maximizes quantity, uses default costs
      if(!is.null(input$owner_sel) && !is.null(input$area_sel) && !is.null(input$subarea_sel) &&
         !is.na(input$budget) && input$obj == 1 && input$cost != 2)
      {r$submit_suggest <- input$submit}
      else
      #selected an owner, selected an area, selected a subarea, entered a budget, maximizes weighted attributes with weights summing to one, uses default costs
      if(!is.null(input$owner_sel) && !is.null(input$area_sel) && !is.null(input$subarea_sel) &&
          !is.na(input$budget) && input$obj == 2 && input$cost != 2 && sum(input$w_urb, input$w_ag, input$w_nat, input$w_temp) == 1)
      {r$submit_suggest <- input$submit}
      else
      #selected an owner, selected an area, selected a subarea, entered a budget, maximizes quantity, custom costs with mean entered
      if(!is.null(input$owner_sel) && !is.null(input$area_sel) && !is.null(input$subarea_sel) &&
         !is.na(input$budget) && input$obj == 1 &&
        input$cost == 2 && !is.null(input$cust_cost))
      {r$submit_suggest <- input$submit}
      else
      #selected an owner, selected an area, selected a subarea, entered a budget, maximizes weighted attributes with weights summing to one, custom costs with mean entered
      if(!is.null(input$owner_sel) && !is.null(input$area_sel) && !is.null(input$subarea_sel) &&
         !is.na(input$budget) && input$obj == 2 && sum(input$w_urb, input$w_ag, input$w_nat, input$w_temp) == 1  && input$cost == 2 && !is.null(input$cust_cost))
      {r$submit_suggest <- input$submit}
      else
      {showModal(modalDialog(title = "Warning!",
      "Please fill all the fields, and ensure weights sum to one (if applicable), before you click the Submit button."))}
    })


  })
}

