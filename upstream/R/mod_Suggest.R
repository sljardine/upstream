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
      style = "height: calc(100vh - 145px); overflow-y: auto;",
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
          label = "Select Species of Interest",
          choices = setNames(
            c("all", "chinook", "chum", "coho", "pink", "sockeye", "steelhead"),
          nm = c("All", "Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead")
          ),
          selected = "all",
          width = '50%',
          multiple = TRUE
        )
      ),
      hr(),
      fluidRow(
        selectizeInput(
          inputId = ns("barrier_idp"),
          label = "Select Projects to Ignore",
          selected = 0,
          multiple = TRUE,
          choices = NULL,
          width = "50%")
      ),
      hr(),
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
          column(4,  "Urban habitat quantity"),
          column(4,  "Agricultural habitat quantity"),
          column(4,  "Natural habitat quantity"),
          column(4,
                 numericInput(inputId = ns("w_urb"),
                              min = 0,
                              max = 1,
                              step = 0.01,
                              label = NULL,
                              value = 0.33)),
          
          column(4,
                 numericInput(inputId = ns("w_ag"),
                              min = 0,
                              max = 1,
                              #step = 0.01,
                              label = NULL,
                              value = 0.33)),
          
          column(4,
                 numericInput(inputId = ns("w_nat"),
                              min = 0,
                              max = 1,
                              #step = 0.01,
                              label = NULL,
                              value = 0.34)),
          tagAppendAttributes(
            style = "margin-left: 15px;",
            sliderInput(
              inputId = ns("w_temp"), 
              label = "Select Temperature Range", 
              min = 9, max = 22, step = .1, 
              value = c(9, 22), ticks = FALSE,
              width = '80%'
            )
          )
        )
      ),
      hr(),
      fluidRow(
        radioButtons(inputId = ns("cost"),
          label = "Cost",
          choiceNames = list(
            "Default Predictions",
            "Provide Mean Project Cost"
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
            label = "Mean Design & Permitting Cost ($)",
            min = 0,
            value = NULL)
          ),
          column(6,
          numericInput(inputId = ns("mean_construction_cost"),
            label = "Mean Construction Cost ($)",
            min = 0,
            value = NULL)
          )
        )
      ),
      hr(),
      fluidRow(
        radioButtons(inputId = ns("remove_bad_match"),
                     label = "Remove Bad Culvert Matches",
                     choices = list("No" = 1, "Yes" = 2), selected = 2,
                     width = "100%", inline = TRUE)
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

    # update huc options, filtering HUCs in selected WRIA ----
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

    # update reactive values object with Suggest inputs ----
    ##budget ----
    observeEvent(input$budget, r$budget_suggest <- input$budget)
    ##area_sel and area_choice ----
    observeEvent(input$area_sel, {
      if("0" %in% input$area_sel){
        r$area_sel_suggest <- wrias %>% dplyr::pull(WRIA_NR)
        r$area_choice_suggest <- "all"
      } else {
        r$area_sel_suggest <- input$area_sel
        r$area_choice_suggest <- "selection"
      }
    }
    )
    ##subarea_sel and subarea_choice ----
    observeEvent(c(input$area_sel, input$subarea_sel), {
      # get areas to filter by
      if("0" %in% input$area_sel){
        cWRIA_NR <- wrias %>% dplyr::pull(WRIA_NR)
      } else {
        cWRIA_NR <- as.integer(input$area_sel)
      }

      if("0" %in% input$subarea_sel){
        r$subarea_sel_suggest <- huc12_wrias %>% dplyr::filter(wria_number %in% cWRIA_NR) %>% dplyr::pull(huc_number)
        r$subarea_choice_suggest <- "all"
        } else {
        r$subarea_sel_suggest<- input$subarea_sel
        r$subarea_choice_suggest <- "selection"
      }
    }
    )
    ##owner_sel ----
    observeEvent(input$owner_sel, {
      if("0" %in% input$owner_sel){
        r$owner_sel_suggest <- c(1:9, 11, 12)
      } else {
        r$owner_sel_suggest <- input$owner_sel
      }
    })
    ##projects to ignore ----
    observeEvent(input$barrier_idp, r$barrier_idp_suggest <- input$barrier_idp)
    observeEvent(c(input$remove_bad_match), {
      
      sfC <- culverts_cmb %>% sf::st_drop_geometry()
      
      if(input$remove_bad_match == 2){
        sfC <- culverts_cmb_gm %>% sf::st_drop_geometry()
      }
      
      updateSelectizeInput(
        session,
        inputId = "barrier_idp",
        choices = setNames(
          c(0, sfC %>% dplyr::pull(site_id) %>% sort()),
          nm = c('None', sfC %>% dplyr::pull(site_id) %>% sort())
        ),
        selected = 0,
        server = TRUE
      )
    }
    )
    ##species_sel ----
    observeEvent(input$species_sel, r$species_sel_suggest <- input$species_sel)
    ##hq (habitat quality definition) ----
    observeEvent(input$hq, r$hq_suggest <- input$hq)
    ##obj (objective function) ----
    observeEvent(input$obj, r$obj_suggest <- input$obj)
    ##weights (habitat quality) ----
    observeEvent(input$w_urb, r$w_urb_suggest <- input$w_urb)
    observeEvent(input$w_ag, r$w_ag_suggest <- input$w_ag)
    observeEvent(input$w_nat, r$w_nat_suggest <- input$w_nat)
    observeEvent(input$w_temp, r$w_temp_suggest <- input$w_temp)
    ##cost (cost definition) ----
    observeEvent(input$cost, r$cost_suggest <- input$cost)
    observeEvent(input$mean_design_cost, r$mean_design_cost_suggest <- input$mean_design_cost)
    observeEvent(input$mean_construction_cost, r$mean_construction_cost_suggest <- input$mean_construction_cost)
    ##bad culvert matches ----
    observeEvent(input$remove_bad_match, {
      if(input$remove_bad_match == 1){
        r$remove_bad_match_suggest <- FALSE
      } else {
        r$remove_bad_match_suggest <- TRUE
      }
    })

    # render leaflet output 
    output$base_map <- leaflet::renderLeaflet({
      get_leaflet_map()
    })

    # tab events ----
    observeEvent(r$tab_sel, {
      if(r$tab_sel == "Welcome"){
        reset_map(leaflet::leafletProxy(ns("base_map")))
      } else if(r$tab_sel == "Explore"){
        reset_map(leaflet::leafletProxy(ns("base_map")))
      } else if(r$tab_sel == "Suggest"){
        reset_map(leaflet::leafletProxy(ns("base_map")))
      }
      else if(r$tab_sel == "Custom"){
        reset_map(leaflet::leafletProxy(ns("base_map")))
      }
      else if(r$tab_sel == "Learn"){
        reset_map(leaflet::leafletProxy(ns("base_map")))
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
          !is.na(input$budget) && input$obj == 2 && input$cost != 2 && sum(input$w_urb, input$w_ag, input$w_nat) == 1)
      {r$submit_suggest <- input$submit}
      else
      #selected an owner, selected an area, selected a subarea, entered a budget, maximizes quantity, custom costs with mean entered
      if(!is.null(input$owner_sel) && !is.null(input$area_sel) && !is.null(input$subarea_sel) &&
         !is.na(input$budget) && input$obj == 1 &&
        input$cost == 2 && !is.na(input$mean_design_cost) && !is.na(input$mean_construction_cost))
      {r$submit_suggest <- input$submit}
      else
      #selected an owner, selected an area, selected a subarea, entered a budget, maximizes weighted attributes with weights summing to one, custom costs with mean entered
      if(!is.null(input$owner_sel) && !is.null(input$area_sel) && !is.null(input$subarea_sel) &&
         !is.na(input$budget) && input$obj == 2 && sum(input$w_urb, input$w_ag, input$w_nat) == 1  &&
         input$cost == 2 && !is.na(input$mean_design_cost)  && !is.na(input$mean_construction_cost))
      {r$submit_suggest <- input$submit}
      else
      {showModal(modalDialog(title = "Warning!",
      "Please fill all the fields, and ensure weights sum to one (if applicable), before you click the Submit button."))}
    })

  })
}

