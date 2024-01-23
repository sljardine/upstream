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
            nm = c(
              'All Ownership Types','City', 'County', 'Federal', 'Private',
              'State', 'Tribal', 'Other', 'Port', 'Drainage District',
              'Irrigation District', 'Unknown')
          ),
          selected = 0,
          width = '50%',
          multiple = TRUE
        )
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
          selectizeInput(
          inputId = ns("barrier_ids"),
          label = "Enter ID(s) for Your Custom Plan",
          multiple = TRUE,
          choices = NULL,
          width = "100%")
         ),
      hr(),
      fluidRow(
        radioButtons(inputId = ns("remove_bad_match"),
                     label = "Remove Bad Culvert Matches",
                     choices = list("No" = 1, "Yes" = 2), selected = 2,
                     width = "100%", inline = TRUE)
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

    # render leaflet output
    output$base_map <- leaflet::renderLeaflet({
      get_leaflet_map()
    })

    # tab events
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


    # update huc options
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

    # update barrier ids to filter to wria, huc12, and owner
    observeEvent(c(input$area_sel, input$subarea_sel, input$owner_sel, input$remove_bad_match), {

      sfC <- culverts_cmb %>% sf::st_drop_geometry()

      # filter bad culvert matches
      if(input$remove_bad_match == 2){
        sfC <- culverts_cmb_gm %>% sf::st_drop_geometry()
      }

      # get areas to filter by
      if("0" %in% input$area_sel){
        cWRIA_NR <- wrias %>% dplyr::pull(WRIA_NR)
      } else {
        cWRIA_NR <- as.integer(input$area_sel)
      }

      # get subareas to filter by
      if("0" %in% input$subarea_sel){
        choice_huc_number <- huc12_wrias %>%
          dplyr::filter(wria_number %in% cWRIA_NR) %>%
          dplyr::pull(huc_number)
      } else {
        choice_huc_number <- as.integer(input$subarea_sel)
      }

      # get site ids for owner types to filter by
      cSiteIds <- c()
      if('0' %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::pull(site_id))}
      if('1' %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_city) %>% dplyr::pull(site_id))}
      if('2' %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_county) %>% dplyr::pull(site_id))}
      if('3' %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_federal) %>% dplyr::pull(site_id))}
      if('4' %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_private) %>% dplyr::pull(site_id))}
      if('5' %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_state) %>% dplyr::pull(site_id))}
      if('6' %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_tribal) %>% dplyr::pull(site_id))}
      if('7' %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_other) %>% dplyr::pull(site_id))}
      if('8' %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_port) %>% dplyr::pull(site_id))}
      if('9' %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_drainage_district) %>% dplyr::pull(site_id))}
      if('11' %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_irrigation_district) %>% dplyr::pull(site_id))}
      if('12' %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_unknown) %>% dplyr::pull(site_id))}

      # filter sites
      sfC <- sfC %>% dplyr::filter(wria_number %in% cWRIA_NR & huc_number %in% choice_huc_number & site_id %in% cSiteIds)

      # update select input choices
      updateSelectizeInput(
        session,
        inputId = "barrier_ids",
        choices = sfC %>% dplyr::pull(site_id) %>% sort(),
        server = TRUE
      )

    })

    # update reactive values object with Custom inputs
    ##area_sel and area_choice ----
    observeEvent(input$area_sel, {
      if("0" %in% input$area_sel){
        r$area_sel_custom <- wrias %>% dplyr::pull(WRIA_NR)
        r$area_choice_custom <- "all"
      } else {
        r$area_sel_custom <- input$area_sel
        r$area_choice_custom <- "selection"
      }
    })

    ##subarea_sel and subarea_choice ----
    observeEvent(c(input$area_sel, input$subarea_sel), {
      # get areas to filter by
      if("0" %in% input$area_sel){
        cWRIA_NR <- wrias %>% dplyr::pull(WRIA_NR)
      } else {
        cWRIA_NR <- as.integer(input$area_sel)
      }

      if("0" %in% input$subarea_sel){
        r$subarea_sel_custom <- huc12_wrias %>%
          dplyr::filter(wria_number %in% cWRIA_NR) %>%
          dplyr::pull(huc_number)
        r$subarea_choice_custom <- "all"
      } else {
        r$subarea_sel_custom <- input$subarea_sel
        r$subarea_choice_custom <- "selection"
      }
    })

    ##owner_sel ----
    observeEvent(input$owner_sel, {
      if("0" %in% input$owner_sel){
        r$owner_sel_custom <- c(1:9, 11, 12)
      } else {
        r$owner_sel_custom <- input$owner_sel
      }
    })

    ##projects to ignore ----
    observeEvent(input$barrier_idp, r$barrier_idp_custom <- input$barrier_idp)
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
          nm = c('None',
                 sfC %>% dplyr::pull(site_id) %>% sort())
        ),
        selected = 0,
        server = TRUE
      )
    }
    )

    ##selected culvs ----
    observeEvent(input$barrier_ids, r$barrier_ids_custom <- input$barrier_ids, ignoreNULL = FALSE)

    ##convert remove bad culvert matches to logical----
    observeEvent(input$remove_bad_match, {
      if(input$remove_bad_match == 1){
        r$remove_bad_match_custom <- FALSE
      } else {
        r$remove_bad_match_custom <- TRUE
      }
    })

    ##cost (cost definition) ----
    observeEvent(input$cost, r$cost_custom <- input$cost)
    observeEvent(input$mean_design_cost, r$mean_design_cost_custom <- input$mean_design_cost)
    observeEvent(input$mean_construction_cost, r$mean_construction_cost_custom <- input$mean_construction_cost)

    ##hq (habitat quality definition) ----
    observeEvent(input$hq, r$hq_custom <- input$hq)

    # Custom tab submit event
    observeEvent(input$submit, {
      # Check all conditions together
      if(!is.null(input$barrier_ids) &&
         ((input$cost != 2) ||
          (input$cost == 2 && !is.na(input$mean_design_cost) && !is.na(input$mean_construction_cost)))) {
        r$submit_custom <- input$submit
      } else {
        # Show warning if conditions are not met
        showModal(
          modalDialog(
            title = "Warning!",
            "Please fill all the fields correctly before you click the Submit button."
          )
        )
      }
    })

  })
}

