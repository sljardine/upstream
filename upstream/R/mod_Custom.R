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
        selectizeInput(
          inputId = ns("barrier_idp"),
          label = "Already Planned / Will Complete",
          selected = 0,
          multiple = TRUE,
          choices = NULL,
          width = "50%")
      ),
      hr(),
        fluidRow(
          selectizeInput(
          inputId = ns("barrier_ids"),
          label = "Enter ID(s) for your custom plan",
          multiple = TRUE,
          choices = NULL,
          width = "100%")
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


    # # update input choices for barrier IDs
    # updateSelectizeInput(
    #   session,
    #   inputId = "barrier_ids",
    #   choices = culverts_cmb %>% sf::st_drop_geometry() %>% dplyr::pull(site_id) %>% sort(),
    #   server = TRUE
    # )


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
    observeEvent(c(input$area_sel, input$subarea_sel, input$owner_sel), {

      sfC <- culverts_cmb %>% sf::st_drop_geometry()

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
        r$subarea_sel_custom <- huc12_wrias %>% dplyr::filter(wria_number %in% cWRIA_NR) %>% dplyr::pull(huc_number)
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
    ##planned culvs ----
    observeEvent(input$barrier_idp, r$barrier_idp_custom <- input$barrier_idp)
    updateSelectizeInput(
      session,
      inputId = "barrier_idp",
      choices = setNames(
        c(0,culverts_cmb %>% sf::st_drop_geometry() %>% dplyr::pull(site_id) %>% sort()),
        nm = c('None',
               culverts_cmb %>% sf::st_drop_geometry() %>% dplyr::pull(site_id) %>% sort())
      ),
      selected = 0,
      server = TRUE
    )
    ##selected culvs ----
    observeEvent(input$barrier_ids, r$barrier_ids_custom <- input$barrier_ids, ignoreNULL = FALSE)

    # Custom tab submit event
    observeEvent(input$submit, {
      if(!is.null(input$barrier_ids))
      {r$submit_custom <- input$submit}
      else
      {showModal(modalDialog(title = "Warning!",
                             "Please enter at least one set of barrier IDs before you click the Submit button."))}
    })

  })
}

