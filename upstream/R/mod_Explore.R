#' Explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

commonVariables <- setNames(
  c(
    "wria_number", "owner_type_code", "potential_species",
    "dn_count", "up_count", "cost",
    "hfull_length", "hfull_area", "hfull_volume",
    "hmarg_length", "hmarg_area", "hmarg_volume",
    "hfull_length_natural", "hfull_area_natural", "hfull_volume_natural",
    "hmarg_length_natural", "hmarg_area_natural", "hmarg_volume_natural",
    "hfull_length_agri", "hfull_area_agri", "hfull_volume_agri",
    "hmarg_length_agri", "hmarg_area_agri", "hmarg_volume_agri",
    "hfull_length_urb", "hfull_area_urb", "hfull_volume_urb",
    "hmarg_length_urb", "hmarg_area_urb", "hmarg_volume_urb",
    "hfull_length_TempVMM08", "hfull_area_TempVMM08", "hfull_volume_TempVMM08",
    "hmarg_length_TempVMM08", "hmarg_area_TempVMM08", "hmarg_volume_TempVMM08",
    "percent_fish_passable_code", "corrected_dn_wsdot", "corrected_dn_other"
  ),
  nm = c(
    "WRIA", "Owner Type", "Potential Species",
    "Count of Downstream Barriers", "Count of Upstream Barriers", "Cost",
    "Full Habitat (Length)", "Full Habitat (Area)", "Full Habitat (Volume)",
    "Marginal Habitat (Length)", "Marginal Habitat (Area)", "Marginal Habitat (Volume)",
    "Full Natural Habitat (Length)", "Full Natural Habitat (Area)", "Full Natural Habitat (Volume)",
    "Marginal Natural Habitat (Length)", "Marginal Natural Habitat (Area)", "Marginal Natural Habitat (Volume)",
    "Full Agricultural Habitat (Length)", "Full Agricultural Habitat (Area)", "Full Agricultural Habitat (Volume)",
    "Marginal Agricultural Habitat (Length)", "Marginal Agricultural Habitat (Area)", "Marginal Agricultural Habitat (Volume)",
    "Full Urban Habitat (Length)", "Full Urban Habitat (Area)", "Full Urban Habitat (Volume)",
    "Marginal Urban Habitat (Length)", "Marginal Urban Habitat (Area)", "Marginal Urban Habitat (Volume)",
    "Full Temp (Length)", "Full Temp (Area)", "Full Temp (Volume)",
    "Marginal Temp (Length)", "Marginal Temp (Area)", "Marginal Temp (Volume)",
    "Passability", "WSDOT Downstream Corrections", "non-WSDOT Downstream Corrections"
  )
)

length_vars <- setNames(
  c(
    "wria_number", "owner_type_code", "potential_species",
    "dn_count", "up_count", "cost",
    "hfull_length", "hmarg_length", "hfull_length_natural",
    "hmarg_length_natural",
    "hfull_length_agri",
    "hmarg_length_agri",
    "hfull_length_urb",
    "hmarg_length_urb",
    "hfull_length_TempVMM08",
    "hmarg_length_TempVMM08",
    "percent_fish_passable_code", "corrected_dn_wsdot", "corrected_dn_other"
  ),
  nm = c(
    "WRIA", "Owner Type", "Potential Species",
    "Count of Downstream Barriers", "Count of Upstream Barriers", "Cost",
    "Full Habitat (Length)",
    "Marginal Habitat (Length)",
    "Full Natural Habitat (Length)",
    "Marginal Natural Habitat (Length)",
    "Full Agricultural Habitat (Length)",
    "Marginal Agricultural Habitat (Length)",
    "Full Urban Habitat (Length)",
    "Marginal Urban Habitat (Length)",
    "Full Temp (Length)",
    "Marginal Temp (Length)",
    "Passability", "WSDOT Downstream Corrections", "non-WSDOT Downstream Corrections"
  )
)

volume_vars <- setNames(
  c(
    "wria_number", "owner_type_code", "potential_species",
    "dn_count", "up_count", "cost",
    "hfull_volume", "hmarg_volume", "hfull_volume_natural",
    "hmarg_volume_natural", "hfull_volume_agri",
    "hmarg_volume_agri", "hfull_volume_urb", "hmarg_volume_urb",
    "hfull_volume_TempVMM08", "hmarg_volume_TempVMM08",
    "percent_fish_passable_code", "corrected_dn_wsdot", "corrected_dn_other"
  ),
  nm = c(
    "WRIA", "Owner Type", "Potential Species",
    "Count of Downstream Barriers", "Count of Upstream Barriers", "Cost",
    "Full Habitat (Volume)", "Marginal Habitat (Volume)", "Full Natural Habitat (Volume)",
    "Marginal Natural Habitat (Volume)", "Full Agricultural Habitat (Volume)", "Marginal Agricultural Habitat (Volume)",
    "Full Urban Habitat (Volume)", "Marginal Urban Habitat (Volume)", "Full Temp (Volume)", "Marginal Temp (Volume)",
    "Passability", "WSDOT Downstream Corrections", "non-WSDOT Downstream Corrections"
  )
)

area_vars <- setNames(
  c(
    "wria_number", "owner_type_code", "potential_species",
    "dn_count", "up_count", "cost",
    "hfull_area", "hmarg_area", "hfull_area_natural", "hmarg_area_natural",
    "hfull_area_agri", "hmarg_area_agri", "hfull_area_urb", "hmarg_area_urb",  "hfull_area_TempVMM08",
    "hmarg_area_TempVMM08",
    "percent_fish_passable_code", "corrected_dn_wsdot", "corrected_dn_other"
  ),
  nm = c(
    "WRIA", "Owner Type", "Potential Species",
    "Count of Downstream Barriers", "Count of Upstream Barriers", "Cost",
    "Full Habitat (Area)", "Marginal Habitat (Area)", "Full Natural Habitat (Area)", "Marginal Natural Habitat (Area)",  "Full Agricultural Habitat (Area)", "Marginal Agricultural Habitat (Area)", "Full Urban Habitat (Area)", "Marginal Urban Habitat (Area)","Full Temp (Area)",  "Marginal Temp (Area)",
    "Passability", "WSDOT Downstream Corrections", "non-WSDOT Downstream Corrections"
  )
)

# UI ----
mod_Explore_ui <- function(id){
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
            nm = c("All Ownership Types","City", "County", "Federal", "Private",
                   "State", "Tribal", "Other", "Port", "Drainage District", "Irrigation District", "Unknown")
          ),
          selected = 0,
          width = '50%',
          multiple = TRUE
        )
      ),
      hr(),
      fluidRow(
        radioButtons(inputId = ns("remove_bad_match"),
                     label = "Remove Bad Culvert Matches",
                     choices = list("No" = 1, "Yes" = 2), selected = 2,
                     width = "100%", inline = TRUE)
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
                     selected = "Length"
        )
      ),
      hr(),
      fluidRow(
        selectInput(
          ns("plot_type"),
          label = "Select Plot Type",
          choices = list("Scatterplot", "Histogram"),
          selected = "Scatterplot"
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Scatterplot'",
        ns = ns,
        fluidRow(
          column(
            6,
            selectizeInput(
              inputId = ns("x_axis_variable"),
              label = "Variable on X axis",
              # TODO - Pass as argument or add to r reactive function?
              choices = commonVariables,
              selected = "cost",
              width = "100%"),
            offset = 0
          ),
          column(
            6,
            selectizeInput(
              inputId = ns("y_axis_variable"),
              label = "Variable on Y axis",
              choices = commonVariables,
              selected = "hfull_length",
              width = "100%"
            ),
            offset = 0
          )
        ),
        fluidRow(
          column(
            6,
            sliderInput(
              ns("x_jitter"), "X Variable Jitter",
              value = 0, max = .4, min = 0, step = .01,
              width = "100%", ticks = FALSE
            ),
            offset = 0
          ),
          column(
            6,
            sliderInput(
              ns("y_jitter"), "Y Variable Jitter",
              value = 0, max = .4, min = 0, step = .01,
              width = "100%", ticks = FALSE
            ),
            offset = 0
          )
        )
      ),
      conditionalPanel(
        "input.plot_type == 'Histogram'",
        ns = ns,
        selectInput(
          inputId = ns("histogram_variable"),
          label = "Variable to display",
          # TODO - Pass as argument or add to r reactive function?
          choices = commonVariables
        ),
        # number of bins for numerical vars
        conditionalPanel(
          "input.histogram_variable != 'potential_species'",
          ns = ns,
          sliderInput(
            inputId = ns("histogram_nbins"),
            label = "Number of bins",
            min = 1, max = 100, value = 30,
            ticks = FALSE
          )
        )
      ),
      hr(),
      fluidRow(
        selectInput(
          inputId = ns("color_variable"),
          label = "Select Variable for Map and Plot Fill Color",
          choices = commonVariables
        )
      ),
      fluidRow(
        column(12,
               radioButtons(inputId = ns("highlight"),
                            label = "Highlight Barrier(s)",
                            choices = list("No" = 1, "Yes" = 2),
                            width = "100%"),
               offset = 0
        )
      ),
      fluidRow(
        conditionalPanel(
          condition = "input.highlight == 2",
          ns = ns,
          fluidRow(
            column(12,
                   selectizeInput(
                     inputId = ns("barrier_ids"),
                     label = "Enter ID(s) to Highlight",
                     multiple = TRUE,
                     choices = NULL,
                     width = "100%"
                   )
            )
          )
        )
      ),
      hr(),
      fluidRow(
        checkboxInput(ns("rezoom_on_submit"), "Reset Map Zoom on Submit", value = TRUE)
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

# scatter plot variable choices
cScatterPlotVariables <- commonVariables

# histogram choices
cHistogramVariables <- commonVariables

# color choices
cColorVariables <- commonVariables

    # Explore tab submit event
    observeEvent(input$submit, {
      if(!is.null(input$owner_sel) && !is.null(input$area_sel) && !is.null(input$subarea_sel) &&
         input$highlight == 1)
      {r$submit_explore <- input$submit}
      else
        if(!is.null(input$owner_sel) && !is.null(input$area_sel) && !is.null(input$subarea_sel) &&
           input$highlight == 2)
        {r$submit_explore <- input$submit}
      else
      {showModal(modalDialog(title = "Warning!",
                             "Please fill all the fields before you click the Submit button."))}
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

    # update x variable based on hq
    observeEvent(input$hq, {
      if(input$hq == 3) {
        filteredNames <- volume_vars
      } else if(input$hq == 2) {
        filteredNames <- area_vars
      } else {
        filteredNames <- length_vars
      }

      # Update the x-axis variable choices
      updateSelectizeInput(session, "x_axis_variable", choices = filteredNames)
    })

    # update y variable based on hq
    observeEvent(input$hq, {
      if(input$hq == 3) {
        filteredNames <- volume_vars
      } else if(input$hq == 2) {
        filteredNames <- area_vars
      } else {
        filteredNames <- length_vars
      }

      # Update the x-axis variable choices
      updateSelectizeInput(session, "y_axis_variable", choices = filteredNames)
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
      if("0" %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::pull(site_id))}
      if("1" %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_city) %>% dplyr::pull(site_id))}
      if("2" %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_county) %>% dplyr::pull(site_id))}
      if("3" %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_federal) %>% dplyr::pull(site_id))}
      if("4" %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_private) %>% dplyr::pull(site_id))}
      if("5" %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_state) %>% dplyr::pull(site_id))}
      if("6" %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_tribal) %>% dplyr::pull(site_id))}
      if("7" %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_other) %>% dplyr::pull(site_id))}
      if("8" %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_port) %>% dplyr::pull(site_id))}
      if("9" %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_drainage_district) %>% dplyr::pull(site_id))}
      if("11" %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_irrigation_district) %>% dplyr::pull(site_id))}
      if("12" %in% input$owner_sel){cSiteIds <- c(cSiteIds, sfC %>% dplyr::filter(is_unknown) %>% dplyr::pull(site_id))}

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

    # update color variables for plot type
    observeEvent(c(input$plot_type, input$area_sel, input$owner_sel, input$hq), {
      if(input$plot_type == "Scatterplot") {
        if(!is.null(input$hq) && input$hq != "") {
          if(input$hq == 3) {  # Assuming 3 corresponds to 'Volume'
            cVars <- volume_vars
          } else if(input$hq == 2) {  # Assuming 2 corresponds to 'Area'
            cVars <- area_vars
          } else {  # Default case, assuming any other value including 1 corresponds to 'Length'
            cVars <- length_vars
          }
        } else {
          cVars <- commonVariables
        }
      } else {
        cVars <- setNames(
          c("none", "dn_count", "up_count", "wria_number", "owner_type_code"),
          nm = c("None", "Count of Downstream Barriers", "Count of Upstream Barriers", "WRIA", "Ownership Type")
        )
      }

      if(!is.null(input$area_sel)){
        if(length(input$area_sel) < 2 & !("0" %in% input$area_sel)){
          cVars <- cVars[cVars != "wria_number"]
        }
      }

      if(!is.null(input$owner_sel)){
        if(length(input$owner_sel) < 2 & !("0" %in% input$owner_sel)){
          cVars <- cVars[cVars != "owner_type_code"]
        }
      }

      updateSelectInput(inputId = "color_variable", choices = cVars)
    })

    # update reactive values object with Explore inputs
    ##area_sel and area_choice
    observeEvent(input$area_sel, {
      if("0" %in% input$area_sel){
        r$area_sel_explore <- wrias %>% dplyr::pull(WRIA_NR)
        r$area_choice_explore <- "all"
      } else {
        r$area_sel_explore <- input$area_sel
        r$area_choice_explore <- "selection"
      }
    })

    ##subarea_sel and subarea_choice
    observeEvent(c(input$area_sel, input$subarea_sel), {

      # get areas to filter by
      if("0" %in% input$area_sel){
        cWRIA_NR <- wrias %>% dplyr::pull(WRIA_NR)
      } else {
        cWRIA_NR <- as.integer(input$area_sel)
      }

      if("0" %in% input$subarea_sel){
        r$subarea_sel_explore <- huc12_wrias %>% dplyr::filter(wria_number %in% cWRIA_NR) %>% dplyr::pull(huc_number)
        r$subarea_choice_explore <- "all"
      } else {
        r$subarea_sel_explore <- input$subarea_sel
        r$subarea_choice_explore <- "selection"
      }
    })

    observeEvent(input$owner_sel, {
      if("0" %in% input$owner_sel){
        r$owner_sel_explore <- c(1:9, 11, 12)
      } else {
        r$owner_sel_explore <- input$owner_sel
      }
    })

    observeEvent(input$remove_bad_match, {
      if(input$remove_bad_match == 1){
        r$remove_bad_match_explore <- FALSE
      } else {
        r$remove_bad_match_explore <- TRUE
      }
    })

    observeEvent(input$plot_type, r$plot_type_explore <- input$plot_type)
    observeEvent(input$x_axis_variable, r$x_axis_variable_explore <- input$x_axis_variable)
    observeEvent(input$y_axis_variable, r$y_axis_variable_explore <- input$y_axis_variable)
    observeEvent(input$x_jitter, r$x_jitter_explore <- input$x_jitter)
    observeEvent(input$y_jitter, r$y_jitter_explore <- input$y_jitter)
    observeEvent(input$histogram_variable, r$histogram_variable_explore <- input$histogram_variable)
    observeEvent(input$histogram_nbins, r$histogram_nbins_explore <- input$histogram_nbins)
    observeEvent(input$color_variable, r$color_variable_explore <- input$color_variable)
    observeEvent(input$highlight, r$highlight_explore <- input$highlight)
    observeEvent(input$barrier_ids, r$barrier_ids_explore <- input$barrier_ids, ignoreNULL = FALSE)
    observeEvent(input$rezoom_on_submit, r$rezoom_on_submit_explore <- input$rezoom_on_submit)

    # reset figures tab plot extent and triggers redraw
    observeEvent(input$submit, {
      r$plot_xmin <- NA
      r$plot_xmax <- NA
      r$plot_ymin <- NA
      r$plot_ymax <- NA
      r$plot_brush <- r$plot_brush + 1
    })

    # reset figures tab plot extent
    observeEvent(c(
      input$area_sel,
      input$owner_sel,
      input$plot_type,
      input$x_axis_variable,
      input$y_axis_variable,
      input$histogram_variable
    ), {
      r$plot_xmin <- NA
      r$plot_xmax <- NA
      r$plot_ymin <- NA
      r$plot_ymax <- NA
    })
  })
}
