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
          # TODO - Check - I will join by WRIA_NR to sfCulverts but should we use WRIA_ID instead (WRIA_ID is not in sfCulverts)?
          # TODO - Pass as argument or add to r reactive function?
          choices = setNames(
            sfWRIA %>% dplyr::pull(WRIA_NR),
            nm = sfWRIA %>% dplyr::pull(WRIA_NM) %>% sort()
          ),
          selected = NULL,
          width = '50%',
          multiple = TRUE
        )
      ),
      fluidRow(
        selectizeInput(
          inputId = ns("owner_sel"),
          label = "Select Ownership Group",
          # TODO - Check - I looked up owner names for owner_type_code from https://geodataservices.wdfw.wa.gov/hp/fishpassage/index.html
          # TODO - Pass as argument or add to r reactive function?
          choices = setNames(
            sfCulverts %>% dplyr::distinct(owner_type_code) %>% dplyr::pull(owner_type_code) %>% sort(),
            nm = c('City', 'County', 'Federal', 'Private', 'State', 'Tribal', 'Other', 'Port', 'Drainage District', 'Unknown')
          ),
          selected = NULL,
          width = '50%',
          multiple = TRUE
        )
      ),
      fluidRow(
        selectInput(ns("plot_type"),
                    label = "Select Plot Type",
                    choices = list("Scatterplot", "Histogram"),
                    selected = "Scatterplot"
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Scatterplot'",
        ns = ns,
        fluidRow(
          column(6,
                 selectizeInput(
                   inputId = ns("x_axis_variable"),
                   label = "Variable on X axis",
                   # TODO - Pass as argument or add to r reactive function?
                   choices = setNames(
                     c('cost', 'barrier_count', 'potential_species', 'hmarg', 'hfull'),
                     nm = c('Cost', 'Downstream Barriers', 'Potential Species', 'Marginal Habitat', 'Full Habitat')
                   ),
                   selected = "cost",
                   width = "100%"),
                 offset = 0
          ),
          column(6,
                 selectizeInput(
                   inputId = ns("y_axis_variable"),
                   label = "Variable on Y axis",
                   # TODO - Pass as argument or add to r reactive function?
                   choices = setNames(
                     c('cost', 'barrier_count', 'potential_species', 'hmarg', 'hfull'),
                     nm = c('Cost', 'Downstream Barriers', 'Potential Species', 'Marginal Habitat', 'Full Habitat')
                   ),
                   selected = "hfull",
                   width = "100%"
                 ),
                 offset = 0
          )
        ),
        fluidRow(
          column(6,
                 sliderInput(
                   ns("x_jitter"), "X Variable Jitter",
                   value = 0, max = .4, min = 0, step = .01,
                   width = "100%", ticks = FALSE
                 ),
                 offset = 0
          ),
          column(6,
                 sliderInput(
                   ns("y_jitter"), "Y Variable Jitter",
                   value = 0, max = .4, min = 0, step = .01,
                   width = "100%", ticks = FALSE
                 ),
                 offset = 0
          )
        ),
        fluidRow(
          column(12,
                 radioButtons(inputId = ns("highlight"),
                              label = "Highlight Barrier(s)",
                              choices = list("No" = 1, "Yes" = 2),
                              width = '100%'),
                 offset = 0
          )
        ),
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
      conditionalPanel("input.plot_type == 'Histogram'",
                       ns = ns,
                       selectInput(
                         inputId = ns("histogram_variable"),
                         label = "Variable to display",
                         # TODO - Pass as argument or add to r reactive function?
                         choices = setNames(
                           c('cost', 'potential_species', 'barrier_count', 'hmarg', 'hfull'),
                           nm = c('Cost', 'Potential Species', 'Downstream Barriers', 'Marginal Habitat', 'Full Habitat')
                         )
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
    cScatterPlotVariables <- setNames(
      c('cost', 'barrier_count', 'potential_species', 'hmarg', 'hfull'),
      nm = c('Cost', 'Downstream Barriers', 'Potential Species', 'Marginal Habitat', 'Full Habitat')
    )

    # Explore tab submit event
    observeEvent(input$submit, {
      if(!is.null(input$owner_sel) && !is.null(input$area_sel) &&
         input$highlight == 1)
      {r$submit_explore <- input$submit}
      else
        if(!is.null(input$owner_sel) && !is.null(input$area_sel) &&
           input$highlight == 2)
        {r$submit_explore <- input$submit}
      else
      {showModal(modalDialog(title = "Warning!",
                             "Please fill all the fields before you click the Submit button."))}
    })

    # update y variable select input to not allow selected x variable
    observeEvent(input$x_axis_variable, {
      updateSelectInput(
        session,
        inputId = 'y_axis_variable',
        choices = cScatterPlotVariables[cScatterPlotVariables != input$x_axis_variable],
        selected = input$y_axis_variable
      )
    })

    # update x variable select input to not allow selected y variable
    observeEvent(input$y_axis_variable, {
      updateSelectInput(
        session,
        inputId = 'x_axis_variable',
        choices = cScatterPlotVariables[cScatterPlotVariables != input$y_axis_variable],
        selected = input$x_axis_variable
      )
    })

    # update barrier ids to filter to wria and owner
    observeEvent(c(input$area_sel, input$owner_sel), {
      updateSelectizeInput(
        session,
        inputId = 'barrier_ids',
        choices = sfCulverts %>%
          dplyr::filter(wria_number %in% input$area_sel & owner_type_code %in% input$owner_sel) %>%
          dplyr::pull(site_id) %>%
          sort()
      )
    })

    # update reactive values object with Explore inputs
    observeEvent(input$area_sel, r$area_sel <- input$area_sel)
    observeEvent(input$owner_sel, r$owner_sel <- input$owner_sel)
    observeEvent(input$plot_type, r$plot_type <- input$plot_type)
    observeEvent(input$x_axis_variable, r$x_axis_variable <- input$x_axis_variable)
    observeEvent(input$y_axis_variable, r$y_axis_variable <- input$y_axis_variable)
    observeEvent(input$x_jitter, r$x_jitter <- input$x_jitter)
    observeEvent(input$y_jitter, r$y_jitter <- input$y_jitter)
    observeEvent(input$histogram_variable, r$histogram_variable <- input$histogram_variable)
    observeEvent(input$histogram_nbins, r$histogram_nbins <- input$histogram_nbins)
    observeEvent(input$highlight, r$highlight <- input$highlight)
    observeEvent(input$barrier_ids, r$barrier_ids <- input$barrier_ids)

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
