#' Figures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Figures_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #shinydashboard::box(
      #width = 11,
      #solidHeader = TRUE,
      #htmlOutput(ns("base_map")),
      tags$div(
        style = 'float: right; width: calc(100% - 15.9ch); height: calc(50vh - 100px); min-height: 500px; border: thin solid grey; margin-right: 5px;',
        leaflet::leafletOutput(ns('base_map'), height = '100%'),
        align = "center"
      )
      #)
    ),
    fluidRow(
      #shinydashboard::box(
      #width = 11,
      #solidHeader = TRUE,
      uiOutput(ns("uiPlot")),
      align = "center"
      #)
    )
  )
}



#' Figures Server Functions
#'
#' @noRd
mod_Figures_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # incrementing variable to trigger plot redraw on brush event
    r$plot_brush <- 0

    # plot click barrier id
    r$plot_click_text_output <- ''

    # render leaflet output
    output$base_map <- leaflet::renderLeaflet({
      get_leaflet_map()
    })

    # map zoom event
    observeEvent(input$base_map_zoom, {
      update_map_WRIA_labels(leaflet::leafletProxy(ns('base_map')), input$base_map_zoom, r)
    })

    # explore submit button event for base_map
    observeEvent(r$submit_explore, {
      update_map_selected_WRIA_polygons(leaflet::leafletProxy(ns('base_map')), r)
      update_map_WRIA_labels(leaflet::leafletProxy(ns('base_map')), input$base_map_zoom, r)
      update_map_culvert_markers(leaflet::leafletProxy(ns('base_map')), r)
    })

    # fly to selected wrias on Explore submit
    observeEvent(r$submit_explore, {
      if(r$rezoom_on_submit){
        # selected wria bounding box
        bbox <- get_wria_bounding_box(r$area_sel)

        # zoom map to selected wrias
        leaflet::leafletProxy(mapId = ns("base_map")) %>%
          leaflet::flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      }
    })

    # reset plot click text output
    observeEvent(r$submit_explore, {r$plot_click_text_output <- ''})

    # initiates plot
    user_plot <- reactiveVal(FALSE)
    observeEvent(c(r$submit_explore, r$submit_suggest, r$submit_custom), user_plot(TRUE))

    # render plot on submit button events or brush event
    store_plot <- eventReactive(c(r$submit_explore, r$submit_suggest, r$submit_custom, r$plot_brush), {
      # default plot for app is culvert count by WRIA
      #if(!user_plot()){
      #  #getInitialExploreTabHistogram(sfCulverts, sfWRIA)
      #}
      # explore tab plots (scatter plot and histogram)
      if(user_plot()){
        if(r$tab_sel == "Explore"){
          if(r$plot_type == 'Scatterplot'){
            sfCulverts %>%
              filter_and_format_culverts_for_explore_tab_scatterplot(r) %>%
              figure_explore_tab_scatterplot(r)
          } else if(r$plot_type == 'Histogram'){
            sfCulverts %>%
              filter_and_format_culverts_for_histogram(r) %>%
              figure_explore_tab_histogram(r)
          }
        }
      }
    })

    output$logo <- renderImage({
      list(src = "inst/app/www/placeholder.png",
           width = "75%",
           align = "center")
    }, deleteFile = FALSE)

    # render plot ui
    output$uiPlot <- renderUI({
      tags$div(
        style = 'width: 100%; height: calc(50vh - 100px); min-height: 500px; overflow: hidden;',
        plotOutput(
          ns('plot'),
          brush = brushOpts(ns('plot_brush'), resetOnNew = TRUE, clip = FALSE),
          click = ns('plot_click'), dblclick = ns('plot_dblclick'),
          height = "100%"
        ),
        tagAppendAttributes(
          textOutput(ns('plot_click_text_output')),
          style = 'position: relative; bottom: 99%; right: 10px; text-align: right; font-size: 16px; font-weight: bold;'
        )

      )
    })

    # render plot
    output$plot <- renderPlot({store_plot()})

    # makes figures tab plot zoomable
    observeEvent(input$plot_brush, {
      r$plot_xmin <- input$plot_brush$xmin
      r$plot_xmax <- input$plot_brush$xmax
      r$plot_ymin <- input$plot_brush$ymin
      r$plot_ymax <- input$plot_brush$ymax
      r$plot_brush <- r$plot_brush + 1
    })

    # plot click event to identify culvert
    observeEvent(input$plot_click, {
      if(r$plot_type == 'histogram'){
        r$plot_click_text_output <- ''
      } else if(r$x_axis_variable %in% c('potential_species', 'owner_type_code') |
         r$y_axis_variable %in% c('potential_species', 'owner_type_code')){
        r$plot_click_text_output <- ''
      } else {
        get_plot_click_site_id(r, input$plot_click$x, input$plot_click$y)
      }
    })

    # render plot click text output
    output$plot_click_text_output <- renderText(r$plot_click_text_output)
  })
}
