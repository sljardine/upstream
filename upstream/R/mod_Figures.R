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
      tags$div(
        style = 'float: right; width: calc(100% - 15.9ch); height: calc(50vh - 100px); min-height: 500px; border: thin solid grey; margin-right: 5px;',
        leafgl::leafglOutput(ns('base_map'), height = '100%'),
        align = "center"
      )
    ),
    fluidRow(
      uiOutput(ns("uiPlot")),
      align = "center"
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
    
    # tab events
    observeEvent(r$tab_sel, {
      if(r$tab_sel == 'Welcome'){
        reset_map(leaflet::leafletProxy(ns('base_map')))
        user_plot(FALSE)
      } else if(r$tab_sel == 'Explore'){
        reset_map(leaflet::leafletProxy(ns('base_map')))
        user_plot(FALSE)
      } else if(r$tab_sel == 'Suggest'){
        reset_map(leaflet::leafletProxy(ns('base_map')))
        user_plot(FALSE)
      }
      else if(r$tab_sel == 'Custom'){
        reset_map(leaflet::leafletProxy(ns('base_map')))
        user_plot(FALSE)
      }
    })

    # Explore submit button event for base_map
    observeEvent(r$submit_explore, {
      update_map_selected_WRIA_polygons(
        leaflet::leafletProxy(ns('base_map')), 
        r$area_sel_explore
        )
      update_map_culvert_markers(
        leaflet::leafletProxy(ns('base_map')), 
        r$area_sel_explore, 
        r$owner_sel_explore, 
        r$color_variable_explore, 
        r$highlight_explore, 
        r$barrier_ids_explore
        )
    })

    # fly to selected wrias on Explore submit
    observeEvent(r$submit_explore, {
      if(r$rezoom_on_submit_explore){
        # selected wria bounding box
        bbox <- get_wria_bounding_box(r$area_sel_explore)

        # zoom map to selected wrias
        leaflet::leafletProxy(mapId = ns("base_map")) %>%
        leaflet::flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      }
    })
    
    # Suggest submit button event 
    observeEvent(r$submit_suggest, {
      soln <- solve_opt(
        culverts_cmb, 
        as.numeric(r$budget_suggest), 
        D, 
        as.integer(r$area_sel_suggest)
        )
      remove_map_points(leaflet::leafletProxy(ns('base_map')))
      map_leaflet_opt(
        leaflet::leafletProxy(ns('base_map')),
        culverts_cmb, #culverts
        lines_simp, #lines with linestring geometries 
        soln, #output from solve_opt()
        marginal_line_ids #comids for all lines marginally upstream of each point
      )
    })

    # Custom submit button event 
    observeEvent(r$submit_custom, {
      remove_map_points(leaflet::leafletProxy(ns('base_map')))
      map_leaflet_custom(
        leaflet::leafletProxy(ns('base_map')),
        culverts_cmb, #culverts
        lines_simp, #lines with linestring geometries 
        r$barrier_ids1_custom, #inputs from mod_Custom
        E, #full connectivity matrix
        marginal_line_ids #comids for all lines marginally upstream of each point
      )
    })
    
    # reset plot click text output
    observeEvent(r$submit_explore, {r$plot_click_text_output_explore <- ''})

    # initiates plot
    user_plot <- reactiveVal(FALSE)
    observeEvent(c(r$submit_explore, r$submit_suggest, r$submit_custom), user_plot(TRUE))

    # render plot on submit button events or brush event
    store_plot <- eventReactive(
      c(r$tab_sel, r$submit_explore, r$submit_suggest, r$submit_custom, r$plot_brush), 
      {
      # explore tab plots (scatter plot and histogram)
      if(user_plot()){
        if(r$tab_sel == "Explore"){
          if(r$plot_type_explore == 'Scatterplot'){
            culverts_cmb %>%
              filter_and_format_culverts_for_scatterplot(
                r$area_sel_explore, r$owner_sel_explore, r$x_axis_variable_explore, 
                r$y_axis_variable_explore, r$color_variable_explore) %>%
              figure_scatterplot(r$x_axis_variable_explore, r$y_axis_variable_explore, 
                r$color_variable_explore, r$x_jitter_explore, r$y_jitter_explore, 
                r$highlight_explore, r$barrier_ids_explore, r$plot_xmin, r$plot_xmax, 
                r$plot_ymin, r$plot_ymax
                )
          } else if(r$plot_type_explore == 'Histogram'){
            culverts_cmb %>%
              filter_and_format_culverts_for_histogram(
                r$area_sel_explore, r$owner_sel_explore, r$color_variable_explore, 
                r$histogram_variable_explore
                ) %>%
              figure_histogram(
                r$x_axis_variable_explore, r$y_axis_variable_explore, r$color_variable_explore, 
                r$histogram_variable_explore, r$histogram_nbins_explore, r$highlight_explore, 
                r$barrier_ids_explore, r$plot_xmin, r$plot_xmax, r$plot_ymin, r$plot_ymax
                )
          }
        }
      } else {
        NULL
      }
    }
    )
    
    # logo (not being used)
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
      if(r$tab_sel == 'Explore'){
        if(r$plot_type_explore == 'histogram'){
          r$plot_click_text_output <- ''
        } else if(r$x_axis_variable_explore %in% c('potential_species', 'owner_type_code') | r$y_axis_variable_explore %in% c('potential_species', 'owner_type_code')){
          r$plot_click_text_output_explore <- ''
        } else {
          r$plot_click_text_output_explore <- get_plot_click_site_id(
            r$owner_sel_explore, r$area_sel_explore, r$x_axis_variable_explore, 
            r$y_axis_variable_explore, input$plot_click$x, input$plot_click$y
            )
        }
      }
    })

    # render plot click text output
    output$plot_click_text_output <- renderText({
      if(r$tab_sel == 'Explore'){
        r$plot_click_text_output_explore
      }
    })
  })
}
