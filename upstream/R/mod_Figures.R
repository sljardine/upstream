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
        style = 'float: right; width: calc(100% - 15.9ch); height: calc(50vh - 100px); min-height: 500px; border: thin solid grey; margin-right: 50px;',
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

    # initiates plot
    user_plot <- reactiveVal(FALSE)

    # tab events
    observeEvent(r$tab_sel, {
      reset_map(leaflet::leafletProxy(ns("base_map")))
      user_plot(FALSE)
    })

    # Explore submit button event for base_map
    observeEvent(r$submit_explore, {

      update_map_selected_polygons(
        leaflet::leafletProxy(ns('base_map')),
        r$area_sel_explore,
        r$subarea_sel_explore,
        r$area_choice_explore,
        r$subarea_choice_explore
        )
      update_map_culvert_markers(
        leaflet::leafletProxy(ns('base_map')),
        r$area_sel_explore,
        r$subarea_sel_explore,
        r$owner_sel_explore,
        r$remove_bad_match_explore,
        r$color_variable_explore,
        r$highlight_explore,
        r$barrier_ids_explore
        )
      user_plot(TRUE)

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

      update_map_selected_polygons(
        leaflet::leafletProxy(ns('base_map')),
        r$area_sel_suggest,
        r$subarea_sel_suggest,
        r$area_choice_suggest,
        r$subarea_choice_suggest
      )

      if(r$remove_bad_match_suggest){
        r$points_sel_suggest <- solve_opt(
          points = culverts_cmb_gm,
          budget = as.numeric(r$budget_suggest),
          D = D_gm,
          wria_sel = as.integer(r$area_sel_suggest),
          huc_sel = as.integer(r$subarea_sel_suggest),
          owner_sel = as.integer(r$owner_sel_suggest),
          obj = as.integer(r$obj_suggest),
          w_urb = as.numeric(r$w_urb_suggest),
          w_ag = as.numeric(r$w_ag_suggest),
          w_nat = as.numeric(r$w_nat_suggest),
          w_temp = as.numeric(r$w_temp_suggest),
          hq = as.integer(r$hq_suggest),
          species_sel = r$species_sel_suggest,
          barrier_idp = r$barrier_idp_suggest,
          cost = as.integer(r$cost_suggest),
          mean_design_cost = as.numeric(r$mean_design_cost_suggest),
          mean_construction_cost = as.numeric(r$mean_construction_cost_suggest)
        )
        reset_map(leaflet::leafletProxy(ns('base_map')))
        remove_map_points(leaflet::leafletProxy(ns('base_map')))
        map_leaflet_opt(
          leaf_proxy = leaflet::leafletProxy(ns('base_map')),
          points = culverts_cmb_gm, #culverts
          lines = lines_simp_gm, #lines with linestring geometries
          dslines = lines_ds_gm, #downstream lines with linestring geometries
          soln = r$points_sel_suggest, #output from solve_opt()
          marginal_line_ids = marginal_line_ids_gm, #comids for all lines marginally upstream of each point
          downstream_line_ids = downstream_line_ids_gm, #comids for all lines downstream of each point on main stem
          wria_sel = as.integer(r$area_sel_suggest),
          huc_sel = as.integer(r$subarea_sel_suggest),
          barrier_idp = r$barrier_idp_suggest
        )
        # selected wria bounding box
        bbox <- get_wria_bounding_box(r$area_sel_suggest)
        # zoom map to selected wrias
        leaflet::leafletProxy(mapId = ns("base_map")) %>%
        leaflet::flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      } else{
        r$points_sel_suggest <- solve_opt(
          points = culverts_cmb,
          budget = as.numeric(r$budget_suggest),
          D = D,
          wria_sel = as.integer(r$area_sel_suggest),
          huc_sel = as.integer(r$subarea_sel_suggest),
          owner_sel = as.integer(r$owner_sel_suggest),
          obj = as.integer(r$obj_suggest),
          w_urb = as.numeric(r$w_urb_suggest),
          w_ag = as.numeric(r$w_ag_suggest),
          w_nat = as.numeric(r$w_nat_suggest),
          w_temp = as.numeric(r$w_temp_suggest),
          hq = as.integer(r$hq_suggest),
          species_sel = r$species_sel_suggest,
          barrier_idp = r$barrier_idp_suggest,
          cost = as.integer(r$cost_suggest),
          mean_design_cost = as.numeric(r$mean_design_cost_suggest),
          mean_construction_cost = as.numeric(r$mean_construction_cost_suggest)
        )
        reset_map(leaflet::leafletProxy(ns('base_map')))
        remove_map_points(leaflet::leafletProxy(ns('base_map')))
        map_leaflet_opt(
          leaf_proxy = leaflet::leafletProxy(ns('base_map')),
          points = culverts_cmb, #culverts
          lines = lines_simp, #lines with linestring geometries
          dslines = lines_ds, #downstream lines with linestring geometries
          soln = r$points_sel_suggest, #output from solve_opt()
          marginal_line_ids = marginal_line_ids, #comids for all lines marginally upstream of each point
          downstream_line_ids = downstream_line_ids, #comids for all lines downstream of each point on main stem
          wria_sel = as.integer(r$area_sel_suggest),
          huc_sel = as.integer(r$subarea_sel_suggest),
          barrier_idp = r$barrier_idp_suggest
        )
        # selected wria bounding box
        bbox <- get_wria_bounding_box(r$area_sel_suggest)
        # zoom map to selected wrias
        leaflet::leafletProxy(mapId = ns("base_map")) %>%
          leaflet::flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      }
    })

    # Custom submit button event
    observeEvent(r$submit_custom, {
      update_map_selected_polygons(
        leaflet::leafletProxy(ns('base_map')),
        r$area_sel_custom,
        r$subarea_sel_custom,
        r$area_choice_custom,
        r$subarea_choice_custom
      )

      if(r$remove_bad_match_custom) {
        reset_map(leaflet::leafletProxy(ns('base_map')))
        remove_map_points(leaflet::leafletProxy(ns('base_map')))
        map_leaflet_custom(
          leaf_proxy = leaflet::leafletProxy(ns("base_map")),
          points = culverts_cmb_gm, #culverts
          lines = lines_simp_gm, #lines with linestring geometries
          dslines = lines_ds_gm, #downstream lines with linestring geometries
          prtf_cust = r$barrier_ids_custom, #inputs from mod_Custom
          E = E_gm, #full connectivity matrix
          marginal_line_ids = marginal_line_ids_gm, #comids for all lines marginally upstream of each point
          downstream_line_ids = downstream_line_ids_gm, #comids for all lines downstream of each point on main stem
          barrier_idp = r$barrier_idp_custom #planned barrier IDs
        )
        # selected wria bounding box
        bbox <- get_wria_bounding_box(r$area_sel_custom)
        # zoom map to selected wrias
        leaflet::leafletProxy(mapId = ns("base_map")) %>%
          leaflet::flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      } else {
        reset_map(leaflet::leafletProxy(ns('base_map')))
        remove_map_points(leaflet::leafletProxy(ns('base_map')))
        map_leaflet_custom(
          leaf_proxy = leaflet::leafletProxy(ns("base_map")),
          points = culverts_cmb, #culverts
          lines = lines_simp, #lines with linestring geometries
          dslines = lines_ds, #downstream lines with linestring geometries
          prtf_cust = r$barrier_ids_custom, #inputs from mod_Custom
          E = E, #full connectivity matrix
          marginal_line_ids = marginal_line_ids, #comids for all lines marginally upstream of each point
          downstream_line_ids = downstream_line_ids, #comids for all lines downstream of each point on main stem
          barrier_idp = r$barrier_idp_custom #planned barrier IDs
        )
        # selected wria bounding box
        bbox <- get_wria_bounding_box(r$area_sel_custom)
        # zoom map to selected wrias
        leaflet::leafletProxy(mapId = ns("base_map")) %>%
          leaflet::flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      }
    })

    # reset plot click text output
    observeEvent(r$submit_explore, {r$plot_click_text_output_explore <- ''})


    # render plot on submit button events or brush event
    store_plot <- eventReactive(
      c(user_plot(), r$submit_explore),
      {
        # explore tab plots (scatter plot and histogram)
        if(user_plot()){
          if(r$tab_sel == "Explore"){
            if(r$plot_type_explore == "Scatterplot"){
              if(r$remove_bad_match_explore){
                culverts_cmb_gm %>%
                  filter_and_format_culverts_for_scatterplot(
                    area_sel = r$area_sel_explore,
                    subarea_sel = r$subarea_sel_explore,
                    owner_sel = r$owner_sel_explore,
                    x_axis_variable = r$x_axis_variable_explore,
                    y_axis_variable = r$y_axis_variable_explore,
                    color_variable = r$color_variable_explore
                  ) %>%
                  figure_scatterplot(
                    r$x_axis_variable_explore, r$y_axis_variable_explore,
                    r$color_variable_explore, r$x_jitter_explore, r$y_jitter_explore,
                    r$highlight_explore, r$barrier_ids_explore, r$plot_xmin, r$plot_xmax,
                    r$plot_ymin, r$plot_ymax
                  )
              } else {
                culverts_cmb %>%
                  filter_and_format_culverts_for_scatterplot(
                    r$area_sel_explore, r$subarea_sel_explore, r$owner_sel_explore, r$x_axis_variable_explore,
                    r$y_axis_variable_explore, r$color_variable_explore
                  ) %>%
                  figure_scatterplot(
                    r$x_axis_variable_explore, r$y_axis_variable_explore,
                    r$color_variable_explore, r$x_jitter_explore, r$y_jitter_explore,
                    r$highlight_explore, r$barrier_ids_explore, r$plot_xmin, r$plot_xmax,
                    r$plot_ymin, r$plot_ymax
                  )
              }
            } else if(r$plot_type_explore == 'Histogram'){
              if(r$remove_bad_match_explore){
                culverts_cmb_gm %>%
                  filter_and_format_culverts_for_histogram(
                    r$area_sel_explore, r$subarea_sel_explore, r$owner_sel_explore, r$color_variable_explore,
                    r$histogram_variable_explore
                  ) %>%
                  figure_histogram(
                    r$x_axis_variable_explore, r$y_axis_variable_explore, r$color_variable_explore,
                    r$histogram_variable_explore, r$histogram_nbins_explore, r$highlight_explore,
                    r$barrier_ids_explore, r$plot_xmin, r$plot_xmax, r$plot_ymin, r$plot_ymax
                  )
              } else {
                culverts_cmb %>%
                  filter_and_format_culverts_for_histogram(
                    r$area_sel_explore, r$subarea_sel_explore, r$owner_sel_explore, r$color_variable_explore,
                    r$histogram_variable_explore
                  ) %>%
                  figure_histogram(
                    r$x_axis_variable_explore, r$y_axis_variable_explore, r$color_variable_explore,
                    r$histogram_variable_explore, r$histogram_nbins_explore, r$highlight_explore,
                    r$barrier_ids_explore, r$plot_xmin, r$plot_xmax, r$plot_ymin, r$plot_ymax
                  )
              }
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
        style = 'width: calc(100% - 15.9ch); height: calc(50vh - 100px); min-height: 500px; overflow: hidden;',
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
        } else if(r$x_axis_variable_explore %in% c("potential_species", "owner_type_code", "percent_fish_passable_code") | r$y_axis_variable_explore %in% c("potential_species", "owner_type_code", "percent_fish_passable_code")){
          r$plot_click_text_output_explore <- ''
        } else {
          r$plot_click_text_output_explore <- get_plot_click_site_id(
            r$owner_sel_explore, r$area_sel_explore, r$remove_bad_match_explore, r$x_axis_variable_explore,
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
