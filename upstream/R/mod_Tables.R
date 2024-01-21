#' Tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Tables_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      tags$div(
        style = 'float: right; width: calc(100% - 15.9ch); height: calc(50vh - 100px); min-height: 400px; margin-right: 50px;',
        uiOutput(ns("data_table")),
        align = "center"
      )
    ),
    br(),
    br(),
    fluidRow(
      tags$div(
        style = 'float: right; width: calc(100% - 15.9ch); height: calc(50vh - 100px); min-height: 500px; margin-right: 50px;',
        uiOutput(ns("list")),
        align = "center"
      )
    )
  )
}

#' Tables Server Functions
#'
#' @noRd
mod_Tables_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    user_table <- reactiveVal(FALSE)

    observeEvent(
      c(r$submit_suggest, r$submit_custom),
      user_table(TRUE)
      )

    store_table <- eventReactive(c(r$submit_suggest, r$submit_custom),
      {
        if(r$tab_sel == "Suggest"){
          if(r$remove_bad_match_suggest){
            get_summary_table(
              points = culverts_cmb_gm, 
              points_sel = r$points_sel_suggest, 
              barrier_idp = r$barrier_idp_suggest, 
              hq = r$hq_suggest)
          } else {
            get_summary_table(
              points = culverts_cmb, 
              points_sel = r$points_sel_suggest, 
              barrier_idp = r$barrier_idp_suggest, 
              hq = r$hq_suggest)
          }
        }
        else if(r$tab_sel == "Custom") {
          if(r$remove_bad_match_custom){
            points_sel_custom <- get_points_sel_custom(
              points = culverts_cmb_gm, 
              prtf_cust = r$barrier_ids_custom
              )
            get_summary_table(
              points = culverts_cmb_gm, 
              points_sel = points_sel_custom, 
              barrier_idp = 0
              )
          } else {
            points_sel_custom <- get_points_sel_custom(
              points = culverts_cmb, 
              prtf_cust = r$barrier_ids_custom
              )
            get_summary_table(
              points = culverts_cmb, 
              points_sel = points_sel_custom, 
              barrier_idp = 0
              )
          }
        }
      }
    )

    store_list <- eventReactive(c(r$submit_suggest, r$submit_custom),
      {
        if(r$tab_sel == "Suggest"){
          if(r$remove_bad_match_suggest){
            get_plan_list(
              points = culverts_cmb_gm, 
              points_sel = r$points_sel_suggest, 
              barrier_idp = r$barrier_idp_suggest,
              cost = r$cost_suggest,
              mean_design_cost = r$mean_design_cost_suggest,
              mean_construction_cost = r$mean_construction_cost_suggest)
          } else {
            get_plan_list(
              points = culverts_cmb, 
              points_sel = r$points_sel_suggest, 
              barrier_idp = r$barrier_idp_suggest,
              cost = r$cost_suggest,
              mean_design_cost = r$mean_design_cost_suggest,
              mean_construction_cost = r$mean_construction_cost_suggest)
          }
       }
        else if(r$tab_sel == "Custom") {
          if(r$remove_bad_match_custom){ 
            points_sel_custom <- get_points_sel_custom(
              points = culverts_cmb_gm, 
              prtf_cust = r$barrier_ids_custom
            )
            get_plan_list(
              points = culverts_cmb_gm, 
              points_sel = points_sel_custom, 
              barrier_idp = 0
            )
            } else {
          points_sel_custom <- get_points_sel_custom(
            points = culverts_cmb, 
            prtf_cust = r$barrier_ids_custom
            )
          get_plan_list(
            points = culverts_cmb, 
            points_sel = points_sel_custom, 
            barrier_idp = 0
            )
            }
      }
    }
  )

    # tab events
    observeEvent(r$tab_sel, {
      if(r$tab_sel == "Welcome"){
        user_table(FALSE)
      } else if(r$tab_sel == "Explore"){
          user_table(FALSE)
      } else if(r$tab_sel == "Suggest"){
          user_table(FALSE)
      } else if(r$tab_sel == "Custom"){
        user_table(FALSE)
      } else if(r$tab_sel == "Learn"){
        user_table(FALSE)
      }
    })

    output$data_table <- renderUI({
      if(!user_table()){
        imageOutput(NULL)
      } else {
        output$render_data_table <- DT::renderDataTable(
          DT::datatable({store_table()},
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;','Estimated Habitat Gains'),
          options = list(pageLength = 7, lengthChange = FALSE, searching = FALSE)
          )
          )
        DT::dataTableOutput(ns("render_data_table"))}
    })

    output$list <- renderUI({
      if(!user_table()){
        imageOutput(NULL)
      } else {
        output$render_list <- DT::renderDataTable(
          DT::datatable(
            {store_list()},
            caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;','Selected Culverts'),
            options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE),
            rownames = FALSE
            )
          )
        DT::dataTableOutput(ns("render_list"))}
    })

  })
}
