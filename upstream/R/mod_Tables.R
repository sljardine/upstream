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
    # fluidRow(
    #   uiOutput(ns("data_table"))
    # )
    fluidRow(
      tags$div(
        style = 'float: right; width: calc(100% - 15.9ch); height: calc(50vh - 100px); min-height: 500px; margin-right: 50px;',
        uiOutput(ns("data_table")),
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
          get_summary_table(culverts_cmb, r$points_sel_suggest)
        }
        else if(r$tab_sel == "Custom") {
          points_sel_custom <- get_points_sel_custom(culverts_cmb, r$barrier_ids1_custom)
          get_summary_table(culverts_cmb, points_sel_custom)
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

    output$logo <- renderImage({
      list(src = "inst/app/www/placeholder.png",
           width = "75%",
           align = "center")
    }, deleteFile = FALSE)

    output$data_table <- renderUI({
      if(!user_table()){
        imageOutput(NULL)
      } else {
        output$render_data_table <- DT::renderDataTable({store_table()})
        DT::dataTableOutput(ns("render_data_table"))}
    })

  })
}
