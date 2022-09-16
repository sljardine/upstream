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
      #shinydashboard::box(
        #width = 6,
        #solidHeader = TRUE, 
        #DT::dataTableOutput(ns('data_table'))
      uiOutput(ns("data_table"))
      #)
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
    
    observeEvent(c(r$submit_suggest,
      r$submit_custom),
      user_table(TRUE))

    store_table <- eventReactive(c(r$submit_suggest,
      r$submit_custom),
      {shinipsum::random_DT(5, 3, "numeric")}
      )

    output$logo <- renderImage({
      list(src = "inst/app/www/placeholder.png",
           width = "75%",
           align = "center")
    }, deleteFile = FALSE)
    
    output$data_table <- renderUI({
      if(!user_table()){
        imageOutput(ns("logo")) 
      } else {
        output$render_data_table <- DT::renderDataTable({store_table()})
        DT::dataTableOutput(ns("render_data_table"))}
    })
    
  })
}
