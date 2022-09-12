#' Welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Welcome_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Welcome Server Functions
#'
#' @noRd 
mod_Welcome_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Welcome_ui("Welcome_1")
    
## To be copied in the server
# mod_Welcome_server("Welcome_1")
