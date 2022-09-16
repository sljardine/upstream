#' Learn UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Learn_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Learn Server Functions
#'
#' @noRd 
mod_Learn_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    