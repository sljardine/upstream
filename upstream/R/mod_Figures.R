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
        uiOutput(ns("plot"))
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
    
    user_plot <- reactiveVal(FALSE)
    
    observeEvent(c(r$submit_explore,
      r$submit_suggest,
      r$submit_custom),
      user_plot(TRUE))
      
    store_plot <- eventReactive(c(r$submit_explore,
      r$submit_suggest,
      r$submit_custom),
      {shinipsum::random_ggplot(type = "point")
      })

    output$logo <- renderImage({
      list(src = "inst/app/www/placeholder.png",
        width = "75%",
        align = "center")
    }, deleteFile = FALSE)
    
    output$plot <- renderUI({
      if(!user_plot()){
        imageOutput(ns("logo")) 
        } else {
        renderPlot({store_plot()})
        }
      })
    
    })
}
