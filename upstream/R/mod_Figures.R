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
      htmlOutput(ns("base_map")),
      align = "center"
      #)
    ),
    fluidRow(
      #shinydashboard::box(
      #width = 11,
      #solidHeader = TRUE, 
      uiOutput(ns("plot")),
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
    
    output$base_map <- renderUI({
      tags$iframe(src= "www/base_map.html",
        width = 450, height = 450,
        frameBorder = "0")
    })
    
    output$plot <- renderUI({
      if(!user_plot()){
        imageOutput(ns("logo")) 
        } else {
        renderPlot({store_plot()})
        }
      })
    
    })
}
