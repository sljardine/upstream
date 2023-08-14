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
    fluidPage(
      fluidRow(
      h3(HTML("<b>Welcome to Upstream</b>"), align = "center"),
      p("Upstream is a tool to inform data-based barrier culvert restoration planning
        in Western Washington.", align = "center")
      ),
      fluidRow(
        column(6,
            tags$button(id = ns("explore_tab"),
              class = "btn action-button",
              style = "background-color:transparent",
              tags$img(src = "www/explore_final.png",
                width = "100%")
         )
        ),
        column(6,
          tags$button(id = ns("suggest_tab"),
            class = "btn action-button",
            style = "background-color:transparent",
            tags$img(src = "www/suggest_final.png",
              width = "100%")
          )
         )
        ),
      fluidRow(
        column(6,
          tags$button(id = ns("custom_tab"),
            class = "btn action-button",
            style = "background-color:transparent",
            tags$img(src = "www/custom_final.png",
              width = "100%")
         )
        ),
        column(6,
          tags$button(id = ns("learn_tab"),
            class = "btn action-button",
            style = "background-color:transparent",
            tags$img(src = "www/learn_final.png",
            width = "100%")
     )
    )
   )
  )
 )

}

#' Welcome Server Functions
#'
#' @noRd
mod_Welcome_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$explore_tab, {
      r$tab_sel <- "Explore"
    })

    observeEvent(input$suggest_tab, {
      r$tab_sel <- "Suggest"
    })

    observeEvent(input$custom_tab, {
      r$tab_sel <- "Custom"
    })

    observeEvent(input$learn_tab, {
      r$tab_sel <- "Learn"
    })

  })
}
