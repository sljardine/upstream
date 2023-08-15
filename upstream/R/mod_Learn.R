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
    fluidRow(
      h3(HTML("<b>Project Team</b>"), align = "left", style = 'margin-left: 50px;'),
      tags$div(tags$ul(
        tags$li("Sunny Jardine (lead PI)"),
        tags$li("Robby Fonner (PI)"),
        tags$li("Dan Holland (PI)"),
        tags$li("Mark Scheuerell (PI)"),
        tags$li("Braeden Van Deynze (PI)"),
        tags$li("Logan Blair (postdoc)")
        ),
        style = 'margin-left: 50px;'
        )
    ),
    fluidRow(
      h3(HTML("<b>Funding</b>"), align = "left", style = 'margin-left: 50px;'),
      p("Funding for Upstream comes from the Washington Sea Grant", align = "left", style = 'margin-left: 50px;')
    ),
    fluidRow(
      h3(HTML("<b>Upstream User Guide</b>"), align = "left", style = 'margin-left: 50px;'),
      p("Coming Soon", align = "left", style = 'margin-left: 50px;')
    ),
    fluidRow(
      h3(HTML("<b>Upstream Video Tutorial</b>"), align = "left", style = 'margin-left: 50px;'),
      p("Coming Soon", align = "left", style = 'margin-left: 50px;')
    )
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
