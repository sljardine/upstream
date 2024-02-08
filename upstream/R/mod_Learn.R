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
        tags$li("Sunny Jardine (Project lead, Lead developer)"),
        tags$li("Robby Fonner (Project advisor)"),
        tags$li("Dan Holland (Project advisor)"),
        tags$li("Mark Scheuerell (Project advisor)"),
        tags$li("Braeden Van Deynze (Project advisor)"),
        tags$li("Connor Lewis-Smith (Developer, IT)"),
        tags$li("Logan Blair (Developer)"),
        tags$li("Andrew Cooke (Developer)"),
        tags$li("Jeffery Comnick (Developer)"),
        tags$li("J Kahn (Developer)"),
        tags$li("Luke Rogers (Developer)"),
        tags$li("Zephyr Pfotenhauer (Illustrator)")
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
      p(HTML("The Upstream <a href='https://sljardine.github.io/upstream_manual/' target='_blank'>User Guide</a> is a Work in Progress resource for navigating and utilizing the Upstream application."), align = "left", style = 'margin-left: 50px;')
    ),
    fluidRow(
      h3(HTML("<b>Upstream Video Tutorial</b>"), align = "left", style = 'margin-left: 50px;'),
      p("Coming Soon", align = "left", style = 'margin-left: 50px;')
    ),
    fluidRow(
      h3(HTML("<b>Found a Bug?</b>"), align = "left", style = 'margin-left: 50px;'),
      p(HTML("Reporting a bug or feature request can help improve Upstream. To do so, please visit <a href='https://github.com/sljardine/upstream/issues' target='_blank'>GitHub Issues page</a> to report the issue."), align = "left", style = 'margin-left: 50px;')
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
