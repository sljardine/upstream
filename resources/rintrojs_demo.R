library(shiny)
library(rintrojs)

ui <- fluidPage(
  introjsUI(),
  column(2,
         br(),
         actionButton("help", "About this Page")
  ),
  column(2,
         introBox(
           selectInput("Task", label = "Select Task", 
          choices =  c("Please select","Upload","Analyze Data")),
           data.step = 1,
          data.intro = "This is the selectInput called Task, you do xyz with this"
         )
  ),
  column(2,
         introBox(
           selectInput(
             "breaks", "Breaks",
             c("Sturges",
               "Scott",
               "Freedman-Diaconis",
               "[Custom]" = "custom")),
           data.step = 2, 
           data.intro = "This is the selectInput called breaks, you do xyz with this"
         )
  ),
  column(2,
         introBox(
           sliderInput("breakCount", "Break Count", min=1, max=1000, value=10),
           data.step = 3, 
           data.intro = "This is the sliderInput called breakCount, you do xyz with this"
         )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  observeEvent(input$help,
               introjs(session, 
  options = list("showBullets" = "false", "showProgress" = "true", 
  "showStepNumbers" = "false", "nextLabel" = "Next", 
  "prevLabel" = "Prev", "skipLabel" = "Skip"))
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)