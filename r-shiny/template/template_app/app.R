library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "capstone"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Investigation", tabName = "investigation", icon = icon("dashboard")),
      menuItem("Models", tabName = "models", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)