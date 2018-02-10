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
    tabItems(
      # First tab content
      tabItem(tabName = "investigation",
      # Boxes need to be put in a row (or column)
        fluidRow(
          box(plotOutput("plot1", height = 350)),
          
          box(sliderInput("imp", "Select importance level: ", 0, 100, 50, step = 10))
        )
      ),
      tabItem(tabName = "models",
        h2('Models will go here..')
      )
    )
  )
)

server <- function(input, output) {
  load("data/ranger_imp.rda") # var.importance data generated ranger function in Build_Ranger.R
  
  output$plot1 <- renderPlot({
    barplot(imp[imp > input$imp], horiz=TRUE, las= 1, xlab = "Importance Level", main = "Variable Importance Plot")
  })
}

shinyApp(ui, server)