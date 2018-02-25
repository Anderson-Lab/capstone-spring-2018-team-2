library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "capstone"),
  
  dashboardSidebar(sidebarMenuOutput("sideMenu")
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "investigation", class = "active",
      # Boxes need to be put in a row (or column)
        fluidRow(
          box(plotOutput("plot1", height = 350)),
          
          box(sliderInput("imp", "Select importance level: ", 0, 100, 50, step = 10))
          
        )
      )
    )
  )
)

server <- function(input, output) {
  load("data/ranger_imp.rda") # var.importance data generated ranger function in Build_Ranger.R
  load("data/meta.rda")
  output$sideMenu <- renderMenu({ sidebarMenu(
      menuItem("Investigation", 
               tabName = "investigation", 
               icon = icon("dashboard"),
               textInput("var_name", "Check Variable Description", "EVALCOVR"),
               verbatimTextOutput("var_description"))
      )
  });
  output$plot1 <- renderPlot({
    barplot(imp[imp > input$imp], horiz=TRUE, las= 1, xlab = "Importance Level", main = "Variable Importance Plot")
  })
  output$var_description <- renderText({ meta_named_char[input$var_name] })
}

shinyApp(ui, server)