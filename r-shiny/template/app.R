library(shiny)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(dplyr)

variables = c(1:24)

ui <- dashboardPage(
  dashboardHeader(title = "capstone"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Investigation", 
               tabName = "investigation", 
               icon = icon("dashboard")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "investigation", class = "active",
      # Boxes need to be put in a row (or column)
        box(
          
          selectInput('num_vars', 'Show number of variables:', variables, selected = variables[10], selectize=TRUE),
          verbatimTextOutput("var_descriptions")
        ), 
      
        fluidRow(
          box(
            tabsetPanel(type = "tabs",
                        tabPanel("Gini", plotOutput("gini", height = 350)),
                        tabPanel("Cutoff", plotOutput("cutoff", height = 350)),
                        tabPanel("ROC", htmlOutput("roc"), height = 500)
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  load("data/ranger_imp.rda") # var.importance data generated ranger function in Build_Ranger.R
  load("data/meta.rda")
  load
  
  output$var_descriptions <- renderPrint(meta_named_char[data()[ ,1]])
  
  data = reactive({
    arrange(imp.dt,desc(imp))[1:input$num_vars, ]
  })
  
  output$gini <- renderPlot({
    ggplot(data=data(), aes(x=reorder(rn,imp), y=imp)) +
      geom_bar(stat="identity", fill = "dodgerblue3", color="black") + 
      ggtitle('Variable Importance: Gini Impurity') +
      xlab('Variables') +
      ylab('Relative Importance')+
      coord_flip()
  })
  
  # output$cutoff <- renderImage({})
  # output$roc <- renderText({})
}

shinyApp(ui, server)