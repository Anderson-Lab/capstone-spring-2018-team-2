library(shiny)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(dplyr)

variables = c(1:24)

ui <- dashboardPage(
  dashboardHeader(title = "Benefit Focus & CofC"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Hospitilization", 
               tabName = "hospitilization", 
               icon = icon("dashboard")
      ),
      menuItem("Behaviors",
               tabName = "behaviors",
               icon = icon("dashboard")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "hospitilization", class = "active",
        fluidRow(
          column(width=5,
            box(
              width=NULL, # needs to be set to null when using column layout
              sliderInput("hosp_vars", "Importance variables to show: ",min = 1, max = 15, value = 10)
            ),
            box(
              width=NULL,
              div(dataTableOutput("hosp_descriptions"), style = "font-size:80%")
            )
          ),
          column(width=7,
            box(
              width=NULL,
              tabsetPanel(type = "tabs",
                          tabPanel("Gini", plotOutput("hospitilizationPlot")),
                          tabPanel("Cutoff", plotOutput("cutoff")),
                          tabPanel("ROC", plotOutput("roc"))
              )
            ),
            box(
              title = "Gini Impurity", width = NULL, background = "light-blue",
              "These are variable importance values reported from a ranger model that predicts hospital discharges"
            )
          )
        )
      ),
      tabItem(tabName = "behaviors",
          fluidRow(
            column(width=5,
              box(
                width = NULL,
                selectInput("behavior", "Target: ", names(behavior_models)),
                sliderInput("beh_vars", "Importance variables to show: ",min = 1, max = 15, value = 10)
              ),
              box(
                width = NULL,
                div(dataTableOutput("beh_descriptions"), style = "font-size:80%")
              )
            ),
            column(width=7,
              box(
                width = NULL,
                tabsetPanel(type = "tabs",
                            tabPanel("Plot", plotOutput("behaviorPlot")),
                            tabPanel("Confusion Matrix", verbatimTextOutput("confusionPrint"))                )
              ),
              box(
                title = "Gini Impurity", width = NULL, background = "light-blue",
                "Variable importance values from a behavior ranger model"
              )
            )
        )
      )
    )
  )
)

server <- function(input, output) {
  load("data/behavior_models.rda")
  load("data/meta.rda")
  load("data/confusion_matrices.rda")
  
  meta_named_char <- c(meta_named_char, age.cat="concatenated age")
  
  output$hospitilizationPlot <- renderPlot({
    load("data/ranger_imp.rda")
    hosp.imp.dt.top <- arrange(imp.dt,desc(imp))[1:input$hosp_vars, ]
    descriptions = as.data.frame(hosp.imp.dt.top$rn)
    colnames(descriptions) <- c("Predictor")
    descriptions$Description <- meta_named_char[hosp.imp.dt.top$rn]
    
    output$hosp_descriptions <- renderDataTable(descriptions, list(searching = FALSE, paging = FALSE))
    
    ggplot(hosp.imp.dt.top, aes(x=reorder(rn,imp), y=imp)) +
      geom_bar(stat="identity", fill = "dodgerblue3", color="black") + 
      xlab('Variables') +
      ylab('Relative Importance') +
      coord_flip()
  })
  
  
  output$cutoff <- renderPlot({
    library(ROCR)
    load('data/ranger_hosp_fit.rda')
    load('data/ranger_hosp_preds.rda')
    load('data/ranger_hosp_true.rda')
    classNames <- c('NoHosp', 'Hosp')
    levels(y.test)<-classNames
    colnames(preds.test)<-classNames
    pred <- prediction( preds.test[,1],  y.test)
    plot(performance(pred, "sens" , x.measure = "cutoff"), col = 'red', ylab= NULL, main="Optimal Cutoff")
    par(mar=c(4,4,4,4))
    par(new=T)
    plot(performance(pred, "spec" , x.measure = "cutoff"),add = TRUE, col = 'blue', xlab = NULL)
    axis(side = 4,  at = .5, labels = 'specificity', padj = 1 )
    legend(.4, .2, legend=c("Sensitivity", "Specificity"),
           col=c("red", "blue"), lty=1, cex=0.75, y.intersp= 1.7, x.intersp= .5)
    
  })
  
  output$roc <- renderPlot({
    library(ROCR)
    load('data/ranger_hosp_fit.rda')
    load('data/ranger_hosp_preds.rda')
    load('data/ranger_hosp_true.rda')
    classNames <- c('NoHosp', 'Hosp')
    levels(y.test)<-classNames
    colnames(preds.test)<-classNames
    pred <- prediction( preds.test[,1],  y.test)
    plot(performance(pred, "tpr" , x.measure = "fpr"), col = 'red', ylab= NULL)
    abline(0,1)
    performance(pred, "auc")
  })
  
  output$behaviorPlot <- renderPlot({
    
    behavior.imp <-behavior_models[input$behavior]
    behavior.imp.dt <- setDT(as.data.frame(behavior.imp), keep.rownames = TRUE)[]
    behavior.imp.dt.top <- behavior.imp.dt[order(-behavior.imp.dt[[input$behavior]])][1:input$beh_vars, ]
    descriptions <- as.data.frame(behavior.imp.dt.top$rn)
    colnames(descriptions) <- c("Predictor")
    descriptions$Description <- meta_named_char[behavior.imp.dt.top$rn]

    output$beh_descriptions <- renderDataTable(descriptions, list(searching = FALSE, paging = FALSE))
    
    output$confusionPrint <- renderPrint(confusion_matrices[[input$behavior]])
    
    print(ggplot(behavior.imp.dt.top, aes(x=reorder(rn,behavior.imp[[input$behavior]][1:input$beh_vars]), y=behavior.imp[[input$behavior]][1:input$beh_vars])) +
            geom_bar(stat="identity", fill = "dodgerblue3", color="black") + 
            ggtitle(paste('Predicting:', meta_named_char[input$behavior], '(', input$behavior, ')')) +
            xlab('Variables') +
            ylab('Relative Importance')+
            coord_flip())
  })
}

shinyApp(ui, server)