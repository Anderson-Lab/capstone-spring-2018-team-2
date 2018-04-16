library(shiny)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(dplyr)

variables = c(1:24)
load("data/behavior_models.rda")
load("data/planDesignVars.rda")
load("data/behaviorVars.rda")
load("data/controlVars.rda")
load("data/preventiveVars.rda")

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
      ),
      menuItem("CDC Guidelines",
               tabName = "buckets",
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
                width=NULL,
                title = "Target: IPDIS15X (Inpatient Hospitalizations)",
                solidHeader = TRUE, 
                status = "primary",
                div(checkboxGroupInput("planDesignVars", label = h4("Plan Design Variables"), 
                                   choices = plan.dsn,
                                   selected = plan.dsn), style="height: 150px; overflow-y: scroll;"),
                div(checkboxGroupInput("behaviorVars", label = h4("Behavior Variables"), 
                                   choices = behaviors,
                                   selected = behaviors), style="height: 150px; overflow-y: scroll;"),
                div(checkboxGroupInput("controlVars", label = h4("Control Variables"), 
                                   choices = controls,
                                   selected = controls), style="height: 150px; overflow-y: scroll;"),
                hr(),
                actionButton("makeModel", "Generate Model")
  
              )
          ),
          column(width=7,
            box(
              width=NULL,
              solidHeader = TRUE, 
              status = "primary",
              tabsetPanel(type = "tabs",
                          tabPanel("Gini", plotOutput("hospitilizationPlot")),
                          tabPanel("Var Descriptions", div(dataTableOutput("hosp_descriptions"), style = "font-size:80%")),
                          tabPanel("Cutoff", plotOutput("cutoff")),
                          tabPanel("ROC", plotOutput("roc"))
              )
            ),
            fluidRow(
              infoBoxOutput("aucBox"),
              infoBoxOutput("accBox")
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
      ),
      tabItem(tabName = "buckets",
              fluidRow(
                column(width = 6,
                       box(
                         width=NULL,
                         title = "Follow/Not-Follow Guidelines",
                         solidHeader = TRUE, 
                         status = "primary",
                         div(checkboxGroupInput("preventiveVars", label = h4("Choose Preventive Care Variables"), 
                                                choices = preventive_behaviors,
                                                selected = preventive_behaviors), style="height: 375px; overflow-y: scroll;")
                       )
                ),
                column(width = 6,
                       box(
                         width = NULL,
                         plotOutput("cdcPlot")
                  )
                )
              )
              
      )
    )
  )
)

server <- function(input, output) {
  load("data/meta.rda")
  load("data/confusion_matrices.rda")
  load("data/mepsPrivate.2015.rda")
  load("data/mepsBehaviorBuckets.rda")
  # source('data/shiny_ranger.R', local = TRUE)
  
  meta_named_char <- c(meta_named_char, age.cat="concatenated age")
  
  output$hospitilizationPlot <- renderPlot({
    load("data/ranger_imp.rda")
    hosp.imp.dt.top <- arrange(imp.dt,desc(imp))[1:10, ]
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
    legend(.3, .8, legend=c("Sensitivity", "Specificity"),
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
  
  output$aucBox <- renderInfoBox({
    infoBox(
      "AUC", "90%", icon = icon("check-circle"),
      color = "light-blue", fill = TRUE
    )
  })
  
  output$accBox <- renderInfoBox({
    infoBox(
      "Accuracy", "74%", icon = icon("thumbs-up"),
      color = "yellow", fill = TRUE
      )
  })
  
  
  output$cdcPlot <- renderPlot({
    buckets %>%
      filter(AGE15X > 40 & AGE15X <= 60) %>%
      group_by(behave_bucket) %>%
      summarize(Frequency = n()) %>%
      ggplot(., aes(x = behave_bucket, y = Frequency)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      ggtitle("Those Who Follow CDC Guidelines, Age 40-60 (MEPS 2015)")
  })
  
}

shinyApp(ui, server)