library(shiny)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(ROCR)
library(ranger)
library(caret)
library(dplyr)

source('data/shiny_ranger.R', local = TRUE)

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
                div(checkboxGroupInput("planVars", label = h4("Plan Design Variables"), 
                                   choices = plan.dsn,
                                   selected = plan.dsn), style="height: 150px; overflow-y: scroll;"),
                div(checkboxGroupInput("behaviorVars", label = h4("Behavior Variables"), 
                                   choices = behaviors,
                                   selected = behaviors), style="height: 150px; overflow-y: scroll;"),
                div(checkboxGroupInput("controlVars", label = h4("Control Variables"), 
                                   choices = controls,
                                   selected = controls), style="height: 150px; overflow-y: scroll;"),
                hr(),
                div(style="display: inline-block;vertical-align:top; width: 150px;", 
                    numericInput('nonHospWt', 
                                 "NonHosp Wt.", 
                                 value = 0.3,
                                 step = 0.1,
                                 width = '150px')
                ),
                div(style="display: inline-block;vertical-align:top; width: 150px;",
                    numericInput('hospWt',
                                 "Hosp Wt.", 
                                 value = 1,
                                 step = 1,
                                 width = '150px')
                ),
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
                          tabPanel("ROC", plotOutput("roc")),
                          tabPanel("Matrix", verbatimTextOutput("confusion_matrix"))
              )
            ),
            fluidRow(
              infoBoxOutput("aucBox"),
              infoBoxOutput("cutoffBox")
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
  load("data/meta.rda")
  load("data/confusion_matrices.rda")
  load("data/x.test.rda")
  load("data/y.test.rda")
  load('data/train.rda')
  load("data/mepsBehaviorBuckets.rda")
  
  meta_named_char <- c(meta_named_char, age.cat="Binned Ages")

  modelVars <- eventReactive(input$makeModel,
                             {buildHospModel(train, input$planVars, input$behaviorVars, input$controlVars, input$nonHospWt, input$hospWt)},
                             ignoreNULL = FALSE
  )
  
  output$hospitilizationPlot <- renderPlot({ 
    
    fit = modelVars()
    fit.top = fit$variable.importance[order(unlist(fit$variable.importance),decreasing=TRUE)]
    importance = as.data.frame(stack(fit.top))
    colnames(importance) <- c("Importance", "Predictor")
    importance$Description <- meta_named_char[levels(importance$Predictor)]
    
    output$hosp_descriptions <- renderDataTable(importance[c(1:10),c(2:3)], list(searching = FALSE, paging = FALSE))
    
    ggplot(importance[1:10,], aes(x=reorder(Predictor, Importance), y=Importance)) +
      geom_bar(stat="identity", fill = "dodgerblue3", color="black") + 
      xlab('Variables') +
      ylab('Relative Importance') +
      coord_flip()
  })
  
  output$cutoff <- renderPlot({
    fit = modelVars()
    preds.test <- as.data.frame(predict(fit, x.test)$predictions)
    
    classNames <- c('NoHosp', 'Hosp')
    levels(y.test)<-classNames
    
    colnames(preds.test)<-classNames
    cutOff = .7
    
    test.Results<-data.frame(preds.test,
                             obs = y.test,
                             pred = ifelse(preds.test[,classNames[1]] < cutOff, classNames[1], classNames[2]))
    # test Results
    levels(test.Results$pred) <- classNames
    
    
    pred <- prediction( test.Results[,1],  y.test)
    plot(performance(pred, "sens" , x.measure = "cutoff"), col = 'red', ylab= NULL, main="Optimal Cutoff")
    par(mar=c(4,4,4,4))
    par(new=T)
    plot(performance(pred, "spec" , x.measure = "cutoff"),add = TRUE, col = 'blue', xlab = NULL)
    axis(side = 4,  at = .5, labels = 'specificity', padj = 1 )
    legend(.3, .8, legend=c("Sensitivity", "Specificity"),
           col=c("red", "blue"), lty=1, cex=0.75, y.intersp= 1.7, x.intersp= .5)
    
    
  })
  
  output$roc <- renderPlot({
    fit = modelVars()
    preds.test <- as.data.frame(predict(fit, x.test)$predictions)
    
    classNames <- c('NoHosp', 'Hosp')
    levels(y.test)<-classNames
    
    colnames(preds.test)<-classNames
    cutOff = .7
    
    test.Results<-data.frame(preds.test,
                             obs = y.test,
                             pred = ifelse(preds.test[,classNames[1]] < cutOff, classNames[1], classNames[2]))
    # test Results
    levels(test.Results$pred) <- classNames
    pred <- prediction( test.Results[,1],  y.test)
    plot(performance(pred, "tpr" , x.measure = "fpr"), col = 'red', ylab= NULL)
    abline(0,1)
    performance(pred, "auc")
  })
  
  output$behaviorPlot <- renderPlot({
    
    behavior.imp <-behavior_models[input$behavior]
    behavior.imp.dt <- setDT(as.data.frame(behavior.imp), keep.rownames = TRUE)[]
    behavior.imp.dt.top <- behavior.imp.dt[order(-behavior.imp.dt[[input$behavior]])][1:input$beh_vars, ]
    descriptions <- as.data.frame(behavior.imp.dt.top)
    colnames(descriptions) <- c("Predictor", "Importance")
    descriptions$Description <- meta_named_char[behavior.imp.dt.top$rn]

    output$beh_descriptions <- renderDataTable(descriptions[,c(1,3)], list(searching = FALSE, paging = FALSE))
    
    output$confusionPrint <- renderPrint(confusion_matrices[[input$behavior]])
    
    print(ggplot(descriptions[,c(1,2)], aes(x=reorder(Predictor, Importance), y=Importance)) +
            geom_bar(stat="identity", fill = "dodgerblue3", color="black") + 
            ggtitle(paste('Predicting:', meta_named_char[input$behavior], '(', input$behavior, ')')) +
            xlab('Variables') +
            ylab('Relative Importance')+
            coord_flip())
  })
  
  output$aucBox <- renderInfoBox({
    fit = modelVars()
    preds.test <- as.data.frame(predict(fit, x.test)$predictions)
    
    classNames <- c('NoHosp', 'Hosp')
    levels(y.test)<-classNames
    
    colnames(preds.test)<-classNames
    cutOff = .7
    
    test.Results<-data.frame(preds.test,
                             obs = y.test,
                             pred = ifelse(preds.test[,classNames[1]] < cutOff, classNames[1], classNames[2]))
    # test Results
    levels(test.Results$pred) <- classNames
    summary <- twoClassSummary(test.Results, lev = classNames)
    
    infoBox(
      "AUC", paste(round(summary[['ROC']] * 100, digits=1), '%'), icon = icon("check-circle"),
      color = "light-blue", fill = TRUE
    )
  })
  
  
  output$cutoffBox <- renderInfoBox({
    fit = modelVars()
    preds.test <- as.data.frame(predict(fit, x.test)$predictions)
    
    classNames <- c('NoHosp', 'Hosp')
    levels(y.test)<-classNames
    
    colnames(preds.test)<-classNames
    cutOff = .7
    
    test.Results<-data.frame(preds.test,
                             obs = y.test,
                             pred = ifelse(preds.test[,classNames[1]] < cutOff, classNames[1], classNames[2]))
    # test Results
    levels(test.Results$pred) <- classNames
    summary <- twoClassSummary(test.Results, lev = classNames)
    
    infoBox(
      "Sens/Spec", paste(round(summary[['Sens']] * 100, digits=1), '% / ', paste(round(summary[['Spec']] * 100), '%')), icon = icon("list"),
      color = "light-blue", fill = TRUE
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