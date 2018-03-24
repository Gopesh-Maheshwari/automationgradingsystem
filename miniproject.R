library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(tidyr)
library(ggplot2)




ui <- dashboardPage(skin = "black",
                    
                    
                    dashboardHeader(title = "Faculty Feedback",titleWidth = ),
                    
                    dashboardSidebar(
                      
                      fluidPage( 
                        sidebarMenu( 
                          menuItem("FACULTY RATINGS",tabName = "FR",icon = icon("thumbs-up",lib="glyphicon")),
                          menuItem("METHODOLOGY",tabName = "method",icon = icon("zoom-in",lib = "glyphicon")),
                          menuItem("DISCLAIMER",tabName = "disclaim",icon = icon("thumbs-up",lib="glyphicon"))
                          
                          
                          
                        )
                        
                        
                      )
                    ) #end of sidebar
                    ,
                    
                    dashboardBody(
                      
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      
                      fluidPage(
                        tabItems(
                          
                          tabItem(tabName = 'FR',
                                  
                                  fluidRow(
                                    
                                    
                                    box(title = "Teacher's Code",
                                        uiOutput("ts"),
                                        actionButton(inputId = "fetch",label = "View",icon = icon("user",lib="glyphicon")),
                                        HTML("<br> </br>"),
                                        uiOutput('tvalue')
                                    )
                                    ,
                                    box(title = "Graph",
                                        plotlyOutput('tplot'),
                                        HTML("<br> </br>"),
                                        plotlyOutput('wplot')
                                        
                                        )
                          
                                    
                                    
                                  ) 
                          )
                          
                          
                        ) ,# end of FR tab
                        
                        tabItem(tabName = 'method',
                                
                                fluidRow(
                                  
                                  
                                  
                                  
                                )
                        ), #end of Method Tab 
                        
                        tabItem(tabName = 'disclaim',
                                
                                fluidRow(
                                  
                                  
                                  
                                  
                                  
                                )
                        )
                      )#end of tabItems 
                      
                    )#end of fluid page
)# end of dashboard body 

#end of dashboard page

server <- function(input, output) {
  
  sem <- read.csv("semester.csv",sep = ";")
  week<- read.csv("weekly.csv",sep = ";")
  
  # displaying the input box
  output$ts <- renderUI({
    
    selectInput(inputId = "tselect",label = "Choose Teacher ID",choices = unique(sem$teacher))
  })
  
  #action button template
  v <- reactiveValues(check = FALSE)
  
  observeEvent(input$fetch,{
    
    v$check <- input$fetch
  })
  
  observeEvent(input$FR,{
    v$check <- FALSE
  })
  
  
  output$tplot <- renderPlotly(
    
    {
      
      if (v$check == FALSE) return()
      
      isolate({
        
        test <- subset(sem, sem$teacher == as.integer(input$tselect),select = c(-1))
        test$fairness <- mean(test$fairness)
        test$knowledge <-mean(test$knowledge)
        test$topics   <- mean(test$topics)
        test$doubt <- mean(test$doubt)
        test$interaction <- mean(test$interaction)
        test$practicalknowledge <- mean(test$practicalknowledge)
        data <- gather(test[1,],key = 'Attributes',value = 'Ratings')
        p <- plot_ly(data , x = ~Attributes,y=~Ratings,type = 'bar',color = ~Attributes)%>%
          layout(title = 'Teacher Semester Wise Ratings',
                 xaxis = list(title = "contraints"),
                 yaxis = list(title = "rating"),
                 paper_bgcolor = 'rgba(245, 246, 249, 1)',
                 plot_bgcolor = 'rgba(245, 246, 249, 1)',
                 showlegend = FALSE) 
        
        
      })
      
    }
  )
  output$wplot <- renderPlotly(
    
    {
      
      if (v$check == FALSE) return()
      
      isolate({
        
        wtest <- subset(week, week$teacher == as.integer(input$tselect),select = c(-1))
        wtest$knowledge <-mean(wtest$knowledge)
        wtest$topicscovered<- mean(wtest$topicscovered)
        wtest$doubtclearing <- mean(wtest$doubtclearing)
        data1 <- gather(wtest[1,],key = 'Attributes',value = 'Ratings')
        p1 <- plot_ly(data1 , x = ~Attributes,y=~Ratings,type = 'bar',color = ~Attributes)%>%
          layout(title = 'Teacher Weekly Ratings',
                 xaxis = list(title = "contraints"),
                 yaxis = list(title = "rating"),
                 paper_bgcolor = 'rgba(245, 246, 249, 1)',
                 plot_bgcolor = 'rgba(245, 246, 249, 1)',
                 showlegend = FALSE) 
        
        
      })
      
    }
  )
  
  output$tvalue<- renderUI(
    
    
    {
      
      if (v$check == FALSE) return()
      
      isolate({
        
        s <- subset(sem, sem$teacher == as.integer(input$tselect),select = c(-1))
        s$fairness <- mean(s$fairness)
        s$knowledge <-mean(s$knowledge)
        s$topics   <- mean(s$topics)
        s$doubt <- mean(s$doubt)
        s$interaction <- mean(s$interaction)
        s$practicalknowledge <- mean(s$practicalknowledge)
        
        sem_val <- s[1,]
        sv <- (sem_val$knowledge * 0.2) +(sem_val$practicalknowledge * 0.3) +(sem_val$interaction * 0.15) +(sem_val$fairness * 0.2) +(sem_val$topics * 0.05) + (sem_val$doubt * 0.1)
        
        w <- subset(week,week$teacher == as.integer(input$tselect),select = c(-1))
        w$knowledge <-mean(w$knowledge)
        w$doubtclearing <- mean(w$doubtclearing)
        w$topicscovered <-mean(w$topicscovered)
        
        week_val <- w[1,]
        wv <- (week_val$knowledge * 0.35) + (week_val$doubtclearing * 0.4) + (week_val$topicscovered * 0.25) 
        final <- (sv * 0.4) + (wv * 0.6)
        
        v <-valueBox(value = final,subtitle = 'Score',icon = icon("user",lib="glyphicon"),color = 'blue')
        
      })
      
    }
    
  )
  
  
  
  
  
  
  
  
  
  
  
}


shinyApp(ui = ui, server = server)