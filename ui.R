#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)
library(shinyWidgets)
library(shinythemes)
library(DT)

modularList <- list('Whole graph' = 21, '1'=1, '2'=2, '3'=3, '4'=4, Chinese=5, '6'=6, Hollywood=7,'8'=8,'9'=9,'10'=10,'11'=11,'12'=12,'13'=13,
                    '14'=14,'15'=15,'16'=16,'17'=17,'18'=18,'19'=19,'20'=20)
eigenvectorList <- list('Whole graph' = 19, '1'=1, '2'=2, '3'=3, '4'=4, '5'=5, '6'=6, '7'=7,'8'=8,'9'=9,'10'=10,'11'=11,'12'=12,'13'=13,
                    '14'=14,'15'=15,'16'=16,'17'=17,'18'=18)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dara Network"),
  theme = shinythemes::shinytheme('paper'),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "community",
        label = "Choose clustering methods:", 
        choices = c("Greedy modularity maximization", "Leading eigenvector (unweighted)")
      ),
      selectInput("class",
                  "Choose modularity class ():",
                  choices = modularList,
                  selected = 1),
      checkboxGroupInput(
        inputId = "central",
        label = "Choose centrality measures:", 
        choiceNames = c("Degree", "Betweenness", "Closeness", "Eigenvector","PageRank"),
        choiceValues = c(1,2,3,4,5),
        selected = c(1,2,3,4,5),
        inline = FALSE
      ),
      sliderInput("top", "Pick the number of top rank nodes:", min = 1, max = 30, value = 10,step =1),
      actionButton('show_graph', 'Show'))
    ,
    mainPanel(
      tabsetPanel(
        tabPanel('network', visNetworkOutput("network")),
        tabPanel('table', DT::dataTableOutput("table"))
      )
      
    )
    
  )
))



