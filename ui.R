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

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dara Network"),
  theme = shinythemes::shinytheme('paper'),
  sidebarLayout(
    sidebarPanel(
      pickerInput("class",
                  "Choose modularity class (Greedy modularity maximization):",
                  choices = 1:20),
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



