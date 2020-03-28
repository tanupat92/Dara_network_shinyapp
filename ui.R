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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Star Network"),
  theme = shinythemes::shinytheme('paper'),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      pickerInput("class",
                  "Choose modularity class",
                  choices = 1:20),
      actionButton('show_graph', 'Show'))
    ,
    mainPanel(
      visNetworkOutput("network")
    )
    
    # Show a plot of the generated distribution
    
  )
))



