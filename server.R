#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)
library(igraph)
library(tidyverse)


averageCentrality <- function(graph, select = c(1,2,3,4,5), topN = 10){
  #'input igraph type, output data.frame of nodes ranked by average centrality
  deg <- data.frame(degree(graph))
  bet <- data.frame(betweenness(graph))
  close <- data.frame(closeness(graph))
  eigen <- data.frame(eigen_centrality(graph)$vector)
  page <- data.frame(page_rank(graph)$vector)
  deg <- (deg-min(deg))/(max(deg)- min(deg))
  bet <- (bet-min(bet))/(max(bet)-min(bet))
  close <- (close-min(close))/(max(close)-min(close))
  eigen <- (eigen-min(eigen))/(max(eigen)-min(eigen))
  page <- (page-min(page))/(max(page)-min(page))
  avLis <- list(deg,bet,close,eigen,page)
  avLis <- avLis[select]
  df <- data.frame(index = 1:dim(deg)[1])
  rownames(df) <- rownames(deg)
  for (i in 1:length(avLis)){
    tmp <- data.frame(avLis[i])
    colnames(tmp) <- i
    df <- merge(df, tmp, by=0, all=TRUE)
    rownames(df) <- df$Row.names
    df <- df %>% select(-Row.names)
  }
  df <- df[,2:6]
  df$name <- rownames(df)
  df <- df[,c(6,1:length(select))]
  rankAv <- df %>% gather(key, value,-name) %>% 
    group_by(name) %>% mutate(average = mean(value)) %>% 
    spread(key,value) %>% arrange(desc(average))
  return(rankAv[1:topN,])
}

gUn <- readRDS('gUn.rds')
denW <- cluster_fast_greedy(gUn, weights = E(gUn)$total) 
commList <- list()
for (i in 1:20){
  commList[[i]] <- induced_subgraph(gUn, vids=denW$membership == i )
}


# Define server logic required to draw a histogram
server <- function(input, output) {
  name <- reactive({averageCentrality(commList[[as.integer(input$class)]]) %>% .[['name']]})
  value <- reactive({averageCentrality(commList[[as.integer(input$class)]]) %>% mutate(triple=average*20) %>% .[['triple']]})
  graph <- reactive({commList[[as.integer(input$class)]] %>%set.vertex.attribute(name='value', index=V(commList[[as.integer(input$class)]])[V(commList[[as.integer(input$class)]])$name %in% name()], value = as.integer(value()))})
  graph2 <- reactive({graph() %>%set.vertex.attribute(name='value', index=V(commList[[as.integer(input$class)]])[!(V(commList[[as.integer(input$class)]])$name %in% name())], value = as.integer(5))})
  data <- eventReactive(input$show_graph,{graph2() %>% 
      set.vertex.attribute(name='group', index=V(commList[[as.integer(input$class)]])[V(commList[[as.integer(input$class)]])$name %in% name()], value = 'top10') %>%
      toVisNetworkData()})
  #node <- eventReactive(input$show_graph,{data.frame(id =igraph::as_data_frame(commList[[input$class]], what = 'vertices')[,1], labels = id =igraph::as_data_frame(commList[[input$class]], what = 'vertices')[,1])})
  #edge <- reactive({igraph::as_data_frame(commList[[input$class]], what='edges')[,1:2]})
  output$network <- renderVisNetwork({
    net <- data()
    reactive({net$nodes[net$nodes['id']] %in% averageCentrality(commList[[as.integer(input$class)]])$name})
    visNetwork(net$nodes, net$edges, height = '1400px', width = '700px') %>%
      visNodes(font = list(size=20)) %>%
      visOptions(selectedBy = "group", 
                 highlightNearest = TRUE, 
                 nodesIdSelection = TRUE) %>%
      visPhysics(stabilization = TRUE)
  })
  
  
  
}