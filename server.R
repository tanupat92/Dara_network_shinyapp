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
library(DT)

modularList <- list('Whole graph' = 21, '1'=1, '2'=2, '3'=3, '4'=4, Chinese=5, '6'=6, Hollywood=7,Country=8,Channel3=9,'10'=10,'11'=11,'12'=12,'13'=13,
                    Reporter=14,GirlyBerry=15,Hollywood2=16,'17'=17,'18'=18,'19'=19,'20'=20)
eigenvectorList <- list('Whole graph' = 20, Chinese=1, '2'=2, '3'=3, Country=4, '5'=5, '6'=6, '7'=7,'8'=8,Hollywood=9,'10'=10,'Chinese Hollywood'=11,'12'=12,'13'=13,
                        '14'=14,'15'=15,'16'=16,Director=17,'18'=18,'19'=19)
averageCentrality <- function(graph, sel = c(1,2,3,4,5), topN = 10, digits =2){
  #'input igraph type, output data.frame of nodes ranked by average centrality
  avLis <- list()
  for (i in sel){
    if (i == 1){
      deg <- data.frame(degree(graph))
      deg <- (deg-min(deg))/(max(deg)- min(deg))
      avLis[['degree']] <- deg
    } else if (i ==2){
      bet <- data.frame(betweenness(graph))
      bet <- (bet-min(bet))/(max(bet)-min(bet))
      avLis[['betweeenness']] <- bet
    } else if (i==3){
      close <- data.frame(closeness(graph))
      close <- (close-min(close))/(max(close)-min(close))
      avLis[['closeness']] <- close
    } else if (i==4){
      eigen <- data.frame(eigen_centrality(graph)$vector)
      eigen <- (eigen-min(eigen))/(max(eigen)-min(eigen))
      avLis[['eigenvector']] <- eigen
    } else if (i==5){
      page <- data.frame(page_rank(graph)$vector)
      page <- (page-min(page))/(max(page)-min(page))
      avLis[['PageRank']] <- page
    }
  }
  n <- length(sel)
  df <- data.frame(index = 1:dim(avLis[[1]])[1])
  rownames(df) <- rownames(avLis[[1]])
  if (n > 1){
    for (i in 1:length(avLis)){
      tmp <- data.frame(avLis[i])
      colnames(tmp) <- i
      df <- merge(df, tmp, by=0, all=TRUE)
      rownames(df) <- df$Row.names
      df <- df %>% select(-Row.names)
    }
    m <- n+1
    df <- df[,2:m]
    df$name <- rownames(df)
    df <- df[,c(n+1,1:n)]
    rankAv <- df %>% gather(key, value,-name) %>% 
      group_by(name) %>% mutate(average = mean(value)) %>% 
      spread(key,value) %>% arrange(desc(average))
    colnames(rankAv) <- c('name', 'average', names(avLis))
  } else{
    df$name <- rownames(df)
    df <- merge(df, data.frame(avLis[1]), by = 0, all=TRUE)
    df <- df[,c(3,4)]
    rankAv <- df %>% mutate(average = df[,2])%>% arrange(desc(average))
  }
  round_df <- function(x, digits) {
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
  }
  rankAv <- round_df(rankAv, digits)
  return(rankAv[1:topN,])
}

gUn <- readRDS('gUn.rds')


makeSubgraphCluster <- function(graph, cluster=c(cluster_fast_greedy, cluster_leading_eigen), weight = c(TRUE,FALSE), weightname, includeWhole= TRUE, special){
  if (special == "Greedy modularity maximization"){
    den <- cluster_fast_greedy(graph, weights = E(graph)$total)
  } else if (special == "Leading eigenvector (unweighted)"){
    den <- cluster_leading_eigen(graph, weights =NULL)
  } else{
    w <- ifelse(weight, E(graph)$weightname, NULL)
    den <- cluster(graph, weights = w)
  }
  commList <- list()
  
  n <- max(den$membership)
  for (i in 1:n){
    commList[[i]] <- induced_subgraph(graph, vids=den$membership==i)
  }
  if (includeWhole){
    commList[[n+1]] <- graph
  }
  return(commList)
}

server <- function(input, output,session) {

  commList <- reactive({ makeSubgraphCluster(gUn,NULL, NULL,NULL, TRUE, input$community )
  })
  df <- reactive({averageCentrality(commList()[[as.integer(input$class)]],sel = input$central, topN = as.integer(input$top))})
  dffull <- eventReactive(input$show_graph,{averageCentrality(commList()[[as.integer(input$class)]], topN=gorder(commList()[[as.integer(input$class)]]))})
  value <- reactive({df() %>% mutate(triple=average*20) %>% .[['triple']]})
  graph <- reactive({commList()[[as.integer(input$class)]] %>%set.vertex.attribute(name='value', index=V(commList()[[as.integer(input$class)]])[V(commList()[[as.integer(input$class)]])$name %in% df()[['name']]], value = as.integer(value()))})
  graph2 <- reactive({graph() %>%set.vertex.attribute(name='value', index=V(commList()[[as.integer(input$class)]])[!(V(commList()[[as.integer(input$class)]])$name %in% df()[['name']])], value = as.integer(5))})
  data <- eventReactive(input$show_graph,{graph2() %>% 
      set.vertex.attribute(name='group', index=V(commList()[[as.integer(input$class)]])[V(commList()[[as.integer(input$class)]])$name %in% df()[['name']]], value = paste('top',as.integer(input$top))) %>%
      toVisNetworkData()})
  output$network <- renderVisNetwork({
    net <- data()
    #reactive({net$nodes[net$nodes['id']] %in% averageCentrality(commList()[[as.integer(input$class)]])$name})
    visNetwork(net$nodes, net$edges) %>%
      visNodes(font = list(size=20)) %>%
      visOptions(selectedBy = list(variable="group",main = "All nodes", selected = paste('top', input$top)), 
                 highlightNearest = TRUE, 
                 nodesIdSelection = TRUE) %>%
      visPhysics(stabilization = FALSE) %>% 
      visEdges(smooth = TRUE)
  })
  output$table <- DT::renderDT({
    dffull()
  })
  
  
  observe({
    x <- input$community
    if (x == "Greedy modularity maximization"){
      updateSelectInput(session, "class",
                        label = paste("Select from 20 classes or the whole graph:"),
                        choices = modularList,
                        selected = 1)
    } else {
      updateSelectInput(session, "class",
                        label = paste("Select from 19 classes or the whole graph:"),
                        choices = eigenvectorList,
                        selected = 1)
    }
        
  })
  
}