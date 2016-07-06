library(igraph)
library(dplyr)
library(data.table)

### 1. Functions----
#Trun edge list to adjacency matrix
graphized <- function(x){
  mat <- as.matrix(get.adjacency(graph.edgelist(as.matrix(x), directed=T)))
  graph <- graph.adjacency(mat,mode="directed", weighted = TRUE)
  return(graph)
}

#Describing basic topography of graphs
topo <- function(x){
  nodes <- vcount(x)
  edges <- ecount(x)
  density <- graph.density(x, loops = T)
  
  topo <- data.frame(nodes,edges,density)
  return(topo)
}

### 2. Data prepration----
#I have already wrangled the email database to extract sender and recievers. Emails might have multiple recipients, and CCs.
#I have tried the simplest analysis with the first recipient as the most important person in the correspondance.
# same person might have ifferent addresses, names, mispelling, etc. entitiy resolution has been done to consolidate them
df <- read.csv('hless.csv', sep = ",")
levels(df$reason)[length(levels(df$reason)) > 3 ] <- "1"
df$X <- NULL
colnames(df) <- c("From", "To", "Reason")

#turn the edge lsit to a graph
edge_list <- df[c(1,2)]
graph <- graphized(edge_list)
topo(graph)

### 3. Senders/Recipeints of confidential/secret email
#to do: filter data frame based on confidential emails. trasnpose and AAt.
senders <- select(df, To, Reason)


