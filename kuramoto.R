################################
# COMMUNITY DETECTION
################################

setwd("C:/Users/duy.bui/Documents/GitHub/kuramoto")
WORKSPACE <- getwd()

#--------- LIBRARY
library(igraph)
library(reshape2)

#--------- READING FILES

jira <- read.csv(file = paste(WORKSPACE, "jira.csv", sep = "/"), header = TRUE, 
                sep = ",", stringsAsFactors = FALSE, strip.white = TRUE)

tasks <- read.csv(file = paste(WORKSPACE, "effort_on_tasks.csv", sep = "/"), header = TRUE, 
                 sep = ",", stringsAsFactors = FALSE, strip.white = TRUE)

repos <- read.csv(file = paste(WORKSPACE, "effort_on_repos.csv", sep = "/"), header = TRUE, 
                 sep = ",", stringsAsFactors = FALSE, strip.white = TRUE)

# For testing
t <- jira[1:100,]
t <- subset(t, select = c("Worker.Id", "Task.Id"))

t2 <- as.data.frame(table(t))

network <- graph.data.frame(t2)
plot(network)

V(network)
E(network)

melt( t, id.vars = "Worker.Id")

get.adjacency(graph.edgelist(as.matrix(2), directed=FALSE))

graph.adjacency(as.matrix(t2), mode = "undirected", weighted = T)
