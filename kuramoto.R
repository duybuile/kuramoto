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

#--------- PROCESSING

# Remove 
jira <- subset(jira, !is.na(jira$Worker.Id) & !is.na(jira$Task.Id))

# For testing

set.seed(3223)
jira_sample <- jira[sample(nrow(jira), nrow(jira)*0.5), ]

jira_sample <- subset(jira_sample, select = c("Worker.Id", "Task.Id"))

t2 <- as.data.frame(table(jira_sample))
t2 <- subset(t2, t2$Freq > 0)

jira <- subset(jira, select = c("Worker.Id", "Task.Id"))

t3 <- as.data.frame(table(jira))
t3 <- subset(t3, t3$Freq > 0)

network <- graph.data.frame(t2, directed = FALSE)
network <- graph.data.frame(t3, directed = FALSE)
#plot(network)

#V(network)
#E(network)

fc <- fastgreedy.community(network)
modularity(fc)
membership(fc)
fc_size <- sizes(fc)
length(size)

wt <- walktrap.community(network)
modularity(wt)
wt_size <- sizes(wt)

save(network, file = "matrix.Rdata")

load("jira_matrix.Rdata")
load("network.Rdata")



#This is for the whole database (takes about 3 hours)
# Result is not great as there is only one community

melt( t, id.vars = "Worker.Id")

get.adjacency(graph.edgelist(as.matrix(2), directed=FALSE))

graph.adjacency(as.matrix(t2), mode = "undirected", weighted = T)
