library(igraph)
library(VGAM)
library(som)
library(kohonen)
library(modeest)
 
maxiter<-10
myself<-5
c<-4
verbose<-1
count_error<-0
 
g = read.graph("karate.gml",format="gml")
 
# obtain summary information about the graph
summary(g)
 
# fastgreedy community finding algorithm
fc = fastgreedy.community(as.undirected(g))
 
if (verbose==1){
    fastcom<-community.to.membership(g,fc$merges, steps = which.max(fc$modularity)-1)
    V(g)$color <- fastcom$membership+1
    g$layout <- layout.fruchterman.reingold
    plot(g)
    title("Fast Greedy community structure")
}
 
# InfoMap community finding algorithm (can be slow)
imc = infomap.community(as.undirected(g))
 
if (verbose==1){
    V(g)$color <- imc$membership
    g$layout <- layout.fruchterman.reingold
    dev.new()
    plot(g)
    title("InfoMap Community structure ")
}
 
cat(" Modularity InfoMap Algorithm -> \"", modularity(imc) , "\"\n")
 
cat(" Modularity Fast Greedy Algorithm -> \"", modularity(fc), "\"\n")
 
A<-get.adjacency(g)
A<-matrix(A,dim(A))
A<- A + diag(dim(A)[1])
somA <- som(data = A, grid = somgrid(c,1, "hexagonal"), keep.data = TRUE) 
 
somApredict <- predict(somA, A, trainX = A, trainY = imc$membership)
 
# predictions without adjust
if (verbose==1){
    V(g)$color <- somApredict$unit.classif
    g$layout <- layout.fruchterman.reingold
    dev.new()
    plot(g)
    title("Community structure SOM without fine tuning")
}
 
vecinos <- neighborhood(g, 1,nodes=V(g), mode="all")
# editing predictions
newclass <- array(0, dim(A)[1])
oldclass<-newclass
 
for (init in 1:maxiter) {
 
    for(k in 1:dim(A)[1]){ 
 
        if (init>myself){
            neimode<-as.integer(mlv( oldclass[vecinos[[k]][2:length(vecinos[[k]])]  ] , method = "mfv")$M)
        }
        else{
            neimode<-as.integer( mlv( somApredict$unit.classif[vecinos[[k]][1:length(vecinos[[k]])]] , method = "mfv")$M)
        }
 
        if (length(neimode)>1){
            cat(" Node -> \"", k , " is between comunities: ", neimode  ,   "\n")
        }
 
        newclass[k]<-neimode[round(runif(1,min=1, max =length(neimode)))]
 
        if ((init> myself)&(length(neimode)>1)){
            newclass[k]<-oldclass[k]
        }
    }
cat(init)
oldclass<-as.integer(newclass)
newclass<-array(0, dim(A)[1])
}
 
# predictions with adjust based on neighbours
if (verbose==1){
    V(g)$color <- oldclass
    g$layout <- layout.fruchterman.reingold
    dev.new()
    plot(g)
    title("Community structure SOM with fine tuning")
}
 
# Strength of communities and merit factor for SOM
COM <- unique(oldclass)
S <-array(-1 ,(length(COM)))
for (j in 1:length(COM)) {
    intra <- (sum(A[oldclass==COM[j],oldclass==COM[j]]) - sum(oldclass==COM[j]))/2
    extra <- (sum(A[oldclass== COM[j],]) - sum(oldclass==COM[j])) - 2* intra
    total <- intra + extra
    S[j] <- (intra - extra)/total
    cat(j)
}
Q_SOM <- sum(S)
S_SOM <- S
# Strength of communities and merit factor for fast greedy
 
COM <- unique(fc$membership)
S <-array(-1 ,(length(COM)))
for (j in 1:length(COM)) {
    intra <- (sum(A[fc$membership==COM[j],fc$membership==COM[j]]) - sum(fc$membership==COM[j]))/2
    extra <- (sum(A[fc$membership== COM[j],]) - sum(fc$membership==COM[j])) - 2*intra
    total <- intra + extra
    S[j] <- (intra - extra)/total
    cat(j)
}
S_FS<-S
Q_FS <- sum(S)
 
# Strength of communities and merit factor for infomap
 
COM <- unique(imc$membership)
S <-array(-1 ,(length(COM)))
for (j in 1:length(COM)) {
    intra <- (sum(A[imc$membership==COM[j],imc$membership==COM[j]]) - sum(imc$membership==COM[j]))/2
    extra <- (sum(A[imc$membership== COM[j],]) - sum(imc$membership==COM[j])) - 2*intra
    total <- intra + extra
    S[j] <- (intra - extra)/total
    cat(j)
}
Q_IM <- sum(S)
S_IM <- S
 
cat("Strength SOM \n");
cat("----------------------------\n");
cat(S_SOM , "\n")
cat("Strength Infomap \n");
cat("----------------------------\n");
cat(S_IM, "\n")
cat("Strength Fast Greedy \n");
cat("----------------------------\n");
cat(S_FS , "\n")
 
cat("Merit Factor \n");
cat("----------------------------\n");
cat("  SOM    INFOMAP   FAST-GREEDY\n");
cat(Q_SOM,Q_IM , Q_FS)
