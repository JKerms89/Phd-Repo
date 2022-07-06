#################2-mode network ERGM national network #######################

library(statnet)
library(igraph)
library(readxl)
library(dplyr)
library(tidyr)
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
RCA <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- RCA %>% filter(!is.na(act2obj) & act2obj == 'national' & !is.na(parfam))
actnodes <- data.frame(RCA$actorg, RCA$act_nat, RCA$act_type, RCA$act_scope_RECON, RCA$parfam)
actnodes$RCA.act_nat[(actnodes$RCA.act_nat == "NA")] <- "UNSPECIFIED"
objnodes <- data.frame(RCA$objs, RCA$objnat, RCA$objtype, RCA$obj_scope_RECON)
objnodes$parfam <- NA
colnames(actnodes)<-c('Name','Country','Type', 'Scope', 'Parfam')
colnames(objnodes)<-c('Name','Country','Type', 'Scope', 'Parfam')
nodelist<-rbind(actnodes,objnodes)
nodelist<-unique(nodelist)
nodelist <- nodelist[!is.na(nodelist$Type),]
edgelist <- data.frame(actnodes$Name, objnodes$Name)
edgelist$objnodes.Name <- paste0(edgelist$objnodes.Name, " constituency")
colnames(edgelist)<-c('Claimant','Object')
edgematrix <- t(matrix(edgelist,nrow=2))
write.table(edgelist, 'edgelist2Mnatnet.csv', row.names=FALSE, col.names=FALSE, sep=",")
g <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=FALSE))
g <- graph.adjacency(g, mode = "undirected", weighted=T)

#Convert to statnet graph
library(statnet)
library(intergraph)
g2 <- asNetwork(g)

# OR, create a statnet object from the outset.Here is how to do it.
# first, import the 2MO edgelist csv file (refer to the previous code above for the
# nodelist)
df <- read.csv("edgelist2Mnatnet.csv",header = FALSE)
mode1 <- unique(df[,1])
mode2 <- unique(df[,2])
length(mode2)
library(statnet)
natnet <- as.network(df, bipartite = 153, directed = FALSE)
natnet%v%'org_nat'<- nodelist$Country
natnet%v%'org_type' <- nodelist$Type
natnet%v%'org_fam' <- nodelist$Parfam
natnet%v%'org_scope' <- nodelist$Scope

# Convert coefficient to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#Generally, the first term that people use is the edges term. It is a statistic which counts how many edges there are in the network.

m1 <- ergm(natnet ~ edges)
summary(m1)
plogis(coef(m1)[['edges']])

#####

m2 <- as.formula(natnet ~ 
                   b1factor('org_scope') + 
                   edges) # edges term

m3 <- ergm(m2)
summary(m3)
theta <- m3$coef
logit2prob(theta)

#########

m2 <- as.formula(natnet ~ 
                   b1factor('org_type') + 
                   edges) # edges term

m3 <- ergm(m2)
summary(m3)
theta <- m3$coef
logit2prob(theta)

# The results above show that politicians are twice as likely to refer to identity objects vis-a-vis 
# gov/executives. 


