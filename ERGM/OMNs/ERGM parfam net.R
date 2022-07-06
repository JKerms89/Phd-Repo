#################2-mode network (parfam) ERGM ######################
library(statnet)
library(network)
library(igraph)
library(readxl)
library(dplyr)
library(tidyr)
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
RCA <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA17.xlsx")
RCA <- RCA %>% filter(!is.na(act2obj) & !is.na(parfam) & act2obj == 'national')
actnodes <- data.frame(RCA$actorg, RCA$act_nat, RCA$parfam, RCA$act_type)
objnodes <- data.frame(RCA$objs, RCA$objnat)
objnodes$parfam <- NA
objnodes$Type <- NA
colnames(actnodes)<-c('Name','Country', 'Parfam', 'Type')
colnames(objnodes)<-c('Name','Country', 'Parfam', 'Type')
nodelist<-rbind(actnodes,objnodes)
nodelist<-unique(nodelist)
nodelist$Parfam[is.na(nodelist$Parfam)] <- "NOT APPLICABLE"
nodelist$Type[is.na(nodelist$Type)] <- "NOT APPLICABLE"
# Make edgelist
edgelist <- data.frame(actnodes$Name, objnodes$Name)
edgelist$objnodes.Name <- paste0(edgelist$objnodes.Name, " constituency")
colnames(edgelist)<-c('V1','V2')
write.table(edgelist, 'edgelistnetparfam.csv', row.names=FALSE, col.names=FALSE, sep=",")
# Then make the statnet graph object.
df <- read.csv("edgelistnetparfam.csv",header = FALSE)
mode1 <- unique(df[,1])
mode2 <- unique(df[,2])
length(mode2)
library(statnet)
net <- as.network(df, bipartite = 153, directed = FALSE)
net%v%'org_nat'<- nodelist$Country
net%v%'org_fam' <- nodelist$Parfam
net%v%'org_type' <- nodelist$Type

#Examine edges term ERGM

summary(net)
m1 <- ergm(net ~ edges)
summary(m1)
plogis(coef(m1)[['edges']])

# b1factor, removing the NA node attribute.
m2 <- ergm(net ~ edges + b1factor("org_fam",levels=c(1:10)))

# Convert coefficient to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

theta <- m2$coef
logit2prob(theta)


# I still need to amend the graph and create only a nationalised network.
# I need to change base to party family considered 'most Eurepeanised'?
m3 <- ergm(net ~ edges + b1factor("org_nat"))
summary(m3)
theta <- m3$coef
logit2prob(theta)

#Tomorrow I add act_type to see which actors have biggest influence
# on nationalised network
m4 <- ergm(net ~ edges +
             b1factor("org_fam"))
theta <- m4$coef
logit2prob(theta)

