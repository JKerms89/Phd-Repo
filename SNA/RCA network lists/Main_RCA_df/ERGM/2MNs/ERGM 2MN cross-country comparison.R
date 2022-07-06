#################2-mode network ERGM according to COUNTRY#######################
library(statnet)
library(igraph)
library(readxl)
library(dplyr)
library(texreg)
library(tidyr)
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
RCA <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- RCA %>% filter(!is.na(act2obj) & source_country == 'POLAND')
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
write.table(edgelist, 'edgelist2MN.csv', row.names=FALSE, col.names=FALSE, sep=",")
g <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=FALSE))
g <- graph.adjacency(g, mode = "undirected", weighted=T)

#Because igraph does not automatically recognize two-mode networks, it is necessary to tell igraph that there are two types of vertices. 
bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type

#add degree attribute
V(g)$degree <- degree (g, v= V(g), mode = 'all')  
table(V(g)$degree)
summary(g)

#Add node attributes

V(g)$org_nat <- nodelist$Country[match(V(g)$name, nodelist$Name)]
V(g)$org_type <- nodelist$Type[match(V(g)$name, nodelist$Name)]
V(g)$org_parfam <- nodelist$Parfam[match(V(g)$name, nodelist$Name)]

#Convert to statnet graph
library(statnet)
library(intergraph)
g2 <- asNetwork(g)

# OR, create a statnet object from the outset.Here is how to do it.
# first, import the 2MO edgelist csv file (refer to the previous code above for the
# nodelist)
df <- read.csv("edgelist2MN.csv",header = FALSE)
mode1 <- unique(df[,1])
mode2 <- unique(df[,2])
length(mode2)
library(statnet)
netPL <- as.network(df, bipartite = 155, directed = FALSE)
netPL%v%'org_nat'<- nodelist$Country
netPL%v%'org_type' <- nodelist$Type
netPL%v%'org_fam' <- nodelist$Parfam
netPL%v%'org_scope' <- nodelist$Scope


###################### GERMANY 2MN #######################
RCA <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- RCA %>% filter(!is.na(act2obj) & source_country == 'GERMANY')
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
write.table(edgelist, 'edgelist2MN.csv', row.names=FALSE, col.names=FALSE, sep=",")
g <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=FALSE))
g <- graph.adjacency(g, mode = "undirected", weighted=T)

#Because igraph does not automatically recognize two-mode networks, it is necessary to tell igraph that there are two types of vertices. 
bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type

#add degree attribute
V(g)$degree <- degree (g, v= V(g), mode = 'all')  
table(V(g)$degree)
summary(g)

#Convert to statnet graph
library(statnet)
library(intergraph)
g2 <- asNetwork(g)
# OR, create a statnet object from the outset.Here is how to do it.
# first, import the 2MO edgelist csv file (refer to the previous code above for the
# nodelist)
df <- read.csv("edgelist2MN.csv",header = FALSE)
mode1 <- unique(df[,1])
mode2 <- unique(df[,2])
length(mode2)
library(statnet)
netDE <- as.network(df, bipartite = 140, directed = FALSE)
netDE%v%'org_nat'<- nodelist$Country
netDE%v%'org_type' <- nodelist$Type
netDE%v%'org_fam' <- nodelist$Parfam
netDE%v%'org_scope' <- nodelist$Scope


###################### ITALY 2MN #######################
RCA <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- RCA %>% filter(!is.na(act2obj) & source_country == 'ITALY')
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
write.table(edgelist, 'edgelist2MN.csv', row.names=FALSE, col.names=FALSE, sep=",")
g <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=FALSE))
g <- graph.adjacency(g, mode = "undirected", weighted=T)

#Because igraph does not automatically recognize two-mode networks, it is necessary to tell igraph that there are two types of vertices. 
bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type

#add degree attribute
V(g)$degree <- degree (g, v= V(g), mode = 'all')  
table(V(g)$degree)
summary(g)

#Convert to statnet graph
library(statnet)
library(intergraph)
g2 <- asNetwork(g)
# OR, create a statnet object from the outset.Here is how to do it.
# first, import the 2MO edgelist csv file (refer to the previous code above for the
# nodelist)
df <- read.csv("edgelist2MN.csv",header = FALSE)
mode1 <- unique(df[,1])
mode2 <- unique(df[,2])
length(mode2)
library(statnet)
netIT <- as.network(df, bipartite = 134, directed = FALSE)
netIT%v%'org_nat'<- nodelist$Country
netIT%v%'org_type' <- nodelist$Type
netIT%v%'org_fam' <- nodelist$Parfam
netIT%v%'org_scope' <- nodelist$Scope

###################### NETHERLANDS 2MN #######################
RCA <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- RCA %>% filter(!is.na(act2obj) & source_country == 'NETHERLANDS')
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
write.table(edgelist, 'edgelist2MN.csv', row.names=FALSE, col.names=FALSE, sep=",")
g <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=FALSE))
g <- graph.adjacency(g, mode = "undirected", weighted=T)

#Because igraph does not automatically recognize two-mode networks, it is necessary to tell igraph that there are two types of vertices. 
bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type

#add degree attribute
V(g)$degree <- degree (g, v= V(g), mode = 'all')  
table(V(g)$degree)
summary(g)

#Convert to statnet graph
library(statnet)
library(intergraph)
g2 <- asNetwork(g)
# OR, create a statnet object from the outset.Here is how to do it.
# first, import the 2MO edgelist csv file (refer to the previous code above for the
# nodelist)
df <- read.csv("edgelist2MN.csv",header = FALSE)
mode1 <- unique(df[,1])
mode2 <- unique(df[,2])
length(mode2)
library(statnet)
netNL <- as.network(df, bipartite = 100, directed = FALSE)
netNL%v%'org_nat'<- nodelist$Country
netNL%v%'org_type' <- nodelist$Type
netNL%v%'org_fam' <- nodelist$Parfam
netNL%v%'org_scope' <- nodelist$Scope

# Convert coefficient to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# Compare tie-likelihood by org_scope at the 2-mode level. One would expect national scopes
# orgs to dominate. 

scopeNL <-  ergm(netNL ~ nodematch('org_scope', diff = T) + nodematch('org_nat') + 
              b2factor('org_scope', base = 2) + 
              edges)

scopePL <-  ergm(netPL ~ nodematch('org_scope', diff = T) + nodematch('org_nat') +
                   b2factor('org_scope', base = 2) + 
                   edges)

scopeDE <-  ergm(netDE ~ nodematch('org_scope', diff = T) + nodematch('org_nat') +
                   b2factor('org_scope', base = 2) + 
                   edges)

scopeIT <-  ergm(netIT ~ nodematch('org_scope', diff = T) + nodematch('org_nat') +
                   b2factor('org_scope', base = 2) + 
                   edges)



screenreg(list(scopeNL, scopePL, scopeDE, scopeIT), file = 'homophily+scope2MN_countrycomparison.doc')

# Use the node mixing term to explore relations between act_scope variable in more detail...

scopeNL2 <-  ergm(netNL ~ b2factor('org_scope', base = 2) +
                   edges)

scopePL2 <-  ergm(netPL ~ b2factor('org_scope', base = 2) + 
                  edges)

scopeDE2 <-  ergm(netDE ~ b2factor('org_scope', base = 2) + 
                  edges)

scopeIT2 <-  ergm(netIT ~ b2factor('org_scope', base = 2) + 
                  edges)

screenreg(list(scopeNL2, scopePL2, scopeDE2, scopeIT2), file = 'b2factorcountry2MN.doc')



# calculate homophily first of all..

homNL <-  ergm(netNL ~ nodematch('org_nat') + 
                   edges)

homPL <-  ergm(netPL ~  nodematch('org_nat') + 
                   edges)

homDE <-  ergm(netDE ~ nodematch('org_nat') + 
                   edges)

homIT <-  ergm(netIT ~ nodematch('org_nat') + 
                   edges)

screenreg(list(homNL, homPL, homDE, homIT), file = 'homophily2MN.doc')

theta <- homNL$coef
prob <- as.data.frame(logit2prob(theta))
write.xlsx(prob, 'probhomNL.xlsx', row.names = FALSE)

theta2 <- homPL$coef
logit2prob(theta2)

theta3 <- homDE$coef
logit2prob(theta3)

theta4 <- homIT$coef
logit2prob(theta4)

