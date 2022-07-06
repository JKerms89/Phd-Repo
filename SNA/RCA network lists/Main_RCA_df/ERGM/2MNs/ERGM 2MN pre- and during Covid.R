#################2-mode network ERGM according to PERIOD #######################
#First, pre-covid network

library(statnet)
library(igraph)
library(readxl)
library(dplyr)
library(tidyr)
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
RCA <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- RCA %>% filter(!is.na(act2obj) & period == 'pre-Covid')
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
write.table(edgelist, 'edgelist2MN_pre-covid.csv', row.names=FALSE, col.names=FALSE, sep=",")
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
df <- read.csv("edgelist2MN_pre-covid.csv",header = FALSE)
mode1 <- unique(df[,1])
mode2 <- unique(df[,2])
length(mode2)
library(statnet)
precovnet <- as.network(df, bipartite = 249, directed = FALSE)
precovnet%v%'org_nat'<- nodelist$Country
precovnet%v%'org_type' <- nodelist$Type
precovnet%v%'org_fam' <- nodelist$Parfam
precovnet%v%'org_scope' <- nodelist$Scope


############ Now, for the during Covid network##################

RCA <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- RCA %>% filter(!is.na(act2obj) & period == 'Covid')
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
write.table(edgelist, 'edgelist2MN_covid.csv', row.names=FALSE, col.names=FALSE, sep=",")
g2 <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=FALSE))
g2 <- graph.adjacency(g2, mode = "undirected", weighted=T)

#Because igraph does not automatically recognize two-mode networks, it is necessary to tell igraph that there are two types of vertices. 
bipartite.mapping(g2)
V(g2)$type <- bipartite_mapping(g2)$type

#add degree attribute
V(g2)$degree <- degree (g2, v= V(g2), mode = 'all')  
table(V(g2)$degree)
summary(g2)

#Add node attributes

V(g2)$org_nat <- nodelist$Country[match(V(g2)$name, nodelist$Name)]
V(g2)$org_type <- nodelist$Type[match(V(g2)$name, nodelist$Name)]
V(g2)$org_parfam <- nodelist$Parfam[match(V(g2)$name, nodelist$Name)]

#Convert to statnet graph
library(statnet)
library(intergraph)
g3 <- asNetwork(g2)

# OR, create a statnet object from the outset.Here is how to do it.
# first, import the 2MO edgelist csv file (refer to the previous code above for the
# nodelist)
df <- read.csv("edgelist2MN_covid.csv",header = FALSE)
mode1 <- unique(df[,1])
mode2 <- unique(df[,2])
length(mode2)
library(statnet)
covnet <- as.network(df, bipartite = 230, directed = FALSE)
covnet%v%'org_nat'<- nodelist$Country
covnet%v%'org_type' <- nodelist$Type
covnet%v%'org_fam' <- nodelist$Parfam
covnet%v%'org_scope' <- nodelist$Scope

# Convert coefficient(s) to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# Now create ERGM and compare scope according to period
# First compare 'domestication' probability using ERGM

m1 <- ergm(precovnet ~ nodemix('org_scope')+ edges)
summary(m1)
theta_m1 <- m1$coef 
logit2prob(theta_m1)

m2 <- ergm(covnet ~  nodemix('org_scope')+  edges)
summary(m2)
theta_m2 <- m2$coef
logit2prob(theta_m2)

library(texreg)
screenreg(list(m1,m2), custom.model.names = c("pre-Covid", "during Covid"), file = "ERGM_2MN_period.doc")


###################b2factor############


m1 <- ergm(precovnet ~ b1factor('org_scope') +
             b2factor('org_scope') + edges)
summary(m1)
theta_m1 <- m1$coef 
logit2prob(theta_m1)

m2 <- ergm(covnet ~  b1factor('org_scope') +
             b2factor('org_scope') + edges)
summary(m2)
theta_m2 <- m2$coef
logit2prob(theta_m2)

library(texreg)
screenreg(list(m1,m2), custom.model.names = c("pre-Covid", "during Covid"))


##### homophily, I expect to increase during the pandemic. Probability of claimant invoking national
# object increases by 10 per cent (34 to 43%) during the pandemic ########

m1 <- ergm(precovnet ~ nodematch('org_nat') + edges)
summary(m1)
theta_m1 <- m1$coef 
logit2prob(theta_m1)

m2 <- ergm(covnet ~  nodematch('org_nat') + edges)
summary(m2)
theta_m2 <- m2$coef
logit2prob(theta_m2)

library(texreg)
screenreg(list(m1,m2), custom.model.names = c("pre-Covid", "during Covid"))

#Gov/exec actors are less likely to make discursive appeals to identity during 
#the crisis and political parties/legislative actors make more identity claims.

m13 <- ergm(precovnet ~  b1factor('org_type', levels =c("government/executive", "other state executive agencies", "politicians", "political parties", "legislative", "central banks")) + edges)
theta_m13 <- m13$coef
logit2prob(theta_m13)

m14 <- ergm(covnet ~  b1factor('org_type', levels =c("government/executive", "other state executive agencies", "politicians", "political parties", "legislative", "central banks")) + edges)
theta_m14 <- m14$coef
logit2prob(theta_m14)

screenreg(list(m13,m14),custom.model.names = c("pre-Covid", "during Covid"))

########nodematch org_nat ##########

m9 <- ergm(precovnet ~ nodematch('org_nat', diff = TRUE, levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) + 
             b1factor('org_nat', levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) + 
             b2factor('org_nat', levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) + 
             edges)
summary(m9)
theta_m9 <- m9$coef
logit2prob(theta_m9)


m10 <- ergm(covnet ~ nodematch('org_nat', diff = TRUE, levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) + 
              b1factor('org_nat', levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) + 
             b2factor('org_nat', levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) + 
             edges)
summary(m10)
theta_m10 <- m10$coef
logit2prob(theta_m10)

screenreg(list(m9,m10),custom.model.names = c("pre-Covid", "during Covid"))

# In sum, the ERGM model shows that networks become more self-referential during the pandemic as homophily increases
# and the probability of discursive ties toward 'own country, national' increases by 10 per cent. 

############# ERGM tutorial ###################
# https://www.youtube.com/watch?v=1a0H9vnJMZs # 

summary(covnet ~ edges )

m1 = ergm(covnet ~ edges )
summary(m1)
theta_m1 <- m1$coef
logit2prob(theta_m1)

m2 = ergm(precovnet ~ edges )
summary(m2)
theta_m2 <- m2$coef
logit2prob(theta_m2)

# The probability of the same tie being present is circa 2% which is the density of the graph.
# Let's calculate homophily re org scope

fauxmodel.01 <- ergm(precovnet ~ edges + 
                       b2factor('org_scope') + nodematch('org_scope',diff=T)) 

summary(fauxmodel.01)
theta_m <- fauxmodel.01$coef
logit2prob(theta_m)

# Here we can see that there is only a 55% chance of actors referring to their own country.

fauxmodel.02 <- ergm(covnet ~ edges + 
                       b2factor('org_scope') + nodematch('org_scope',diff=T)) 

summary(fauxmodel.02)
theta_m2 <- fauxmodel.02$coef
logit2prob(theta_m2)

# Here we can see that actors are more self-referential during the pandemic. 











