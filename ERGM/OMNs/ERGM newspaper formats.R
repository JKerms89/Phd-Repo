### Create ERGM Model(s) to compare quality/tabloid newspapers re 'domestication' of EU affairs
### OMN ###
### First, import and create network graph of 'quality newspaper' network

library(dplyr)
library(statnet)
library(igraph)
library(readxl)
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
df <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- df %>% filter(!is.na(act2adr) & newsp_type == 'quality')
edgelist<-data.frame(RCA$actorg,RCA$adrorg)
edgelist<-na.omit(edgelist)
natlist<-data.frame(RCA$actorg,RCA$act_nat, RCA$act_type, RCA$act_scope_RECON)
natlist2<-data.frame(RCA$adrorg,RCA$adr_nat, RCA$adr_type, RCA$adr_scope_RECON)
colnames(natlist)<-c('Name','Country','Type', 'Scope')
colnames(natlist2)<-c('Name','Country','Type', 'Scope')
natlist<-rbind(natlist,natlist2)
natlist<-unique(natlist)
colnames(edgelist)<-c('Claimant','Addressee')
edgematrix2 <- t(matrix(edgelist,nrow=2))
g <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=TRUE))
g <- graph.adjacency(g, mode = "directed", weighted=T)

# add node attributes

V(g)$act_nat <- natlist$Country[match(V(g)$name, natlist$Name)]
V(g)$act_type <- natlist$Type[match(V(g)$name, natlist$Name)]
V(g)$act_scope <- natlist$Scope[match(V(g)$name, natlist$Name)]

### Secondly, import and create network graph of 'tabloid newspaper' network

setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
df <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- df %>% filter(!is.na(act2adr) & newsp_type == 'tabloid / regional')
edgelist<-data.frame(RCA$actorg,RCA$adrorg)
edgelist<-na.omit(edgelist)
natlist<-data.frame(RCA$actorg,RCA$act_nat, RCA$act_type, RCA$act_scope_RECON)
natlist2<-data.frame(RCA$adrorg,RCA$adr_nat, RCA$adr_type, RCA$adr_scope_RECON)
colnames(natlist)<-c('Name','Country','Type', 'Scope')
colnames(natlist2)<-c('Name','Country','Type', 'Scope')
natlist<-rbind(natlist,natlist2)
natlist<-unique(natlist)
colnames(edgelist)<-c('Claimant','Addressee')
edgematrix2 <- t(matrix(edgelist,nrow=2))
g2 <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=TRUE))
g2 <- graph.adjacency(g2, mode = "directed", weighted=T)

# add node attributes

V(g2)$act_nat <- natlist$Country[match(V(g2)$name, natlist$Name)]
V(g2)$act_type <- natlist$Type[match(V(g2)$name, natlist$Name)]
V(g2)$act_scope <- natlist$Scope[match(V(g2)$name, natlist$Name)]

# Convert tab and quality nets to statnet object

library(statnet)
library(intergraph)
net1 <- asNetwork(g)
net2 <- asNetwork(g2)

# Convert coefficient(s) to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# First compare 'domestication' probability using ERGM
m1 <- ergm(net1 ~ nodematch('act_scope') +  nodeofactor ('act_scope') +  edges)
summary(m1)
theta_m1 <- m1$coef 
logit2prob(theta_m1)

m2 <- ergm(net2 ~ nodematch('act_scope') + nodeofactor ('act_scope') +  edges)
summary(m2)
theta_m2 <- m2$coef
logit2prob(theta_m2)

# Here we can see that tabloid newspapers are more self-referential compared to quality (56% to 41% probabilites respectively).
# On the face of it, there does not seem to be much difference when you compare nodefactor generally. But
# when you distinguish between in- and out-ties, the tabloid newspapers are more domesticated. 

# Interestingly, as out-degree probability of tie, domestic actors are more likely to
# form ties in network as the claimant by a ratio of 4:6. That is, in 6 ties, 4 of them 
# are likely to come from domestic actors, and 2 will come from EU-level actors.

m3 <- ergm(net1 ~ nodeifactor ('act_scope') +  edges)
summary(m3)
theta_m3 <- m3$coef
logit2prob(theta_m3)

m4 <- ergm(net2 ~ nodeifactor ('act_scope') +  edges)
summary(m4)
theta_m4 <- m4$coef
logit2prob(theta_m4)

# From the above, we can see that the probability of EU actor being addressee
# impassive actor is much higher vis-a-vis other covariates.
# Lets compare according to nationality now.

m5 <- ergm(net1 ~ nodematch('act_nat', levels = c(20,23,32,43,46)) + nodeofactor('act_nat', levels = c(20,23,32,43,46)) +  edges)
summary(m5)
theta_m5 <- m5$coef
logit2prob(theta_m5)

m6 <- ergm(net2 ~ nodematch('act_nat', levels = c(10,12,17,19,20)) + nodeofactor('act_nat', levels = c(10,12,17,19,20)) +  edges)
summary(m6)
theta_m6 <- m6$coef
logit2prob(theta_m6)

# The differences are striking. In the tabloid newspapers, national agency is more
# prevalent vis-a-vis EU-level actors. However, the contrary is true in the case 
# of quality newspapers. 

m7 <- ergm(net1 ~ nodematch('act_nat', levels = c(20,23,32,43,46)) + nodefactor('act_nat', levels = c(20,23,32,43,46)) +  edges)
summary(m7)
theta_m7 <- m7$coef
logit2prob(theta_m7)

m8 <- ergm(net2 ~ nodematch('act_nat', levels = c(10,12,17,19,20)) + nodefactor('act_nat', levels = c(10,12,17,19,20)) +  edges)
summary(m8)
theta_m8 <- m8$coef
logit2prob(theta_m8)

# But differences are minimal if just looking at nodefactor in general. What we
# can take from this analysis is that national actors are more commonly the 
# protagonists of political demands and EU actors are more commonly the targets of 
# political demands whereas the converse is true in the case of quality newspapers (which are more balanced).
# This implies already a tacit bias in news selection (a media logic). Although there 
# are no extreme differences between them (however there is a significant correlation).

m9 <- ergm(net1 ~ nodematch('act_nat', levels = c(20,23,32,43,46)) + nodeifactor('act_nat', levels = c(20,23,32,43,46)) +  edges)
summary(m9)
theta_m9 <- m9$coef
logit2prob(theta_m9)

m10 <- ergm(net2 ~ nodematch('act_nat', levels = c(10,12,17,19,20)) + nodeifactor('act_nat', levels = c(10,12,17,19,20)) +  edges)
summary(m10)
theta_m10 <- m10$coef
logit2prob(theta_m10)

# In conclusion, in terms of visibility (quantitative), there is evidence of 
# Europeanisation. However, qualitatively, there is evidence of homophily in terms of nationality,
# and actors sharing same nationality tend to keep within their groups. There is not much evidence
# of transnational communicative linkages given highly significant no. of nodematch.act_nat variable. 





#FINAL COMPARISON ERGM b/w formats

m1 <- ergm(net1 ~ nodematch('act_scope') +  nodeofactor ('act_scope') + nodematch('act_nat') + nodematch('act_type') + nodemix('act_scope', levels = c('Own Country, National', 'EU Supranational', 'Other EU Member State' )) + edges)
summary(m1)
theta_m1 <- m1$coef 
logit2prob(theta_m1)

m2 <- ergm(net2 ~ nodematch('act_scope') +  nodeofactor ('act_scope') + nodematch('act_nat') + nodematch('act_type') + nodemix('act_scope', levels = c('Own Country, National', 'EU Supranational', 'Other EU Member State' )) + edges)
summary(m2)
theta_m2 <- m1$coef 
logit2prob(theta_m2)

library(texreg)
screenreg(list(m1,m2), custom.model.names = c("quality", "tabloid"), file = "ERGM_newsp_type.doc")




###### Testing the nodemix term

m4 <- ergm(net1 ~ nodefactor('act_scope') + nodemix('act_scope', levels = c('Own Country, National', 'EU Supranational', 'Other EU Member State' )) + edges)
summary(m4)
theta_m4 <- m4$coef 
logit2prob(theta_m4)
