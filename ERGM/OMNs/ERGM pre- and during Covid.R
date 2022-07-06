### Create ERGM Model(s) to compare quality/tabloid newspapers re 'domestication' of EU affairs
### OMN ###
### firstly, import and create network graph of 'pre Covid' network

library(statnet)
library(igraph)
library(readxl)
library(dplyr)
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
df <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- df %>% filter(!is.na(act2adr) & period == 'pre-Covid')
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

### Secondly, import and create network graph of 'during Covid' network

setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
df <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- df %>% filter(!is.na(act2adr) & period == 'Covid')
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

# Convert tab and quality nets to statnet object. 
######N.B: net 1 = pre-covid, and net2 = during covid#####

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
m1 <- ergm(net1 ~ nodefactor ('act_scope', base =2) + nodemix('act_scope', levels = c('Own Country, National', 'EU Supranational', 'Other EU Member State' ))+ edges)
summary(m1)
theta_m1 <- m1$coef 
logit2prob(theta_m1)

m2 <- ergm(net2 ~  nodefactor ('act_scope', base = 2) + nodemix('act_scope', levels = c('Own Country, National', 'EU Supranational', 'Other EU Member State' ))+  edges)
summary(m2)
theta_m2 <- m2$coef
logit2prob(theta_m2)

library(texreg)
screenreg(list(m1,m2), custom.model.names = c("pre-Covid", "during Covid"), file = "ERGM_period.doc")

#(insert comments here)

m3 <- ergm(net1 ~ nodeifactor ('act_scope') +  edges)
summary(m3)
theta_m3 <- m3$coef
logit2prob(theta_m3)

m4 <- ergm(net2 ~ nodeifactor ('act_scope') +  edges)
summary(m4)
theta_m4 <- m4$coef
logit2prob(theta_m4)

# (insert comments here)

m5 <- ergm(net1 ~ nodematch("act_nat", diff = TRUE, levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) + nodeofactor('act_nat', levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) +  edges)
summary(m5)
theta_m5 <- m5$coef
logit2prob(theta_m5)

m6 <- ergm(net2 ~ nodematch('act_nat', diff = TRUE, levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) + nodeofactor('act_nat', levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) +  edges)
summary(m6)
theta_m6 <- m6$coef
logit2prob(theta_m6)

# There is a striking difference in homohphily between pre and during covid. The probability of
# in-group ties pre covid is 77% with addition of new tie, and probabiliy during covid is 92%!
# This shows that visibility of EU actors is high, but transnational communicative linkages decrease (significant effect)
# as actors tend to make more in-group (i.e. same nationality) during the pandemic. 


m7 <- ergm(net1 ~ nodematch('act_nat', diff = TRUE, levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) + nodefactor('act_nat', levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) +  edges)
summary(m7)
theta_m7 <- m7$coef
logit2prob(theta_m7)

m8 <- ergm(net2 ~ nodematch('act_nat', diff = TRUE, levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) + nodefactor('act_nat', levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) +  edges)
summary(m8)
theta_m8 <- m8$coef
logit2prob(theta_m8)

#(insert comments here)

m9 <- ergm(net1 ~ nodematch('act_nat', diff = TRUE, levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) + nodeifactor('act_nat', levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) +  edges)
summary(m9)
theta_m9 <- m9$coef
logit2prob(theta_m9)

m10 <- ergm(net2 ~ nodematch('act_nat', diff = TRUE, levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) + nodeifactor('act_nat', levels = c("EUROPEAN UNION", "GERMANY", "ITALY", "NETHERLANDS", "POLAND")) +  edges)
summary(m10)
theta_m10 <- m10$coef
logit2prob(theta_m10)

# Main takeaway point is homophily increases and this effect is significant from pre and
# during the Covid crisis. Homophily is indicative of 'segmented Europeanisation'- EU actors
# are visible (mainly as passive actors) and in-group tie probabilities (same nationality)
#are higher during crisis (see below).

m11 <- ergm(net1 ~ nodematch('act_nat') + edges)
theta_m11 <- m11$coef
logit2prob(theta_m11)

m12 <- ergm(net2 ~ nodematch('act_nat') + edges)
theta_m12 <- m12$coef
logit2prob(theta_m12)

library(texreg)
screenreg(list(m11,m12))

#Lets see which actor types 'benefit' from crisis...
m13 <- ergm(net1 ~ nodematch('act_type') + nodefactor('act_type', levels =c("government/executive", "other state executive agencies", "politicians", "political parties", "legislative", "central banks")) + edges)
theta_m13 <- m13$coef
logit2prob(theta_m13)

m14 <- ergm(net2 ~ nodematch('act_type') + nodefactor('act_type', levels =c("government/executive", "other state executive agencies", "politicians", "political parties", "legislative", "central banks")) + edges)
theta_m14 <- m14$coef
logit2prob(theta_m14)

screenreg(list(m13,m14))
# There are not any startling results here. Although we can see that gov/exec benefit from
# crises in terms of visibility and politicians/political parties lose out. 


