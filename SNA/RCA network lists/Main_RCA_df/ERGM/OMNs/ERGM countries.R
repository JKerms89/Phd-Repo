### Create ERGM Model(s) to carry out across-country comparisons 'domestication' of EU affairs
### OMN ###
### First, import and create network graph of 'PL' network
# Create a nationalised network (act2adr = euronpean) and examine correlations
# between different variables (OMN)
library(statnet)
library(igraph)
library(readxl)
library(dplyr)
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
df <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- df %>% filter(!is.na(act2adr) & source_country == 'POLAND')
edgelist<-data.frame(RCA$actorg,RCA$adrorg)
edgelist<-na.omit(edgelist)
natlist<-data.frame(RCA$actorg,RCA$act_nat, RCA$act_type, RCA$act_scope, RCA$act_scope_RECON, RCA$parfam)
natlist2<-data.frame(RCA$adrorg,RCA$adr_nat, RCA$adr_type, RCA$adr_scope,RCA$adr_scope_RECON, RCA$parfam)
colnames(natlist)<-c('Name','Country','Type', 'Scope', 'Scope_newsp', 'Parfam')
colnames(natlist2)<-c('Name','Country','Type', 'Scope','Scope_newsp','Parfam')
natlist<-rbind(natlist,natlist2)
natlist<-unique(natlist)
natlist$Parfam[(is.na(natlist$Parfam))] <- "Not applicable"
colnames(edgelist)<-c('Claimant','Addressee')
edgematrix2 <- t(matrix(edgelist,nrow=2))
g <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=TRUE))
g <- graph.adjacency(g, mode = "directed", weighted=T)

# add node attributes

V(g)$act_nat <- natlist$Country[match(V(g)$name, natlist$Name)]
V(g)$act_type <- natlist$Type[match(V(g)$name, natlist$Name)]
V(g)$act_scope2 <- natlist$Scope_newsp[match(V(g)$name, natlist$Name)]
V(g)$act_scope <- natlist$Scope[match(V(g)$name, natlist$Name)]
V(g)$par_fam <- natlist$Parfam[match(V(g)$name, natlist$Name)]

### Import and create network graph of 'IT' network

RCA <- df %>% filter(!is.na(act2adr) & source_country == 'ITALY')
edgelist<-data.frame(RCA$actorg,RCA$adrorg)
edgelist<-na.omit(edgelist)
natlist<-data.frame(RCA$actorg,RCA$act_nat, RCA$act_type, RCA$act_scope, RCA$act_scope_RECON, RCA$parfam)
natlist2<-data.frame(RCA$adrorg,RCA$adr_nat, RCA$adr_type, RCA$adr_scope,RCA$adr_scope_RECON, RCA$parfam)
colnames(natlist)<-c('Name','Country','Type', 'Scope', 'Scope_newsp', 'Parfam')
colnames(natlist2)<-c('Name','Country','Type', 'Scope','Scope_newsp','Parfam')
natlist<-rbind(natlist,natlist2)
natlist<-unique(natlist)
natlist$Parfam[(is.na(natlist$Parfam))] <- "Not applicable"
colnames(edgelist)<-c('Claimant','Addressee')
edgematrix2 <- t(matrix(edgelist,nrow=2))
g2 <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=TRUE))
g2 <- graph.adjacency(g2, mode = "directed", weighted=T)

# add node attributes

V(g2)$act_nat <- natlist$Country[match(V(g2)$name, natlist$Name)]
V(g2)$act_type <- natlist$Type[match(V(g2)$name, natlist$Name)]
V(g2)$act_scope2 <- natlist$Scope_newsp[match(V(g2)$name, natlist$Name)]
V(g2)$act_scope <- natlist$Scope[match(V(g2)$name, natlist$Name)]
V(g2)$par_fam <- natlist$Parfam[match(V(g2)$name, natlist$Name)]


### Import and create network graph of 'NL' network
RCA <- df %>% filter(!is.na(act2adr) & source_country == 'NETHERLANDS')
edgelist<-data.frame(RCA$actorg,RCA$adrorg)
edgelist<-na.omit(edgelist)
natlist<-data.frame(RCA$actorg,RCA$act_nat, RCA$act_type, RCA$act_scope, RCA$act_scope_RECON, RCA$parfam)
natlist2<-data.frame(RCA$adrorg,RCA$adr_nat, RCA$adr_type, RCA$adr_scope,RCA$adr_scope_RECON, RCA$parfam)
colnames(natlist)<-c('Name','Country','Type', 'Scope', 'Scope_newsp', 'Parfam')
colnames(natlist2)<-c('Name','Country','Type', 'Scope','Scope_newsp','Parfam')
natlist<-rbind(natlist,natlist2)
natlist<-unique(natlist)
natlist$Parfam[(is.na(natlist$Parfam))] <- "Not applicable"
colnames(edgelist)<-c('Claimant','Addressee')
edgematrix2 <- t(matrix(edgelist,nrow=2))
g3 <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=TRUE))
g3 <- graph.adjacency(g3, mode = "directed", weighted=T)

# add node attributes

V(g3)$act_nat <- natlist$Country[match(V(g3)$name, natlist$Name)]
V(g3)$act_type <- natlist$Type[match(V(g3)$name, natlist$Name)]
V(g3)$act_scope2 <- natlist$Scope_newsp[match(V(g3)$name, natlist$Name)]
V(g3)$act_scope <- natlist$Scope[match(V(g3)$name, natlist$Name)]
V(g3)$par_fam <- natlist$Parfam[match(V(g3)$name, natlist$Name)]

### Import and create network graph of 'DE' network

RCA <- df %>% filter(!is.na(act2adr) & source_country == 'GERMANY')
edgelist<-data.frame(RCA$actorg,RCA$adrorg)
edgelist<-na.omit(edgelist)
natlist<-data.frame(RCA$actorg,RCA$act_nat, RCA$act_type, RCA$act_scope, RCA$act_scope_RECON, RCA$parfam)
natlist2<-data.frame(RCA$adrorg,RCA$adr_nat, RCA$adr_type, RCA$adr_scope,RCA$adr_scope_RECON, RCA$parfam)
colnames(natlist)<-c('Name','Country','Type', 'Scope', 'Scope_newsp', 'Parfam')
colnames(natlist2)<-c('Name','Country','Type', 'Scope','Scope_newsp','Parfam')
natlist<-rbind(natlist,natlist2)
natlist<-unique(natlist)
natlist$Parfam[(is.na(natlist$Parfam))] <- "Not applicable"
colnames(edgelist)<-c('Claimant','Addressee')
edgematrix2 <- t(matrix(edgelist,nrow=2))
g4 <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=TRUE))
g4<- graph.adjacency(g4, mode = "directed", weighted=T)

# add node attributes

V(g4)$act_nat <- natlist$Country[match(V(g4)$name, natlist$Name)]
V(g4)$act_type <- natlist$Type[match(V(g4)$name, natlist$Name)]
V(g4)$act_scope2 <- natlist$Scope_newsp[match(V(g4)$name, natlist$Name)]
V(g4)$act_scope <- natlist$Scope[match(V(g4)$name, natlist$Name)]
V(g4)$par_fam <- natlist$Parfam[match(V(g4)$name, natlist$Name)]

#Convert to statnet graph
library(statnet)
library(intergraph)
PLnet <- asNetwork(g)
ITnet <- asNetwork(g2)
NLnet <- asNetwork(g3)
DEnet <- asNetwork(g4)

# Convert coefficient(s) to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#Compare act_scopes across country nets. I expect PL and IT nets to
# be the most 'domesticated'.
# First compare 'domestication' probability using ERGM

PLscope <- ergm(PLnet ~ nodematch('act_scope', diff = TRUE) +  nodefactor ('act_scope', base = 2) +  edges)
summary(PLscope)
theta_PL <- PLscope$coef 
logit2prob(theta_PL)

ITscope  <- ergm(ITnet ~ nodematch('act_scope', diff = TRUE) +  nodefactor ('act_scope', base = 2) +  edges)
summary(ITscope)
theta_IT <- ITscope$coef 
logit2prob(theta_IT)

NLscope <- ergm(NLnet ~ nodematch('act_scope', diff = TRUE) +  nodefactor ('act_scope', base = 2) +  edges)
summary(NLscope)
theta_NL <- NLscope$coef 
logit2prob(theta_NL)

DEscope <- ergm(DEnet ~ nodematch('act_scope', diff = TRUE) +  nodefactor ('act_scope', base = 2) +  edges)
theta_DE <- DEscope$coef 
logit2prob(theta_DE)

library(texreg)
screenreg(list(PLscope, ITscope, NLscope, DEscope), custom.model.names = c("Poland", "Italy", "Netherlands", "Germany"))

# Lets compare act_nat homophily and calculate probability of domestic actor tie

plnat <- ergm(PLnet ~ nodematch('act_nat', diff = TRUE, levels = c("EUROPEAN UNION","POLAND", "FRANCE")) + nodeofactor('act_nat', levels = c("EUROPEAN UNION", "POLAND", "FRANCE"))
+  edges)
summary(plnat)
theta_PL <- plnat$coef 
logit2prob(theta_PL)


itnat <- ergm(ITnet ~ nodematch('act_nat', diff = TRUE, levels = c("EUROPEAN UNION","ITALY", "FRANCE")) + nodeofactor('act_nat', levels = c("EUROPEAN UNION", "ITALY", "FRANCE"))
              +  edges)
summary(itnat)
theta_IT <- itnat$coef 
logit2prob(theta_IT)

nlnat <- ergm(NLnet ~ nodematch('act_nat', diff = TRUE, levels = c("EUROPEAN UNION", "NETHERLANDS", "FRANCE")) + nodeofactor('act_nat', levels = c("EUROPEAN UNION", "NETHERLANDS", "FRANCE"))
              +  edges)
summary(nlnat)
theta_NL <- nlnat$coef 
logit2prob(theta_NL)

denat <- ergm(DEnet ~ nodematch('act_nat', diff = TRUE, levels = c("EUROPEAN UNION", "GERMANY", "FRANCE")) + nodeofactor('act_nat', levels = c("EUROPEAN UNION", "GERMANY", "FRANCE"))
              +  edges)
summary(denat)
theta_DE <- denat$coef 
logit2prob(theta_DE)

library(texreg)
screenreg(list(plnat, itnat, nlnat, denat), custom.model.names = c("Poland", "Italy", "Netherlands", "Germany"))


# Now w/o the factor variable

plnat <- ergm(PLnet ~ nodematch('act_nat', diff = TRUE, levels = c("EUROPEAN UNION","POLAND")) +  edges)
summary(plnat)
theta_PL <- plnat$coef 
logit2prob(theta_PL)


itnat <- ergm(ITnet ~ nodematch('act_nat', diff = TRUE, levels = c("EUROPEAN UNION","ITALY")) +  edges)
summary(itnat)
theta_IT <- itnat$coef 
logit2prob(theta_IT)

nlnat <- ergm(NLnet ~ nodematch('act_nat', diff = TRUE, levels = c("EUROPEAN UNION", "NETHERLANDS")) +  edges)
summary(nlnat)
theta_NL <- nlnat$coef 
logit2prob(theta_NL)

denat <- ergm(DEnet ~ nodematch('act_nat', diff = TRUE, levels = c("EUROPEAN UNION", "GERMANY")) +  edges)
summary(denat)
theta_DE <- denat$coef 
logit2prob(theta_DE)

library(texreg)
screenreg(list(plnat, itnat, nlnat, denat), custom.model.names = c("Poland", "Italy", "Netherlands", "Germany"), file = "homophilybycountry.doc")















######## Probability of tie by nationality (EU or country?)
PLnatf <- ergm(PLnet ~ nodefactor ('act_nat', levels = c("EUROPEAN UNION","POLAND")) + nodemix('act_nat', levels = c("EUROPEAN UNION","POLAND")) + edges)
summary(PLnatf)
theta_PL <- PLnatf$coef 
logit2prob(theta_PL)

ITnatf  <- ergm(ITnet ~ nodefactor('act_nat', levels = c("EUROPEAN UNION","ITALY")) + nodemix('act_nat', levels = c("EUROPEAN UNION","ITALY")) + edges)
summary(ITnatf)
theta_IT <- ITnatf$coef 
logit2prob(theta_IT)

NLnatf <- ergm(NLnet ~ nodefactor('act_nat', levels = c("EUROPEAN UNION","NETHERLANDS")) + nodemix('act_nat', levels = c("EUROPEAN UNION","NETHERLANDS")) +  edges)
summary(NLnatf)
theta_NL <- NLnatf$coef 
logit2prob(theta_NL)

DEnatf <- ergm(DEnet ~ nodefactor('act_nat', levels = c("EUROPEAN UNION","GERMANY")) + nodemix('act_nat', levels = c("EUROPEAN UNION","GERMANY")) +  edges)
theta_DE <- DEnatf$coef 
logit2prob(theta_DE)

library(texreg)
screenreg(list(PLnatf, ITnatf, NLnatf, DEnatf), custom.model.names = c("Poland", "Italy", "Netherlands", "Germany"), file = "nodefactorbycountry.doc")










## Now examine factor by actortype

PLactf <- ergm(PLnet ~ nodefactor ('act_type') +   edges)
summary(PLactf)
theta_PL <- PLactf$coef 
logit2prob(theta_PL)

ITactf  <- ergm(ITnet ~ nodefactor('act_type') +  edges)
summary(ITactf)
theta_IT <- ITactf$coef 
logit2prob(theta_IT)

NLactf <- ergm(NLnet ~ nodefactor('act_type') +  edges)
summary(NLactf)
theta_NL <- NLactf$coef 
logit2prob(theta_NL)

DEactf <- ergm(DEnet ~ nodefactor('act_type')+  edges)
theta_DE <- DEactf$coef 
logit2prob(theta_DE)

library(texreg)
screenreg(list(PLactf, ITactf, NLactf, DEactf), custom.model.names = c("Poland", "Italy", "Netherlands", "Germany"), file = "nodefactorbyacttype.doc")
















######## Examine probability of tie based on node scope (a la RECONNECT) cross-
# country comparison

PLscope <- ergm(PLnet ~ nodefactor ('act_scope2') + nodemix('act_scope2', levels = c('Own Country, National', 'EU Supranational', 'Other EU Member State' )) + edges)
summary(PLscope)
theta_PL <- PLscope$coef 
logit2prob(theta_PL)

ITscope  <- ergm(ITnet ~ nodefactor ('act_scope2') + nodemix('act_scope2', levels = c('Own Country, National', 'EU Supranational', 'Other EU Member State' )) +  edges)
summary(ITscope)
theta_IT <- ITscope$coef 
logit2prob(theta_IT)

NLscope <- ergm(NLnet ~nodefactor ('act_scope2') + nodemix('act_scope2', levels = c('Own Country, National', 'EU Supranational', 'Other EU Member State' ))+  edges)
summary(NLscope)
theta_NL <- NLscope$coef 
logit2prob(theta_NL)

DEscope <- ergm(DEnet ~  nodefactor ('act_scope2') + nodemix('act_scope2', levels = c('Own Country, National', 'EU Supranational', 'Other EU Member State' ))+  edges)
theta_DE <- DEscope$coef 
logit2prob(theta_DE)

library(texreg)
screenreg(list(PLscope, ITscope, NLscope, DEscope), custom.model.names = c("Poland", "Italy", "Netherlands", "Germany"), file = 'ERGMactscope.doc')












