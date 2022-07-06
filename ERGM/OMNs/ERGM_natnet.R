# Create a nationalised network (act2adr = national) and examine correlations
# between different variables (OMN)

library(statnet)
library(igraph)
library(readxl)
library(dplyr)
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
df <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- df %>% filter(act2adr == 'national')
edgelist<-data.frame(RCA$actorg,RCA$adrorg)
edgelist<-na.omit(edgelist)
natlist<-data.frame(RCA$actorg,RCA$act_nat, RCA$act_type, RCA$act_scope_RECON, RCA$parfam)
natlist2<-data.frame(RCA$adrorg,RCA$adr_nat, RCA$adr_type, RCA$adr_scope_RECON, RCA$parfam)
colnames(natlist)<-c('Name','Country','Type', 'Scope', 'Parfam')
colnames(natlist2)<-c('Name','Country','Type', 'Scope','Parfam')
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
V(g)$act_scope <- natlist$Scope[match(V(g)$name, natlist$Name)]
V(g)$par_fam <- natlist$Parfam[match(V(g)$name, natlist$Name)]

#Convert to statnet graph
library(statnet)
library(intergraph)
g2 <- asNetwork(g)

plot(g2,
     vertex.col = "tomato", 
     vertex.cex = 1)

# Convert coefficient to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#Generally, the first term that people use is the edges term. It is a statistic which counts how many edges there are in the network.

m1 <- ergm(g2 ~ edges)
summary(m1)
plogis(coef(m1)[['edges']])

#See how parfam affects probability of a tie in nationalised discourse network
# All of the model terms we discussed previously deal with the outgoing and incoming connections of the nodes in the network. This following section looks at node covariates as terms in the ERGM equation.
#node covariate: nodefactor
#We are going to explore the differences in the way connections are made 
#between nodes given categorical covariate. 
#We use the nodefactor() term for this.

m2 <- ergm(g2 ~ nodematch('par_fam') + nodefactor('par_fam', levels = c('1. radical TAN', '3. liberal')) + # Difference in connections conditional on party family. 
                   edges) # edges term

summary(m2)
theta <- m2$coef
logit2prob(theta)

# radical TAN parties are slightly more likely to make a tie in nationalised 
# network than liberal parties. 


#Examine correlation re out-degree
m4 <- as.formula(g2 ~ 
                   nodematch ('par_fam') +
                   nodeofactor('par_fam') + # Difference in connections conditional on party family.
                   mutual + 
                   edges)
m5 <- ergm(m4)

#Note: The use of the nodematch() term should always be done while also 
#including the nodefactor() with the same variable. The reason for this is 
#controlling skewed distributions in your factor variable. As a simple example: 
#consider a network where 90% of the nodes are blue, and 10% are red. Even in 
#the case of a random network, you would expect there to me more ties between
#blue nodes than between blue nodes and red nodes. Adding the nodefactor() term 
#helps control for this overrepresentation of possible ties between nodes that
#share an attribute.

m6 <- as.formula(g2 ~ 
                    nodematch('par_fam', diff = TRUE) + # Making the difference between classes explicit
                    nodefactor('par_fam') + # But also, does class have an effect in the nomination?
                    mutual + 
                    edges)

m7 <- ergm(m6)
summary(m7)



# Another ergm...
m2 <- as.formula(g2 ~ 
                   nodefactor('par_fam') + # Difference in connections conditional on party family.
                   nodematch('par_fam') +
                   edges) # edges term

m3 <- ergm(m2)
summary(m3)


# Convert coefficient to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

theta <- m3$coef
logit2prob(theta)


# Another ergm re act_scope. Unsurprisingly, the probability of national discursive
# tie is much higher vis-a-vis other EU member states. 60% probability of a tie from
# domestic actor via-v-vis other EU member state(effect is significant).

m2 <- as.formula(g2 ~ 
                   nodefactor('act_scope', base = 2) + 
                   edges) # edges term

m3 <- ergm(m2)
summary(m3)
theta <- m3$coef
logit2prob(theta)
















