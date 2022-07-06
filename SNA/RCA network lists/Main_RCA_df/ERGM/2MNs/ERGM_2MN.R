#################2-mode network ERGM#######################
library(statnet)
library(igraph)
library(readxl)
library(dplyr)
library(tidyr)
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
RCA <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- RCA %>% filter(!is.na(act2obj))
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
net <- as.network(df, bipartite = 431, directed = FALSE)
net%v%'org_nat'<- nodelist$Country
net%v%'org_type' <- nodelist$Type
net%v%'org_fam' <- nodelist$Parfam
net%v%'org_scope' <- nodelist$Scope


# Convert coefficient to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


#Generate the edges term. It is a statistic which counts how many edges 
# there are in the network.

summary(g2)
m1 <- ergm(g2 ~ edges)
summary(m1)
plogis(coef(m1)[['edges']])

#See how parfam affects probability of a tie in nationalised discourse network
# All of the model terms we discussed previously deal with the outgoing and incoming connections of the nodes in the network. This following section looks at node covariates as terms in the ERGM equation.
#node covariate: nodefactor
#We are going to explore the differences in the way connections are made 
#between nodes given categorical covariate. 
#We use the nodefactor() term for this.

# The ERGM below calculates homophily by nationality, and likelihood of a b1(actor) tie and b2(constituency) tie in the network. 
#It is a proxy for visibility in the euronet. As expected, the likelihood of a polish object is highest. 

m2 <- ergm(net ~ nodematch('org_nat', diff = F) + b1factor('org_nat', levels = c("EUROPEAN UNION", "POLAND", "ITALY", "GERMANY", "NETHERLANDS")) +
b2factor('org_nat', levels = c("EUROPEAN UNION", "POLAND", "ITALY", "GERMANY", "NETHERLANDS")) + edges) # edges term
summary(m2)

#Lets see which actor types are more likely to make identity scope ties in the network.
m3 <-  ergm(net ~ 
            b1factor('org_nat', levels = c("EUROPEAN UNION", "POLAND", "ITALY", "GERMANY", "NETHERLANDS")) +
             edges)
summary(m3)

# calc. probability that actor refers to own 'national community'
m5 <-  ergm(net ~ 
           nodematch('org_nat') +
           edges)
theta <- m5$coef
logit2prob(theta)
#52% chance that actor invokes national scopes. in other words, 1/2 probability.

# LEts look at the scope of the actor/group. I would expect higher actor/group scope
# homophily and the tie-likelihood of domestic/national scopes to be the highest. Surprisingly,
# homophily is highest in the EU supranational category.

m6 <-  ergm(net ~ 
              nodematch('org_scope', diff = T) +
              edges)
theta <- m6$coef
logit2prob(theta)

# Now lets explore tie-likelihood...

m7 <-  ergm(net ~ 
              b1factor('org_scope', base = 2) +
              b2factor('org_scope', base = 2) + 
              edges)
theta <- m7$coef
logit2prob(theta)
library(texreg)
screenreg(list(m7))






























