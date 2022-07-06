
library(statnet)
library(igraph)
library(readxl)
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
RCA <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA18.xlsx")
edgelist<-data.frame(RCA$actorg,RCA$adrorg)
edgelist<-na.omit(edgelist)
natlist<-data.frame(RCA$actorg,RCA$act_nat, RCA$act_type, RCA$act_scope)
natlist2<-data.frame(RCA$adrorg,RCA$adr_nat, RCA$adr_type, RCA$adr_scope)
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

#Great, now that we have the network, we can evaluate homophily.
#Wecan either use igraph's built in function.

df <- data.frame(get.edgelist(g), stringsAsFactors = F)
df$act_nat1 <- as.numeric(factor(natlist$Country[match(df$X1,natlist$Name)]))
df$act_nat2 <- as.numeric(factor(natlist$Country[match(df$X2,natlist$Name)]))
cor.test(df$act_nat1, df$act_nat2)
# cor 
0.3587139 


# Great, now we can use statnet's ergm() function to fit our first ERGM. The only problem? Our network is an igraph object rather than a statnet one. There is some good news though. People have built a package for converting igraph objects to statnet and vise versa - intergraph. Let's install that and load it in too.

install.packages("statnet")
library(statnet)
install.packages("intergraph")
library(intergraph)
g2 <- asNetwork(g)

plot(g2,
     vertex.col = "tomato", 
     vertex.cex = 1)

#Generally, the first term that people use is the edges term. It is a statistic which counts how many edges there are in the network.

random_graph <- ergm(g2 ~ edges)


# do we interpret this coefficient? Coefficients in ERGMs represent the change in the (log-odds) likelihood of a tie for a unit change in a predictor. We can use a simple formula for converting log-odds to probability to understand them better.

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

theta <- random_graph$coef
logit2prob(theta)

#So the probability of an edge being in the graph is roughly 0.001.
# This can also be seen in the network density.

network.density(g2)

summary(random_graph)

# Calculate reciprocity in the graph

random_graph2 <- ergm(g2 ~ edges + mutual)
theta2 <- random_graph2$coef
logit2prob(theta2)

# Calculate probability of cliques in the graph.

random_graph3 <- ergm(g2 ~ edges + triangle)
theta3 <- random_graph3$coef
logit2prob(theta3)

# Calculate homophily based on node attributes

random_graph4 <- ergm(g2 ~ edges +
                                            nodematch("act_nat") +
                                            nodematch("act_type"))
theta4 <- random_graph4$coef
logit2prob(theta4)

# Calculate transitivity



# We can also simulate graphs using our ERGM fit. We did something similar to this when we simulated random graphs and set the probability of an edge being drawn to be equal to our network's density. We just didn't know it at the time!

set.seed(1234)

# Calculate homophily

random_graph4<- ergm(g2 ~ edges +  
                        nodematch("act_nat") +
                        nodematch("act_type"))
theta4<- random_graph4$coef
logit2prob(theta4)

# edges  nodematch.act_nat nodematch.act_type 
#0.0006191615       0.8938959743       0.7155412997. Based on this data, we 
# the % probability of tie when a new edge is added to graph is 0.06%. The
# probability of tie containing nodes of the same nationality is 89%. The
# probability of ties containing nodes of the same actor type is 71%.

# Generate simulation graph

hundred_simulations <- simulate(random_graph, 
                                coef = theta,
                                nsim = 100,
                                control = control.simulate.ergm(MCMC.burnin = 1000,
                                                                MCMC.interval = 1000))

#Every time you are creating plots you might get this error - "Error in plot.new() : figure margins too large". To avoid such errors you can first check par("mar") output. You should be getting:
# 5.1 4.1 4.1 2.1
par("mar")
par(mar=c(1,1,1,1))

# Let's examine the first nine simulations.
par(mfrow = c(2,2))
sapply(hundred_simulations[1:4], plot, vertex.cex = 1, vertex.col = "tomato")

# We can compare the number of edges our observed graph has to the average of the simulated networks.
net_densities <- unlist(lapply(hundred_simulations, network.density))

hist(net_densities, xlab = "Density", main = "", col = "lightgray")
abline(v = network.density(g2), col = "red", lwd = 3, lty = 2)
abline(v = mean(net_densities), col = "blue", lwd = 3, lty = 1)

#Another way to evaluate our model is to use the built-in goodness of fit measures. Essentially, we will evaluate whether our network has similar structural features as the simulated graphs. ergm has a built-in function - gof() - which calculates the statistics for us. We just have to tell it how many simulations we want to use in the comparison set - the larger the number, the more accurate representation of the model.
gof_stats <- gof(random_graph,
                 control.gof.formula(nsim = 100),
                 coef = theta)

par(mfrow = c(2, 3))
plot(gof_stats, main = '')

#First, let's build a model with only dyad-independent terms. Just like with the random graph, we are essentially fitting a logistic regression.

#We can specify the attribute we want to examine as well as the diff argument, which allows for differential levels of homophily for different groups.

model1 <- ergm(g2 ~ edges +
               nodematch("act_nat") +
                 nodematch("act_type"))

summary(model1)
model1$coef

# refer to following link for guide on ERGM: https://bookdown.org/markhoff/social_network_analysis/homophily-and-exponential-random-graphs-ergm.html
# http://statnet.org/Workshops/ergm_tutorial.html

model2 <- ergm(g2 ~ nodematch("act_nat") + 
              nodematch('act_type') + nodefactor('act_type') +
              nodematch('act_scope') + nodefactor("act_scope") +
                mutual +
                edges)
theta <- model2$coef
logit2prob(theta)

# Use texreg to export the ERGM model as a visually friendly graph.
library(texreg)
screenreg(list(model2))
# See the following link for more on how to use the texreg package
#https://rstudio-pubs-static.s3.amazonaws.com/471073_d45a4acd780b4987932dc8fc47c46dd5.html 

model3 <- ergm(g2 ~ nodematch("act_nat") + 
                 nodematch('act_type') + nodeofactor('act_type') +
                 nodeifactor('act_type') +
                 nodematch('act_scope') + nodeofactor("act_scope") +
                 nodeifactor("act_scope") +
                 mutual +
                 edges)
theta <- model3$coef
logit2prob(theta)
library(texreg)
screenreg(list(model3))

# ERGM re mutual 
model4 <- ergm(g2 ~ mutual('act_scope', diff = TRUE) +
                 edges)
theta <- model4$coef
logit2prob(theta)

# ERGM re homophili re actscope 
model5 <- ergm(g2 ~ nodematch('act_scope', diff = TRUE) +
                 edges)
theta <- model5$coef
logit2prob(theta)

# ERGM re homophili re actnat
model6 <- ergm(g2 ~ nodematch('act_nat', diff = TRUE) +
                 edges)
theta <- model6$coef
logit2prob(theta)

# ERGM re homophili re acttype
model7 <- ergm(g2 ~ nodematch('act_type', diff = TRUE) +
                 edges)
theta <- model7$coef
logit2prob(theta)
summary(model7)

# ERGM re nodefactor and acttype. Base = 12 because gov/exec is the base category.
model8 <- ergm(g2 ~ nodeofactor('act_type', base = 12) +
                 edges)
theta <- model8$coef
logit2prob(theta)
summary(model8)

# ERGM re nodefactor and actscope. National actors are baseline.

model9 <- ergm(g2 ~ nodematch('act_scope') +nodefactor('act_scope') +
                 edges)
theta <- model9$coef
logit2prob(theta)

# ERGM selecting only act_nat factor of 4 countries
model9 <- ergm(g2 ~ nodematch('act_nat') + nodefactor('act_nat', levels=c(22,25,34,45,48)) +
                 edges)
summary(model9)

# Full ERGM
model10 <- ergm(g2 ~ nodematch("act_nat") + nodeofactor('act_nat', levels=c(22,25,34,45,48)) +
                 nodeifactor('act_nat', levels=c(22,25,34,45,48)) +
                 nodematch('act_type') + nodeofactor('act_type') +
                 nodeifactor('act_type') +
                 nodematch('act_scope') + nodeofactor("act_scope", levels = c(1:3)) +
                 nodeifactor("act_scope") +
                edges)



theta <- model10$coef
logit2prob(theta)


library(texreg)
screenreg(list(model10), file = "aggnetergm.doc")





