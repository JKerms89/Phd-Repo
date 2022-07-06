##ERGM on the one-mode italonet###

library(statnet)
library(igraph)
library(readxl)
library(dplyr)
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
RCA <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA17.xlsx")
RCA <- RCA %>% filter(source_country == 'ITALY')
edgelist<-data.frame(RCA$actorg,RCA$adrorg)
edgelist<-na.omit(edgelist)
natlist<-data.frame(RCA$actorg,RCA$act_nat, RCA$act_type, RCA$parfam)
natlist2<-data.frame(RCA$adrorg,RCA$adr_nat, RCA$adr_type, RCA$parfam)
colnames(natlist)<-c('Name','Country','Type', 'Parfam')
colnames(natlist2)<-c('Name','Country','Type', 'Parfam')
natlist<-rbind(natlist,natlist2)
natlist<-unique(natlist)
colnames(edgelist)<-c('Claimant','Addressee')
edgematrix2 <- t(matrix(edgelist,nrow=2))
g <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=TRUE))
g <- graph.adjacency(g, mode = "directed", weighted=T)

# add node attributes

V(g)$act_nat <- natlist$Country[match(V(g)$name, natlist$Name)]
V(g)$act_type <- natlist$Type[match(V(g)$name, natlist$Name)]
V(g)$par_fam <- natlist$Parfam[match(V(g)$name, natlist$Parfam)]


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
#Estimate the simplest model, one with only a term for tie density (akin to an intercept term in a glm):

random_graph <- ergm(g2 ~ edges)

# How do we interpret this coefficient? Coefficients in ERGMs represent the change in the (log-odds) likelihood of a tie for a unit change in a predictor. We can use a simple formula for converting log-odds to probability to understand them better.

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

plogis(coef(random_graph)[['edges']])

#Because that is a dyadic-independent model (the likelihood of a tie doesn't depend on any other), ergm solves the logistic regression instead of resorting to MCMC.
#Note that the edges term represents exactly the density of the network (in log-odds). That is, the probability of any tie (aka the density of the network) is the inverse-logit of the coefficient on edges:

all.equal(network.density(g2), plogis(coef(random_graph)[[1]]))

#Now let's make things more interesting and estimate a term for reciprocity of ties. That is, given an i -> j tie, what is the change in log-odds likelihood of a j -> i tie? The coefficient estimate on mutual tells us exactly that:
# Calculate reciprocity in the graph.

random_graph2 <- ergm(g2 ~ edges + mutual)
theta2 <- random_graph2$coef
logit2prob(theta2)

#Whoa, something different happened there! MCMC happened. 
# ergm went off and did a bunch of simulations to find approximate 
# MLE coefficients. Let's interpret them. The baseline probability of a tie now is:

plogis(coef(random_graph2)[['edges']])

# But if the reciprocal tie is present, then the log odds of the tie is 3.8x 
# greater, which we can translate into probability using the logistic function:

plogis(coef(random_graph2)[['edges']] + coef(random_graph2)[['mutual']])
# [1] 0.09142319

#More likely: 9% chance, compared to the baseline of 0.02%. In other words,
#there us a 9% chance of a mutual tie.
#Before we start writing up our submission to Science though, we need to check
#two things: 1) that the MCMC routine behaved well (that our estimates are likely
# good approximations of the MLEs), and 2) that our model fits the data well. 
#statnet has functions to do both those things.
# We use the mcmc.diagnostics function to get info on the MCMC chains, which by
# default are presented both graphically and through summary statistics. 
# The statistics can be quite useful, but for simplicity here I'm going to 
# silence them and focus on the trace plots of the chains.

mcmc.diagnostics(random_graph2)

#Examining model fit
#Now that we can trust our model estimates, let's see if they make a good fit to
#the data. We use the gof (goodness-of-fit) function for this purpose. 
#gof simulates networks from the ERGM estimates and, for some set of network
#statistics, compares the distribution in the simulated networks to the observed
#values. The current gof implementation has two useful modalities, one checks 
#goodness-of-fit against the statistics included in the model (in aggregate),
# for which the text output is usually sufficient. Note that a p-value closer to
# one is better: This is the difference between the observed networks and 
#simulations from the model.

random_graph2_gof = gof(random_graph2, GOF = ~model)
random_graph2_gof

# The other gof modality checks goodness-of-fit against some standard summary 
# statistics - by default: degree, edgewise shared partners, and path length 
#- decomposed to the components of the distributions. Plotting these is often
# quite informative. The black lines are the observed distributions and the 
# boxplots reflect networks simulated from the model.

random_graph2_gof2 = gof(random_graph2)
par(mfrow = c(2, 2))
plot(random_graph2_gof2)

#To change which statistics are included, specify them as a model formula to 
# the GOF argument to the gof function. E.g.
gof(random_graph2, GOF = ~ triadcensus + odegree + idegree)

#To add a term for homophily within Sampson's groups we use the term nodematch, 
# which takes at least one argument (the nodal attribute), and provides the 
# change in the likelihood of a tie if the two nodes match on that attribute. 
# Note that you can estimate a differential homophily effect; that is, the change
# in tie likelihood for two nodes being in the same group can vary by group, 
# by specifying the diff = TRUE argument to nodematch.
# Before we estimate the model, a handy trick: Remember that ERGM works by 
# calculating network-level statistics. You can get the values of those 
# statistics using the S3 method for summary for an ERGM formula:

summary(g2 ~ edges + mutual + nodematch('act_nat'))

#So of the 707 ties in the network, 22are reciprocal, and 313of them are between 
#actors within same nationality.So we should expect a 
#strong positive coefficient for the group-homophily term. Let's see:

m3 = ergm(g2 ~ edges + mutual + nodematch('act_nat'))
summary(m3)

#Indeed. The log-odds of a within-nationalitytie are 1.5xgreater than an across-group 
#tie. We can exponentiate to find the change in the odds, exp(coef(m3)[3]) = 4.8. 
#The change in the odds is true independent of the other attributes of the tie 
#(e.g. whether or not it is reciprocal). The probability of a tie, however, 
#is non-linear: it depends on the value of other statistics, so to calculate a 
#change in probability you must choose a value for every other statistic in the 
#model, then you can use the inverse-logit to find the difference in probability
#across your effect of interest. E.g. Let's look at the probability of 
#non-reciprocal ties within- and across-groups:

#Probability of a non-reciprocal, across-group tie:

plogis(coef(m3)[1])
#edges 
#0.001380752

#Probability of a non-reciprocal, within-group tie:
plogis(sum(coef(m3)[c(1, 3)])) 
#[1] 0.006610583
# The no. is 6 times greater! This means that nationality is a strong predictor
# of ties occurring. 

#Let's take a look at the goodness of fit of that model:
par(mar=c(1,1,1,1))

par(mfrow = c(2, 2))
invisible(plot(gof(m3)))


# Calculate probability of cliques in the graph.

random_graph3 <- ergm(g2 ~ edges + triangle)
theta3 <- random_graph3$coef
logit2prob(theta3)

# As you can see, Model degeneracy is a major problem for ERGMs. Degeneracy 
#refers to a case where the MLE for the specified sufficient statistics produce
#graphs that are either complete, or empty, or have all edges concentrated in a 
#small region of the graph, or otherwise produce networks that are not of interest. 
#Handcock's 2003 Assessing Degeneracy in Statistical Models of Social Networks 
#(pdf) is an excellent early treatment of the issue. It is a sign of an 
#ill-specified model, but unfortunately we often want estimates for theoretically
#justified reasons that we cannot get due to degeneracy issues. The quintessential
#such term is for triangles: How does the likelihood of a friendship change if 
#two people already have a friend in common? For this small of a network we can 
#estimate that directly:

m5 = ergm(g2~ edges + mutual + triangles)

#Degeneracy warning...Very interesting. This model produced degenerate networks. 
#You could have gotten some morefeedback about this during the fitting,by using:
m5 = ergm(g2~ edges + mutual + triangles,control=control.ergm(seed=1), verbose=T)

#You might try to increase the MCMC sample size:
m5 = ergm(g2~ edges + mutual + triangles,
          control = control.ergm(seed=1, MCMC.samplesize=20000),
          verbose=T)
mcmc.diagnostics(m5, center=F)

#How about trying the more robust version of modeling triangles: GWESP? (For a technical introduction to GWESP see Hunter and Handcock; for a more intuitive description and empirical application
#see Goodreau Kitts and Morris 2009)

m5 = ergm(g2~ edges + mutual +gwesp(0.5,fixed=T),
          control = control.ergm(seed=1))
mcmc.diagnostics(m5)

#Still degenerate, but maybe getting closer?
m5 = ergm(g2~ edges + mutual +gwesp(0.5,fixed=T) + nodematch('act_nat') + nodematch('act_type'),
          control = control.ergm(seed=1), verbose=T)

pdf('diagnostics1.pdf') #Use the recording function if possible, otherwise send to pdf
mcmc.diagnostics(m5)
dev.off()

# Changing gwesp parameter...
m5 = ergm(g2~ edges + mutual +gwesp(0.25,fixed=T) + nodematch('act_nat') + nodematch('act_type'),
          control = control.ergm(seed=1))
mcmc.diagnostics(m5)

#One more try...
m5 = ergm(g2~ edges + mutual +gwesp(0.15,fixed=T) + nodematch('act_nat') + 
            nodematch('act_type'),control = control.ergm(seed=1,MCMC.samplesize=4096,MCMC.interval=8192),
          verbose=T)


# Calculate homophily based on node attributes

random_graph4 <- ergm(g2 ~ edges +
                        nodematch("act_nat") +
                        nodematch("act_type")) 
theta4 <- random_graph4$coef
logit2prob(theta4)


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

#Edgewise shared partnerships, or triangles revisited. How to avoid degeneracy
# transitivity.

random_graph5 <- ergm(g2 ~ edges +
                        nodematch("act_nat") +
                        nodematch("act_type") + 
                        gwesp(0.25,fixed=FALSE))

# See https://eehh-stanford.github.io/SNA-workshop/ergm-predictions.html for more
# on ERGM and GWESP.

