setwd("C:/OneDrive - Jan/Desktop")
RCAtest <- read_csv("RCAtest.csv")
nodes <- data.frame(RCAtest$`Claimant Name`, RCAtest$`Object constituency`)
edgematrix <- t(matrix(nodes,nrow=2))
g2 <- get.adjacency(graph.edgelist(as.matrix(nodes), directed=FALSE))
g2 <- graph.adjacency(g2, mode = "undirected", weighted=T)
plot(g2)
V(g2)$degree <- degree (g2, v= V(g2), mode = 'all')  
table(V(g2)$degree)
summary(g2)
#Because igraph does not automatically recognize two-mode networks, it is necessary to tell igraph that there are two types of vertices. 
bipartite.mapping(g2)
V(g2)$type <- bipartite_mapping(g2)$type
plot(g2, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)
plot(g2, vertex.label.cex = 0.8, vertex.label.color = "black")
V(g2)$color <- ifelse(V(g2)$type, "lightblue", "salmon")
V(g2)$shape <- ifelse(V(g2)$type, "circle", "square")
E(g2)$color <- "lightgray"
plot(g2, vertex.label.cex = 0.8, vertex.label.color = "black")

plot(g2, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)

g.bp <- bipartite.projection(g2)
plot(g.bp$proj1, vertex.label.color="black", vertex.label.dist=1, vertex.size=7) #simple plot

g2_sub <- induced_subgraph(g2, V(g2)$degree>=12 & V(g2)$degree<=1000)

# Refer to this extremely helpful website for more: https://rpubs.com/pjmurphy/317838 

#png image of bipartite network

plot(g2)
png("bipartite.png", width=12,height=8, units='in', res=300)
par(mar=c(0,2,3,2))
plot(g2, layout=layout.bipartite, vertex.size =3, vertex.color=V(g2)$color, vertex.frame.color=NA, vertex.label.family='mono', vertex.label.cex=1, vertex.label.color='black', vertex.label.font=20, main='First network')
dev.off()
