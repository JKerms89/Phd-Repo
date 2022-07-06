library('igraph')
names <- c('afd', 'bartsch', 'cdu', 'fdp', 'goering_eckardt', 'gruene', 'lindner', 'linkspartei', 'merkel', 'oezdemir', 'schulz', 'spd', 'wagenknecht', 'weidel') # sets list of party and candidate names (except for csu/herrmann)
nodes <- data.frame(names, stringsAsFactors = FALSE) # writes them in to a dataframe of nodes
nodes$count<-NA # new empty column count
nodes$type <-'party' # new column type filled with 'party'
#nodes$id <- c('p01', 'p02', 'p03','p04', 'p05', 'p06', 'p07','p08', 'p09', 'p10', 'p11','p12', 'p13', 'p14', 'p15','p16')
## add URLs to list of nodes
for (i in 1:length(names)) {
  filena = paste0('URLs ',names[i],'.csv')
  new <- read.csv(filena,sep=';', stringsAsFactors = FALSE)
  new$type<-'URL'
  colnames(new)<- c('names', 'count', 'type')
  nodes <- rbind(nodes,new)
}

# nodes <- na.omit(nodes)

nodes$id <- NA # add empty column called id
nodes$id[1:14] <- c('p01', 'p02', 'p03','p04', 'p05', 'p06', 'p07','p08', 'p09', 'p10', 'p11','p12', 'p13', 'p14') # add p-ids to every party or candidate

nodes <- nodes[!duplicated(nodes), ]

## add r-ids to every URL
for (j in 15:nrow(nodes)){
  nodes$id[j] <- paste0('r', j-14)
}

new<-data.frame() # create empty dataframe called new


#colnames(new)<- c('names', 'count','type', 'id')
#new<-NULL
edges<-data.frame(stringsAsFactors = FALSE) # create empty dataframe called edges
# create list of edges by taking URLs from every party/candidate file and integrate them into one list
for (i in 1:length(names)) {
  filena = paste0('URLs ',names[i],'.csv')
  new <- read.csv(filena,sep=';', header=FALSE, stringsAsFactors = FALSE)
  new$from<-names[i]
  colnames(new)<- c('to', 'count', 'from')
  new$count <- (new$count/sum(new$count))*10000
  edges <- rbind(edges,new)
}
edges <- edges[,c('from','to', 'count')]
nrow(nodes)
length(unique(nodes$names))
nrow(edges)
nrow(unique(edges[,c("from", "to")]))
## aggregate in case of still double entries
edges <- aggregate(edges[,3], edges[,-3], sum)
colnames(edges)[3] <- 'weight'

## run if you want to work with ids only
for (i in 1:nrow(edges)) {
  edges$from[i] <- nodes$id[match(edges$from[i], nodes$names)]
  edges$to[i] <- nodes$id[match(edges$to[i], nodes$names)]
}

edgematrix <- tidyr::spread(edges,from, weight, fill = 0) # build edgematrix
row.names(edgematrix) <- edgematrix$to
edgematrix$to <-NULL

nodes <- nodes[,c('id','names', 'type')]

net <- graph_from_incidence_matrix(edgematrix) # create graph from edgematrix
class(net)

V(net)$degree <- degree (net, v= V(net), mode = 'all')    
table(V(net)$type)

summary(net)

net_sub <- induced_subgraph(net, V(net)$degree>=12 & V(net)$degree<=1000)

net_sub_f<-layout_with_fr(net_sub)

net.bp <- bipartite.projection(net) # make bipartite projection

plot(net.bp$proj2, vertex.label.color="black", vertex.label.dist=1, vertex.size=7) #simple plot

net.bp_f2<-layout_with_fr(net.bp$proj2)

## Community detection
comm <- cluster_fast_greedy(net.bp$proj2) # 2 communities
comm2 <- cluster_leading_eigen(net.bp$proj2) # 1 community

x<- membership(comm)

y<- membership(comm2)

## high quality plotting
png("Network_CD7.png", width=12,height=8, units='in', res=300)
par(mar=c(0,2,3,2))
plot(comm, net.bp$proj2, layout=net.bp_f2, vertex.size =2, vertex.frame.color=NA, vertex.label.family='mono', vertex.label.cex=2, vertex.label.color='blue', vertex.label.font=1, edge.width=(E(net.bp$proj2)$weight/70), main='cited domains')
dev.off()

## high quality plotting
png("Network_CD8.png", width=12,height=8, units='in', res=300)
par(mar=c(0,2,3,2))
plot(net_sub, layout=net_sub_f, vertex.size =1, vertex.frame.color=NA, vertex.label.family='mono', vertex.label.cex=1, vertex.label.color='blue', vertex.label.font=1, edge.width=(E(net.bp$proj2)$weight/70), main='cited domains')
dev.off()