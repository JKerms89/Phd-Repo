library(igraph)
library(dplyr)
library(RColorBrewer)
setwd("C:/OneDrive - Jan/Desktop")
library(readr)
RCAtest <- read_csv("RCAtest.csv")
View(RCAtest)
edgelist<-data.frame(RCAtest$`Claimant Name`,RCAtest$`Addressee name`)
natlist<-data.frame(RCAtest$`Claimant Name`,RCAtest$`Claimant nationality`)
natlist2<-data.frame(RCAtest$`Addressee name`,RCAtest$`Addressee nationality`)
colnames(natlist)<-c('Name','Country')
colnames(natlist2)<-c('Name','Country')
natlist<-rbind(natlist,natlist2)
natlist<-unique(natlist)
colnames(edgelist)<-c('Claimant','Addressee')
edgematrix2 <- t(matrix(edgelist,nrow=2))
g <- get.adjacency(graph.edgelist(as.matrix(edgelist), directed=TRUE))
g <- graph.adjacency(g, mode = "directed", weighted=T)
V(g)$C_nationality <- natlist$Country[match(V(g)$name, natlist$Name)]


col<- data.frame(country=unique(V(g)$C_nationality, stringAsFactors=F))
col$color <- brewer.pal(nrow(col), "Set3")
V(g)$color<-col$color[match(V(g)$C_nationality, col$country)]

plot(g)

png("Jan.png", width=12,height=8, units='in', res=300)
par(mar=c(0,2,3,2))
plot(g, vertex.size =10, vertex.color=V(g)$color, vertex.frame.color=NA, vertex.label.family='mono', vertex.label.cex=1, vertex.label.color='black', vertex.label.font=20, main='First network')
dev.off()

#Jan continues with the code developed by Wolf
#Add another edge attribute 

E(g)$newsp_type <- RCAtest$`Newspaper type`

col_edge_attr <- data.frame(newspaper = unique(E(g)$newsp_type))
col_edge_attr$color <-  c("red", "green")

#Add 'Claimant2Addressee' edge relation variable 
RCA_1  <- RCAtest  %>%  mutate(Claimant2Addressee = ifelse( `Claimant nationality` == `Addressee nationality`& `Addressee nationality`!= "EU supranational", "national", ifelse(`Claimant nationality`== "EU supranational" & `Addressee nationality`== "EU supranational", "supranational Europeanisation", ifelse(`Claimant nationality`== "EU supranational" | `Addressee nationality`== "EU supranational", "vertical Europeanisation", "horizontal Europeanisation"))))

#Add 'Claimant2Object' edge relation variable
RCA_2  <- RCA_1 %>%  mutate(Claimant2Object = ifelse( `Claimant nationality` == `Object nationality`& `Object nationality`!= "EU supranational", "national",  ifelse(`Claimant nationality`== "EU supranational" & `Object nationality`== "EU supranational", "supranational Europeanisation", ifelse(`Claimant nationality`!= "EU supranational" & `Object nationality`== "EU supranational", "bottom-up vertical Europeanisation", ifelse(`Claimant nationality`== "EU supranational" & `Object nationality`!= "EU supranational" & `Object nationality`!= "Global", "top-down vertical Europeanisation", ifelse(`Claimant nationality`== "Global" | `Object nationality`== "Global", "globalscope","horizontal Europeanisation"))))))

#Add Actor scope (a la Koopmans) variable
RCA_3  <- RCA_2 %>%  mutate(Actor_scope_Koopmans = ifelse( `Claimant nationality` == `Addressee nationality`& `Newspaper Origin`== `Addressee nationality`&`Addressee nationality`!= "EU supranational", "national", ifelse(`Claimant nationality`== "EU supranational" & `Addressee nationality`== "EU supranational", "supranational Europeanisation", ifelse(`Claimant nationality`== "EU supranational" | `Addressee nationality`== "EU supranational", "vertical Europeanisation","horizontal Europeanisation"))))

#Add Object scope variable (a la de Wilde, Koopmans)
RCA_4  <- RCA_3 %>%  mutate(Object_scope_Koopmans  = ifelse(`Newspaper Origin`== `Object nationality` &`Object nationality`!= "EU supranational", "national", ifelse(`Object nationality`== "EU supranational", "supranational Europeanisation", ifelse(`Object nationality`== "Global", "globalscope","horizontal Europeanisation"))))

# change one value in a row dataframe

RCA_4$C_Organisation[which(RCA_4$C_Organisation == "LG")] <- "LN" 
RCA_4$C_Organisation[which(RCA_4$C_Organisation == "CDU\n")] <- "CDU"

# Change node size according to degree in the network

deg <- degree(g, mode="all")
plot(g, vertex.size=deg*6)

# Change the thickness of the edge line

plot(g, vertex.size =10,edge.width=edge.betweenness(g), vertex.color=V(g)$color, vertex.frame.color=NA, vertex.label.family='mono', vertex.label.cex=1, vertex.label.color='black', vertex.label.font=20, main='First network')

## Make a palette of 4 colors

library(RColorBrewer)
col2  <- brewer.pal(4, "Set1") 

# Create a vector of color (alternative way)
my_color <- col2[as.numeric(as.factor(V(g)$C_nationality))]

# Make the plot
plot(g, vertex.color=my_color)

# Add a legend
legend("bottomleft", legend=levels(as.factor(V(g)$C_nationality))  , col = col2, bty = "n", pch=20 , pt.cex = 3, cex = 1.5, text.col=col2, horiz = FALSE, inset = c(0.1, 0.1))

#Update edge thickness and Node size according to weight
png("Jan.png", width=12,height=8, units='in', res=300)
par(mar=c(0,2,3,2))
plot(g, vertex.size =deg*6,edge.width=edge.betweenness(g), vertex.color=V(g)$color, vertex.frame.color=NA, vertex.label.family='mono', vertex.label.cex=1, vertex.label.color='black', vertex.label.font=20, main='First network')
dev.off()

#Code to match the CHES dataset party columns with my organization column to then extrapolate the party family variables

library(readr)
RCAtest <- read_csv("RCAtest.csv")
View(RCAtest)
library(readr)
CHES_means_2017_2_ <- read_csv("CHES_means_2017 (2).csv")
View(CHES_means_2017_2_)
match(RCA_4$C_Organisation, CHES_means_2017_2_$party)
CHES_means_2017_2_$family[match(RCA_4$C_Organisation, CHES_means_2017_2_$party)]
RCA_4$party_family= CHES_means_2017_2_ $family[match(RCA_4$C_Organisation, CHES_means_2017_2_$party)]
na.omit(RCA_4$party_family) 

#Generate histogram of node degree
hist(deg, breaks=1:vcount(g)-1, main="Histogram of node degree")

#Calculate degree distribution 
deg.dist <- degree_distribution(g, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

#Calculate community detection
ceb <- cluster_edge_betweenness(g) 
dendPlot(ceb, mode="hclust")

plot(ceb, g)

#Aggregate Evaluation of EU by organization 
Eval_EU <- aggregate(RCAtest$`Evaluation EU`, by=list(RCAtest$C_Organisation), FUN=mean)
colnames(Eval_EU)<-c('Organization','Evaluation')
Eval_EU$Organization[which(Eval_EU$Organization == "LG")] <- "LN" 
library(ggplot2)
# Basic barplot
p <- ggplot(data=Eval_EU, aes(x=Organization, y=Evaluation)) +
  geom_bar(stat="identity")
# Horizontal bar plot
p + coord_flip() 

#Delete edge attribute 

delete_edge_attr(g, "newsp_type")
delete_edge_attr(g, "color")






#Create Claimant, Addressee, and Scope dataframe
scopelist<-data.frame(RCA_4$`Claimant Name`,RCA_4$`Addressee name`, RCA_4$Claimant2Addressee)
#Change the column name of dataframe
colnames(scopelist)<-c('C_Name','A_Name','Scope')
#Remove duplicates so that dataframe matches the no. of unique edges 
scopelist2<-unique(scopelist)
#Get edge list a dataframe
compg.edges <- as.data.frame(get.edgelist(g))
#Change column name of edge list so that it is the same as the column name of the preceding dataframe
colnames(compg.edges)<-c('C_Name','A_Name')
# innerjoin so that scope col from df2 creates new column in df1 with matching data.
df_scope <- inner_join(compg.edges, scopelist2)
#Add edge attribute 
E(g)$scope <- df_scope$Scope
#Add colours to edge attributes
col <-  data.frame(scope=unique(E(g)$scope, stringAsFactors=F))
col$color <- brewer.pal(nrow(col), "Accent")
E(g)$color<-col$color[match(E(g)$scope, col$scope)]
#Create network graph in png format inc. legend
png("Jan.png", width=12,height=8, units='in', res=300)
par(mar=c(0,2,3,2))
plot(g, vertex.size =deg*6,edge.width=edge.betweenness(g), vertex.color=V(g)$color, vertex.frame.color=NA, vertex.label.family='mono', vertex.label.cex=1, vertex.label.color='black', vertex.label.font=20, main='First network')
legend("bottomleft", legend=levels(as.factor(E(g)$scope))  , col = col$color, bty = "n", pch=20 , pt.cex = 3, cex = 1.5, text.col= col$color, horiz = FALSE, inset = c(0.1, 0.1))
dev.off()


#Create network graph in png format inc. legend
plot(g, color= E(g)$color)
png("Jan.png", width=12,height=8, units='in', res=300)
par(mar=c(0,2,3,2))
plot(g, vertex.size =deg*6,edge.width=edge.betweenness(g), vertex.color=V(g)$color, vertex.frame.color=NA, vertex.label.family='mono', vertex.label.cex=1, vertex.label.color='black', vertex.label.font=20, main='First network')
#Amend legend manually 
legend("bottomleft", legend=c("Horizontal Europeanization", "Vertical Europeanization", "Supranational Europeanization", "National"), col = c("#7FC97F", "#FDC086", "#FFFF99", "#BEAED4"), bty = "n", pch=20 , pt.cex = 3, cex = 1.0, text.col= c("#7FC97F", "#FDC086", "#FFFF99", "#BEAED4"), horiz = FALSE, inset = c(0.1, 0.1))
dev.off()

#act2adrscope variable code (see below / amend accordingly)
RCA9  <- RCA9 %>%  mutate(act2adr = ifelse( `act_nat2` == `adr_nat2`& `adr_nat2`!= "EUROPEAN UNION" & `adr_nat2`!= 'NA',"national", ifelse(`act_nat2`== "EUROPEAN UNION" & `adr_nat2`== "EUROPEAN UNION", "supranational Europeanisation", ifelse(`act_region` == "EUROPE" & `act_nat2`!= "EUROPEAN UNION" & `adr_nat2`== "EUROPEAN UNION", "bottom-up vertical Europeanisation", ifelse(`act_nat2` ==  "EUROPEAN UNION" & `act_region`== "EUROPE" & `adr_nat2` != "EUROPEAN UNION" & `adr_region`== "EUROPE","top-down vertical Europeanisation", ifelse(`act_region` == 'WORLD / OTHER' | `adr_region` == 'WORLD / OTHER', "regional / global (other)", "horizontal Europeanisation"))))))

# Create act_region column (EUrope or other?)
RCA9  <- RCA9 %>%  mutate(act_region = ifelse(`act_nat2` == "POLAND" |`act_nat2` == "FRANCE" | `act_nat2` == "UNITED KINGDOM" | `act_nat2` == "ITALY" | `act_nat2` == "SLOVAKIA" | `act_nat2` == "GERMANY" | `act_nat2` == "HUNGARY" | `act_nat2` == "UKRAINE" | `act_nat2` == "MALTA" | `act_nat2` == "NETHERLANDS"| `act_nat2` == "TURKEY" | `act_nat2` == "BELGIUM" | `act_nat2` =="CZECH REPUBLIC" | `act_nat2` == "ROMANIA" | `act_nat2` =="AUSTRIA" | `act_nat2` =="LUXEMBOURG" | `act_nat2` =="ALBANIA" | `act_nat2` =="CYPRUS" | `act_nat2` =="IRELAND" | `act_nat2` =="LATVIA" | `act_nat2` =="MOLDOVA" | `act_nat2` =="SPAIN" | `act_nat2` =="SLOVENIA" | `act_nat2` =="GREECE" | `act_nat2` =="SWITZERLAND" | `act_nat2` =="FINLAND" | `act_nat2` =="DENMARK" | `act_nat2` =="NORTHERN IRELAND" | `act_nat2` =="BELARUS" | `act_nat2` =="LITHUANIA" | `act_nat2` =="NORWAY" | `act_nat2` =="PORTUGAL" | `act_nat2` =="KOSOVO" | `act_nat2` =="SWEDEN" | `act_nat2` =="CROATIA" | `act_nat2` =="SERBIA" | `act_nat2` =="BOSNIA & HERZEGOVINA" | `act_nat2` =="ESTONIA" | `act_nat2` == "BULGARIA" | `act_nat2` == "EUROPEAN UNION", "EUROPE", "WORLD / OTHER"))

# Create adr_region column (EUrope or other?)
RCA9  <- RCA9 %>%  mutate(adr_region = ifelse(`adr_nat2` == "POLAND" |`adr_nat2` == "FRANCE" | `adr_nat2` == "UNITED KINGDOM" | `adr_nat2` == "ITALY" | `adr_nat2` == "SLOVAKIA" | `adr_nat2` == "GERMANY" | `adr_nat2` == "HUNGARY" | `adr_nat2` == "UKRAINE" | `adr_nat2` == "MALTA" | `adr_nat2` == "NETHERLANDS"| `adr_nat2` == "TURKEY" | `adr_nat2` == "BELGIUM" | `adr_nat2` =="CZECH REPUBLIC" | `adr_nat2` == "ROMANIA" | `adr_nat2` =="AUSTRIA" | `adr_nat2` =="LUXEMBOURG" | `adr_nat2` =="ALBANIA" | `adr_nat2` =="CYPRUS" | `adr_nat2` =="IRELAND" | `adr_nat2` =="LATVIA" | `adr_nat2` =="MOLDOVA" | `adr_nat2` =="SPAIN" | `adr_nat2` =="SLOVENIA" | `adr_nat2` =="GREECE" | `adr_nat2` =="SWITZERLAND" | `adr_nat2` =="FINLAND" | `adr_nat2` =="DENMARK" | `adr_nat2` =="NORTHERN IRELAND" | `adr_nat2` =="BELARUS" | `adr_nat2` =="LITHUANIA" | `adr_nat2` =="NORWAY" | `adr_nat2` =="PORTUGAL" | `adr_nat2` =="KOSOVO" | `adr_nat2` =="SWEDEN" | `adr_nat2` =="CROATIA" | `adr_nat2` =="SERBIA" | `adr_nat2` =="BOSNIA & HERZEGOVINA" | `adr_nat2` =="ESTONIA" | `adr_nat2` == "BULGARIA" | `adr_nat2` == "EUROPEAN UNION", "EUROPE", "WORLD / OTHER"))

# Create obj_region column (EUrope or other?)
RCA10 <- RCA10 %>%  mutate(obj_region = ifelse(`objnat` == "UNSPECIFIED" | `objnat`== "REGIONAL / GLOBAL" | `objnat`==  "RUSSIA" | `objnat`== "UNITED STATES" | `objnat`==  "CHINA" |  `objnat`== "SRI LANKA" | `objnat`== "LIBYA" | `objnat`== "TIBET" | `objnat`== "SYRIA" | `objnat`== "ARMENIA" | `objnat`== "PALESTINE" | `objnat`== "BRAZIL" | `objnat`== "IRAQ" | `objnat`==  "IRAN", "WORLD / OTHER", "EUROPE"))

# create act2obj scope taxonomy (see below for an example)
RCA10  <- RCA10 %>%  mutate(act2obj = ifelse(`act_nat2` == `objnat` & `objnat`!= "EUROPEAN UNION", "national", ifelse(`act_nat2`== "EUROPEAN UNION" & `objnat`== "EUROPEAN UNION", "supranational Europeanisation", ifelse(`act_region` == "EUROPE" & `act_nat2`!= "EUROPEAN UNION" & `objnat`== "EUROPEAN UNION", "bottom-up vertical Europeanisation", ifelse(`act_nat2` ==  "EUROPEAN UNION" & `obj_region`== "EUROPE" & `obj_region` != "EUROPEAN UNION", "top-down vertical Europeanisation", ifelse(`act_region`== "WORLD / OTHER" | `obj_region`== "WORLD / OTHER", "regional / global (other)", "horizontal Europeanisation"))))))

#Steps to create node and edgelist for one-mode network (example below)

setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset")
library(readxl)
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA8.xlsx")
library(dplyr)
RCAdf2 <- RCAdf %>% filter(source_country == "___________" & !is.na(adrorg))
nodelist <- RCAdf2 %>% select(actorg, act_type2, act_nat2, adrorg, adr_type2, adr_nat2, act2adr)
actnodelist <- nodelist %>% select(actorg,act_type2,act_nat2)
adrnodelist <- nodelist %>% select(adrorg,adr_type2,adr_nat2)
colnames(actnodelist) <-c('id', 'act_type', 'act_nat')
colnames(adrnodelist) <-c('id', 'act_type', 'act_nat')
nodelist2 <- rbind(actnodelist, adrnodelist)
nodelist3 <-unique(nodelist2)
nodelist3$Label = nodelist3$id
nodelist3 <- nodelist3 %>% relocate(Label, .after = id)
colnames(nodelist3) <-c('id', 'Label', 'act_type', 'act_nat')
library(xlsx)
write.xlsx(as.data.frame(nodelist3), 'onemodenetwork_DE_nodelist.xlsx', row.names = FALSE)
edgeslist <- nodelist %>% select(actorg,adrorg, act2adr)
edgeslist$Type = 'Directed'
edgeslist <- edgeslist %>% relocate(Type, .after = adrorg)
colnames(edgeslist) <-c('Source', 'Target', 'Type', 'act2adr scope')
write.xlsx(as.data.frame(edgeslist), 'onemodenetwork_DE_edgelist.xlsx', row.names = FALSE)

#How to add pre-assigned hex values (i.e. colors) to node attributes. This is useful because it means
# every graph I use can have the same colours for the same node attributes.
# E.g. Germany == 'red' (always) and Poland == 'yellow. 
# Firstly, one need to create a color vector specifying no. of colours they 
# wish to use. As Brewer.pal often does not have >15 colors, there is a 
# useful workround. See below...
# If one wants a range of 20 colors, one has to do the following:
# First, lets create a colour column:

onemodenetwork_NL_edgelist$color <- ""
library(RColorBrewer)
nb.cols <- 20
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
df$color <- col[as.numeric(as.factor(df$`act2adr scope`))]

# A similar process applies to node attributes color setting. Let's give it a 
# with the original RCA dataset for act/adr/obj nationality/country attributes.

library(readxl)
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA8.xlsx")
actdf <- RCAdf %>% select(actorg,act_nat2)
adrdf <- RCAdf %>% select(adrorg,adr_nat2)
objdf <- RCAdf %>% select(objnat)
objdf <- na.omit(objdf)
# Make a duplicate column
objdf$nat <- objdf$objnat
objdf$objnat <- tolower((objdf$objnat))
objdf2 <- objdf[!grepl("UNSPECIFIED", objdf$nat),]
objdf2 <- objdf[!grepl("unspecified", objdf$objnat),]
objdf2$objnat <- paste0(objdf2$objnat, " constituency")
#rename columns so they match then rbind the 3 dfs into one df.
colnames(adrdf) <-c('actorg','act_nat2')
colnames(objdf3) <-c('actorg','act_nat2')
rbind(actdf,adrdf, objdf3)
df3 <- rbind(actdf,adrdf, objdf3)
df4 <- unique(df3)
# Count no. of unique data values 
length(unique(df4$act_nat2))

# Result is 80 different nationalities/country node attributes. Now I
# need to assigned colours to the 80 node attributes.
df4$colour <- ""
library(RColorBrewer)
nb.cols <- 80
col <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)
df4$colour <- col[as.numeric(as.factor(df4$act_nat2))]
length(unique(df4$colour))
#Save nationality node attribute/colour df.
library(xlsx)
write.xlsx(df4, 'nodecolorattributes.xlsx')
# Then I need to mutate & match column of nodelist (for DE,IT,NL,PL) 
# with the df4 dataframe.
match(onemodenetwork_NL_nodelist$act_nat, df4$act_nat2)
df4$colour[match(onemodenetwork_NL_nodelist$act_nat, df4$act_nat2)]
onemodenetwork_NL_nodelist$colour = df4$colour[match(onemodenetwork_NL_nodelist$act_nat, df4$act_nat2)]
library(xlsx)
write.xlsx(onemodenetwork_NL_nodelist, 'onemodenetwork_NL_nodelist2.xlsx')

# How to make a legend for nodes (e.g. NL graph) which will be used for gephi graph.
legendnl <- c('NETHERLANDS (24.94%)','EUROPEAN UNION (13.52%)','UNITED KINGDOM (9.09%)','NA (8.62%)','FRANCE (5.13%)','GERMANY (4.9%)', 'ITALY (4.66%)','UNITED STATES (4.43%)','SPAIN (2.56%)','POLAND (2.33%)')
nlcol <- c('#91A714', '#8D60AA','#7C6B4C','#86A716','#A154A2','#AB4D9F','#C34B6B','716859','#AF7E18','#BFA90A')
png(filename="nodeslegendNL.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Node type (top 10)", legend = legendnl, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = nlcol)
dev.off()

# Matching edge attributes with colours
edgescopecol <- c('#FF0000','#008000', '#0000FF', '#FF00FF', '#FFFF00')
edgescopedf <- c('national', 'vertical Europeanisation', 'horizontal Europeanisation', 'regional / global (other)', 'supranational Europeanisation')
df1 <- as.data.frame(edgescopecol)
df2 <- as.data.frame(edgescopedf)
library(dplyr)
df3 <- df2 %>% mutate(colour = edgescopecol)
library(readxl)
onemodenetwork_NL_edgelist <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Netherlands/one-mode networks/onemodenetwork_NL_edgelist.xlsx")
View(onemodenetwork_NL_edgelist)
match(onemodenetwork_NL_edgelist$`act2adr scope`, df3$edgescopedf)
df3$colour[match(onemodenetwork_NL_edgelist$`act2adr scope`, df3$edgescopedf)]
onemodenetwork_NL_edgelist$colours = df3$colour[match(onemodenetwork_NL_edgelist$`act2adr scope`, df3$edgescopedf)]
library(xlsx)
write.xlsx(onemodenetwork_NL_edgelist, 'onemodenetwork_NL_edgelist2.xlsx')

# How to make a legend (e.g NL graph) for edges which will be used for gephi graph.
edgescolor <- c('#FF0000','#008000', '#0000FF', '#FF00FF', '#FFFF00')
edgeslegend <- c('national (38.14%)', 'vertical Europeanisation (30.83%)', 'horizontal Europeanisation (17.59%)', 'regional / global (other) (6.92%)', 'supranational Europeanisation (6.52%)')
png(filename="edgeslegendNL.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Edge type (top 5)", legend = edgeslegend, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = edgescolor)
dev.off()
