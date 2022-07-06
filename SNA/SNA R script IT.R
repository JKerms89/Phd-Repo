#Steps to create node and edgelist for one-mode network (example below)

setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset")
library(readxl)
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA10clean.xlsx")
library(dplyr)
RCAdf2 <- RCAdf %>% filter(source_country == "ITALY" & !is.na(adrorg))
nodelist <- RCAdf2 %>% select(actname, actorg, act_type, act_nat, adrname, adrorg, adr_type, adr_nat)
actnodelist <- nodelist %>% select(actname, actorg,act_type,act_nat)
adrnodelist <- nodelist %>% select(adrname, adrorg,adr_type,adr_nat)
colnames(actnodelist) <-c('id', 'orgid', 'act_type', 'act_nat')
colnames(adrnodelist) <-c('id', 'orgid', 'act_type', 'act_nat')
nodelist2 <- rbind(actnodelist, adrnodelist)

# To create a nodelist for actorg, do the following code:
nodelist2 = nodelist2[!duplicated(nodelist2$orgid),]
colnames(nodelist2) <-c('id', 'orgid', 'act_type', 'act_nat')
library(xlsx)
write.xlsx(as.data.frame(nodelist2), 'IT_nodelistorg_1MN.xlsx', row.names = FALSE)

# create edgelist
edgeslist <- RCAdf2 %>% select(actname,actorg,adrname, adrorg, act2adr, adreval, EUval)
edgeslist$Type = 'Directed'
write.xlsx(as.data.frame(edgeslist), 'IT_edgelist_1MN.xlsx', row.names = FALSE)

# Then match nodes attributes to colors
library(readxl)
IT_nodelistorg_1MN <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/IT_nodelistorg_1MN.xlsx")
df4 <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Misc/nodeattributes(nat)2colorsdf2.xlsx")
View(nodeattributes_nat_2colorsdf2)
match(IT_nodelistorg_1MN$act_nat, df4$act_nat)
df4$colour[match(IT_nodelistorg_1MN$act_nat, df4$act_nat)]
IT_nodelistorg_1MN$colour = df4$colour[match(IT_nodelistorg_1MN$act_nat, df4$act_nat)]
library(xlsx)
write.xlsx(IT_nodelistorg_1MN,'IT_nodelist_1MN.xlsx')

# Matching edge attributes with colours
edgescopecol <- edgescolor <- c('#FF0000','#FF00FF','#008000','#0000FF','#FFA500','#FFFF00')
edgescopedf <- c('national','regional / global (other)', 'bottom-up vertical Europeanisation','horizontal Europeanisation','top-down vertical Europeanisation', 'supranational Europeanisation')
df1 <- as.data.frame(edgescopecol)
df2 <- as.data.frame(edgescopedf)
library(dplyr)
df3 <- df2 %>% mutate(colour = edgescopecol)
library(readxl)
IT_edgelist_1MN <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Italy/One-mode networks/IT_edgelist_1MN.xlsx")
match(IT_edgelist_1MN$`act2adr`, df3$edgescopedf)
df3$colour[match(IT_edgelist_1MN$`act2adr`, df3$edgescopedf)]
IT_edgelist_1MN$colour = df3$colour[match(IT_edgelist_1MN$`act2adr`, df3$edgescopedf)]
library(xlsx)
write.xlsx(IT_edgelist_1MN, 'IT_edgelist_1MN_2.xlsx')


# How to make a legend for nodes which will be used for gephi graph.
legendIT <- c('ITALY (44.63%)','EUROPEAN UNION (15.54%)','NA (8.47%)','GERMANY (5.65%)','UNITED STATES (3.95%)','FRANCE (3.39%)', 'UNITED KINGDOM (3.39%)','POLAND (2.54%)','SPAIN (1.98%)','NETHERLANDS (1.98%)')
ITcol <- c('#B85D6F','#5C9A5C','#FFA910','#717F75','#EF7DB1','#6A886D','#E879A3','#FFE428','#AF6729','#FFB415')
png(filename="nodeslegendIT.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Node type (top 10)", legend = legendIT, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = ITcol)
dev.off()

# How to make a legend (e.g NL graph) for edges which will be used for gephi graph.

#'national' = '#FF0000'
#'regional / global (other)()' = '#FF00FF'
#'bottom-up vertical Europeanisation ()' = '#008000'
#'horizontal Europeanisation ()' = '#0000FF'
#'top-down vertical Europeanisation ()' =  '#FFA500'
#'supranational Europeanisation ()' = '#FFFF00'

edgescolor <- c('#FF0000', '#008000', '#0000FF','#FF00FF', '#FFFF00','#FFA500')
edgeslegend <- c('national (38.76%)','bottom-up vertical Europeanisation (22.29%)', 'horizontal Europeanisation (13.45%)','regional / global (other)(12.45%)', 'supranational Europeanisation (6.63%)', 'top-down vertical Europeanisation (6.43%)')
png(filename="edgeslegendIT.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Edge type (top 5)", legend = edgeslegend, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = edgescolor)
dev.off()
















########################################### BIPARTITE NETWORK################
# To create nodelist ready for Gephi 

library(readxl)
library(dplyr)
library(readxl)
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA10clean.xlsx")
RCAdf2 <- RCAdf %>% filter(source_country == "ITALY" & !is.na(objnat))
RCAdf2$objnat <- paste0(RCAdf2$objnat, " constituency")
RCAdf3 <- RCAdf2[!grepl("UNSPECIFIED constituency", RCAdf2$objnat),]
RCAdf3$objnat <- tolower((RCAdf3$objnat))
actnodelist <- RCAdf3 %>% select(actorg, act_type, act_nat)
actnodelist2 <- unique(actnodelist)
colnames(actnodelist2) <-c('id','act_type', 'nat')
objnodelist <- RCAdf3 %>% select(objnat, objtype)
objnodelist2 <- unique(objnodelist)
library(stringr)
objnodelist2$nat <- word(objnodelist2$objnat, 1,2, sep = " ")
objnodelist2$nat <-gsub("constituency","",as.character(objnodelist2$nat))
objnodelist2$nat[which(objnodelist2$nat == "russia")] <- "russian federation"
objnodelist2$nat = toupper(objnodelist2$nat)
objnodelist2$nat[which(objnodelist2$nat == "REGIONAL /")] <- "REGIONAL / GLOBAL"
colnames(objnodelist2) <-c('id','act_type', 'nat')
library(plyr)
nodelist <- rbind.fill(actnodelist2, objnodelist2)
nodelist %>%  mutate(act_type = ifelse(grepl("constituency", id), "constituency/group", act_type))
nodelist3 <- unique(nodelist2)
nodelist4 <- nodelist3 %>%  mutate(Mode = ifelse(grepl("constituency", id), "2", "1"))
nodelist5 <- nodelist4 %>% relocate(Mode, .after = id)
colnames(nodelist5) <-c('Label','mode', 'act_type', 'nat')
library(xlsx)
write.xlsx(nodelist5, 'IT_nodelist_bipartite.xlsx', row.names = FALSE)

#To create edgelist ready for Gephi 
edgelist <- RCAdf3 %>% select(actorg, objnat, act2obj, EUval)
edgelist$Type <- 'Undirected'
edgelist2 <- edgelist %>% relocate(Type, .after = objnat)
colnames(edgelist2) <-c('Source','Target','Type','act2obj', 'EUeval')
write.xlsx(as.data.frame(edgelist2), 'IT_edgelist_bipartite.xlsx', row.names = FALSE)


# Then match nodes attributes to colors

library(readxl)
IT_nodelist_bipartite <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/IT_nodelist_bipartite.xlsx")
View(IT_nodelist_bipartite)
df4 <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Misc/nodeattributes(nat)2colorsdf2.xlsx")
View(nodeattributes_nat_2colorsdf2)
match(IT_nodelist_bipartite$nat, df4$act_nat)
df4$colour[match(IT_nodelist_bipartite$nat, df4$act_nat)]
IT_nodelist_bipartite$colour = df4$colour[match(IT_nodelist_bipartite$nat, df4$act_nat)]
library(xlsx)
write.xlsx(IT_nodelist_bipartite,'IT_nodelist_bipartite2.xlsx')

# Matching edge attributes with colours
edgescopecol <- edgescolor <- c('#FF0000','#FF00FF','#008000','#0000FF','#FFA500','#FFFF00')
edgescopedf <- c('national','regional / global (other)', 'bottom-up vertical Europeanisation','horizontal Europeanisation','top-down vertical Europeanisation', 'supranational Europeanisation')
df1 <- as.data.frame(edgescopecol)
df2 <- as.data.frame(edgescopedf)
library(dplyr)
df3 <- df2 %>% mutate(colour = edgescopecol)
library(readxl)
library(readxl)
IT_edgelist_bipartite <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/IT_edgelist_bipartite.xlsx")
View(IT_edgelist_bipartite)
match(IT_edgelist_bipartite$`act2obj`, df3$edgescopedf)
df3$colour[match(IT_edgelist_bipartite$`act2obj`, df3$edgescopedf)]
IT_edgelist_bipartite$colour = df3$colour[match(IT_edgelist_bipartite$`act2obj`, df3$edgescopedf)]
library(xlsx)
write.xlsx(IT_edgelist_bipartite, 'IT_edgelist_bipartite2.xlsx')

######### MAking the legends for the bipartite graph (see below)
# How to make a legend for nodes which will be used for gephi graph.
legendIT <- c('ITALY (54.19%)','EUROPEAN UNION (10.32%)','NA (5.16%)','GERMANY (5.16%)','UNITED STATES (4.52%)','FRANCE (4.52%)','UNITED KINGDOM (2.58%)', 'SPAIN (2.58%)','AUSTRIA (1.94%)','POLAND (1.94%)')
ITcol <- c('#B85D6F','#5C9A5C','#FFA910','#717F75','#EF7DB1','#6A886D','#E879A3','#AF6729','#944863','#FFE428')
png(filename="nodeslegendITbipartite.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Node type (top 10)", legend = legendIT, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = ITcol)
dev.off()

# How to make a legend for edges which will be used for gephi graph.

#'national' = '#FF0000'
#'regional / global (other)()' = '#FF00FF'
#'bottom-up vertical Europeanisation ()' = '#008000'
#'horizontal Europeanisation ()' = '#0000FF'
#'top-down vertical Europeanisation ()' =  '#FFA500'
#'supranational Europeanisation ()' = '#FFFF00'

edgescolor <- c('#FF0000', '#008000','#FF00FF','#FFFF00', '#0000FF','#FFA500')
edgeslegend <- c('national (50.93%)','bottom-up vertical Europeanisation (24.84%)','regional / global (other) (11.18%)','supranational Europeanisation (8.7%)','horizontal Europeanisation (3.11%)', 'top-down vertical Europeanisation (1.24%)')
png(filename="edgeslegendITbipartite.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Edge type (top 5)", legend = edgeslegend, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = edgescolor)
dev.off()








