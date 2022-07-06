#Steps to create node and edgelist for one-mode network (example below)

setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset")
library(readxl)
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA10clean.xlsx")
library(dplyr)
RCAdf2 <- RCAdf %>% filter(source_country == "NETHERLANDS" & !is.na(adrorg))
nodelist <- RCAdf2 %>% select(actname, actorg, act_type, act_nat, adrname, adrorg, adr_type, adr_nat)
actnodelist <- nodelist %>% select(actorg,act_type,act_nat)
adrnodelist <- nodelist %>% select(adrorg,adr_type,adr_nat)
colnames(actnodelist) <-c('id', 'act_type', 'act_nat')
colnames(adrnodelist) <-c('id', 'act_type', 'act_nat')
nodelist2 <- rbind(actnodelist, adrnodelist)

# To create a nodelist for actorg, do the following code:
nodelist2 = nodelist2[!duplicated(nodelist2$id),]
colnames(nodelist2) <-c('id', 'act_type', 'act_nat')
library(xlsx)
write.xlsx(as.data.frame(nodelist2), 'NL_nodelistorg_1MN.xlsx', row.names = FALSE)

# create edgelist
edgeslist <- RCAdf2 %>% select(actorg,adrorg, act2adr, adreval, EUval)
edgeslist$Type = 'Directed'
write.xlsx(as.data.frame(edgeslist), 'NL_edgelist_1MN.xlsx', row.names = FALSE)

# Then match the data
library(readxl)
NL_nodelistorg_1MN <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/NL_nodelistorg_1MN.xlsx")
df4 <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Misc/nodeattributes(nat)2colorsdf2.xlsx")
View(nodeattributes_nat_2colorsdf2)
match(NL_nodelistorg_1MN$act_nat, df4$act_nat)
df4$colour[match(NL_nodelistorg_1MN$act_nat, df4$act_nat)]
NL_nodelistorg_1MN$colour = df4$colour[match(NL_nodelistorg_1MN$act_nat, df4$act_nat)]
library(xlsx)
write.xlsx(NL_nodelistorg_1MN,'NL_nodelist_1MN.xlsx')


# Matching edge attributes with colours
edgescopecol <- edgescolor <- c('#FF0000','#FF00FF','#008000','#0000FF','#FFA500','#FFFF00')
edgescopedf <- c('national','regional / global (other)', 'bottom-up vertical Europeanisation','horizontal Europeanisation','top-down vertical Europeanisation', 'supranational Europeanisation')
df1 <- as.data.frame(edgescopecol)
df2 <- as.data.frame(edgescopedf)
library(dplyr)
df3 <- df2 %>% mutate(colour = edgescopecol)
library(readxl)
NL_edgelist_1MN <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/NL_edgelist_1MN.xlsx")
match(NL_edgelist_1MN$`act2adr`, df3$edgescopedf)
df3$colour[match(NL_edgelist_1MN$`act2adr`, df3$edgescopedf)]
NL_edgelist_1MN$colour = df3$colour[match(NL_edgelist_1MN$`act2adr`, df3$edgescopedf)]
library(xlsx)
write.xlsx(NL_edgelist_1MN, 'NL_edgelist_1MN_2.xlsx')

# How to make a legend for nodes which will be used for gephi graph.

legendNL <- c('NETHERLANDS (24.94%)','EUROPEAN UNION (13.52%)','UNITED KINGDOM (9.09%)','NA (8.62%)','FRANCE (5.13%)','GERMANY (4.9%)', 'ITALY (4.66%)','UNITED STATES (4.43%)','SPAIN (2.56%)','POLAND (2.33%)')
NLcol <- c('#FFB415','#5C9A5C','#E879A3','#FFA910','#6A886D','#717F75','#B85D6F','#EF7DB1','#AF6729','#FFE428')
png(filename="nodeslegendNL.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Node type (top 10)", legend = legendNL, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = NLcol)
dev.off()

# How to make a legend (e.g NL graph) for edges which will be used for gephi graph.

#'national' = '#FF0000'
#'regional / global (other)()' = '#FF00FF'
#'bottom-up vertical Europeanisation ()' = '#008000'
#'horizontal Europeanisation ()' = '#0000FF'
#'top-down vertical Europeanisation ()' =  '#FFA500'
#'supranational Europeanisation ()' = '#FFFF00'

edgescolor <- c('#FF0000', '#008000','#FF00FF', '#0000FF','#FFFF00','#FFA500')
edgeslegend <- c('national (37.75%)','bottom-up vertical Europeanisation (19.96%)','regional / global (other)(18.97%)','horizontal Europeanisation (12.45%)', 'supranational Europeanisation (6.52%)', 'top-down vertical Europeanisation (4.35%)')
png(filename="edgeslegendNL.png", width=900, bg="white")
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
RCAdf2 <- RCAdf %>% filter(source_country == "NETHERLANDS" & !is.na(objnat))
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
nodelist2 <- nodelist %>%  mutate(act_type = ifelse(grepl("constituency", id), "constituency/group", act_type))
nodelist3 <- unique(nodelist2)
nodelist4 <- nodelist3 %>%  mutate(Mode = ifelse(grepl("constituency", id), "2", "1"))
nodelist5 <- nodelist4 %>% relocate(Mode, .after = id)
colnames(nodelist5) <-c('Label','mode', 'act_type', 'nat')
library(xlsx)
write.xlsx(nodelist5, 'NL_nodelist_bipartite.xlsx', row.names = FALSE)

#To create edgelist ready for Gephi 
edgelist <- RCAdf3 %>% select(actorg, objnat, act2obj, EUval)
edgelist$Type <- 'Undirected'
edgelist2 <- edgelist %>% relocate(Type, .after = objnat)
colnames(edgelist2) <-c('Source','Target','Type','act2obj', 'EUeval')
write.xlsx(as.data.frame(edgelist2), 'NL_edgelist_bipartite.xlsx', row.names = FALSE)


# Then match nodes attributes to colors

library(readxl)
NL_nodelist_bipartite <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/NL_nodelist_bipartite.xlsx")
View(NL_nodelist_bipartite)
df4 <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Misc/nodeattributes(nat)2colorsdf2.xlsx")
View(nodeattributes_nat_2colorsdf2)
match(NL_nodelist_bipartite$nat, df4$act_nat)
df4$colour[match(NL_nodelist_bipartite$nat, df4$act_nat)]
NL_nodelist_bipartite$colour = df4$colour[match(NL_nodelist_bipartite$nat, df4$act_nat)]
library(xlsx)
write.xlsx(NL_nodelist_bipartite,'NL_nodelist_bipartite2.xlsx')


# Matching edge attributes with colours
edgescopecol <- edgescolor <- c('#FF0000','#FF00FF','#008000','#0000FF','#FFA500','#FFFF00')
edgescopedf <- c('national','regional / global (other)', 'bottom-up vertical Europeanisation','horizontal Europeanisation','top-down vertical Europeanisation', 'supranational Europeanisation')
df1 <- as.data.frame(edgescopecol)
df2 <- as.data.frame(edgescopedf)
library(dplyr)
df3 <- df2 %>% mutate(colour = edgescopecol)
library(readxl)
library(readxl)
NL_edgelist_bipartite <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/NL_edgelist_bipartite.xlsx")
match(NL_edgelist_bipartite$`act2obj`, df3$edgescopedf)
df3$colour[match(NL_edgelist_bipartite$`act2obj`, df3$edgescopedf)]
NL_edgelist_bipartite$colour = df3$colour[match(NL_edgelist_bipartite$`act2obj`, df3$edgescopedf)]
library(xlsx)
write.xlsx(NL_edgelist_bipartite, 'NL_edgelist_bipartite2.xlsx')

######### MAking the legends for the bipartite graph (see below)
# How to make a legend for nodes which will be used for gephi graph.

legendNL <- c('NETHERLANDS (32.94%)','EUROPEAN UNION (10.59%)','UNITED KINGDOM (9.41%)','GERMANY (5.29%)','FRANCE (4.71%)','ITALY (4.12%)','POLAND (3.53%)', 'BELGIUM (2.35%)','NA (2.35%)','SWEDEN (1.76%)')
NLcol <- c('#FFB415','#5C9A5C','#E879A3','#717F75','#6A886D','#B85D6F','#FFE428','#64638E','#FFA910','#AC5933')
png(filename="nodeslegendNLbipartite.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Node type (top 10)", legend = legendNL, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = NLcol)
dev.off()

# How to make a legend for edges which will be used for gephi graph.

#'national' = '#FF0000'
#'regional / global (other)()' = '#FF00FF'
#'bottom-up vertical Europeanisation ()' = '#008000'
#'horizontal Europeanisation ()' = '#0000FF'
#'top-down vertical Europeanisation ()' =  '#FFA500'
#'supranational Europeanisation ()' = '#FFFF00'

edgescolor <- c('#FF0000', '#008000','#FF00FF','#FFFF00', '#0000FF','#FFA500')
edgeslegend <- c('national (47.85%)','bottom-up vertical Europeanisation (25.77%)','regional / global (other) (11.04%)','supranational Europeanisation (9.82%)','horizontal Europeanisation (4.91%)', 'top-down vertical Europeanisation (0.61%)')
png(filename="edgeslegendNLbipartite.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Edge type (top 5)", legend = edgeslegend, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = edgescolor)
dev.off()


