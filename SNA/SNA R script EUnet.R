#Steps to create node and edgelist for two-mode network re Euronet (example below)

########################################### BIPARTITE NETWORK################
# To create nodelist ready for Gephi 

library(readxl)
library(dplyr)
library(readxl)
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCAdf2 <- RCAdf %>% filter(!is.na(objnat))
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
write.xlsx(nodelist5, 'EU_net_nodelist_bipartite.xlsx', row.names = FALSE)

#To create edgelist ready for Gephi 
edgelist <- RCAdf3 %>% select(actorg, objnat, act2obj, EUval)
edgelist$Type <- 'Undirected'
edgelist2 <- edgelist %>% relocate(Type, .after = objnat)
colnames(edgelist2) <-c('Source','Target','Type','act2obj', 'EUeval')
write.xlsx(as.data.frame(edgelist2), 'EU_net_edgelist_bipartite.xlsx', row.names = FALSE)


# Then match nodes attributes to colors

library(readxl)
EU_net_nodelist_bipartite <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/EU_net_nodelist_bipartite.xlsx")
df4 <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Misc/nodeattributes(nat)2colorsdf2.xlsx")
View(nodeattributes_nat_2colorsdf2)
match(EU_net_nodelist_bipartite$nat, df4$act_nat)
df4$colour[match(EU_net_nodelist_bipartite$nat, df4$act_nat)]
EU_net_nodelist_bipartite$colour = df4$colour[match(EU_net_nodelist_bipartite$nat, df4$act_nat)]
library(xlsx)
write.xlsx(EU_net_nodelist_bipartite,'EU_net_nodelist_bipartite2.xlsx')


# Matching edge attributes with colours
edgescopecol <- edgescolor <- c('#FF0000','#FF00FF','#008000','#0000FF','#FFA500','#FFFF00')
edgescopedf <- c('national','regional / global (other)', 'bottom-up vertical Europeanisation','horizontal Europeanisation','top-down vertical Europeanisation', 'supranational Europeanisation')
df1 <- as.data.frame(edgescopecol)
df2 <- as.data.frame(edgescopedf)
library(dplyr)
df3 <- df2 %>% mutate(colour = edgescopecol)
library(readxl)
library(readxl)
EU_net_edgelist_bipartite <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/EU_net_edgelist_bipartite.xlsx")
match(EU_net_edgelist_bipartite$`act2obj`, df3$edgescopedf)
df3$colour[match(EU_net_edgelist_bipartite$`act2obj`, df3$edgescopedf)]
EU_net_edgelist_bipartite$colour = df3$colour[match(EU_net_edgelist_bipartite$`act2obj`, df3$edgescopedf)]
library(xlsx)
write.xlsx(EU_net_edgelist_bipartite, 'EU_net_edgelist_bipartite2.xlsx')


######### MAking the legends for the bipartite graph (see below)
# How to make a legend for nodes which will be used for gephi graph.

legendEU <- c('ITALY (15.19%)', 'POLAND (13.92%)', 'GERMANY (11.39%)', 'NETHERLANDS (9.34%)', 'EUROPEAN UNION (8.39%)', 'UNITED KINGDOM (6.49%)', 'UNITED STATES (3.96%)', 'FRANCE (3.48%)','NA (3.16%)', 'HUNGARY (1.58%)')
EUcol <- c('#B85D6F','#FFE428','#717F75','#FFB415','#5C9A5C','#E879A3','#EF7DB1','#6A886D','#FFA910','#7F6D85')
png(filename="nodeslegendEULbipartite.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Node type (top 10)", legend = legendEU, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = EUcol)
dev.off()


# How to make a legend for edges which will be used for gephi graph.

#'national' = '#FF0000'
#'regional / global (other)()' = '#FF00FF'
#'bottom-up vertical Europeanisation ()' = '#008000'
#'horizontal Europeanisation ()' = '#0000FF'
#'top-down vertical Europeanisation ()' =  '#FFA500'
#'supranational Europeanisation ()' = '#FFFF00'

edgescolor <- c('#FF0000', '#008000','#FF00FF', '#0000FF', '#FFFF00','#FFA500')
edgeslegend <- c('national (47.64%)','bottom-up vertical Europeanisation (23.03%)','regional / global (other) (12.59%)', 'horizontal Europeanisation (7.3%)', 'supranational Europeanisation (5.87%)', 'top-down vertical Europeanisation (3.58%)')
png(filename="edgeslegendEUbipartite.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Edge type (top 5)", legend = edgeslegend, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = edgescolor)
dev.off()


############################### EU one-mode network ##########################
#Steps to create node and edgelist for one-mode network (example below)


setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset")
library(readxl)
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
library(dplyr)
RCAdf2 <- RCAdf %>% filter(!is.na(adrorg))
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
write.xlsx(as.data.frame(nodelist2), 'EU_nodelistorg_1MN.xlsx', row.names = FALSE)
# create edgelist
edgeslist <- RCAdf2 %>% select(actorg,adrorg, act2adr, adreval, EUval)
edgeslist$Type = 'Directed'
write.xlsx(as.data.frame(edgeslist), 'EU_edgelist_1MN.xlsx', row.names = FALSE)

# Then match the data
library(readxl)
EU_nodelistorg_1MN <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/EU_nodelistorg_1MN.xlsx")
df4 <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Misc/nodeattributes(nat)2colorsdf2.xlsx")
match(EU_nodelistorg_1MN$act_nat, df4$act_nat)
df4$colour[match(EU_nodelistorg_1MN$act_nat, df4$act_nat)]
EU_nodelistorg_1MN$colour = df4$colour[match(EU_nodelistorg_1MN$act_nat, df4$act_nat)]
library(xlsx)
write.xlsx(EU_nodelistorg_1MN,'EU_nodelist_1MN.xlsx')

# Matching edge attributes with colours
edgescopecol <- edgescolor <- c('#FF0000','#FF00FF','#008000','#0000FF','#FFA500','#FFFF00')
edgescopedf <- c('national','regional / global (other)', 'bottom-up vertical Europeanisation','horizontal Europeanisation','top-down vertical Europeanisation', 'supranational Europeanisation')
df1 <- as.data.frame(edgescopecol)
df2 <- as.data.frame(edgescopedf)
library(dplyr)
df3 <- df2 %>% mutate(colour = edgescopecol)
library(readxl)
EU_edgelist_1MN <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/EU_edgelist_1MN.xlsx")
match(EU_edgelist_1MN$`act2adr`, df3$edgescopedf)
df3$colour[match(EU_edgelist_1MN$`act2adr`, df3$edgescopedf)]
EU_edgelist_1MN$colour = df3$colour[match(EU_edgelist_1MN$`act2adr`, df3$edgescopedf)]
library(xlsx)
write.xlsx(EU_edgelist_1MN, 'EU_edgelist_1MN_2.xlsx')

##################### LEGENDS RE OMN#########################################################
# How to make a legend for nodes which will be used for gephi graph.

legendEU <- c('ITALY (13.73%)', 'POLAND (12.72%)', 'EUROPEAN UNION (10.04%)', 'GERMANY (9.75%)', 'NETHERLANDS (8.16%)', 'NA (8.09%)', 'UNITED KINGDOM (6.58%)', 'UNITED STATES (4.19%)', 'FRANCE (2.96%)', 'SPAIN (2.31%)')
EUcol <- c('#B85D6F','#FFE428','#5C9A5C','#717F75','#FFB415','#FFA910','#E879A3','#EF7DB1','#6A886D','#AF6729')
png(filename="nodeslegendEU.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Node type (top 10)", legend = legendEU, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = EUcol)
dev.off()

# How to make a legend (e.g NL graph) for edges which will be used for gephi graph.

#'national' = '#FF0000'
#'regional / global (other)()' = '#FF00FF'
#'bottom-up vertical Europeanisation ()' = '#008000'
#'horizontal Europeanisation ()' = '#0000FF'
#'top-down vertical Europeanisation ()' =  '#FFA500'
#'supranational Europeanisation ()' = '#FFFF00'

edgescolor <- c('#FF0000', '#008000','#FF00FF', '#0000FF','#FFA500', '#FFFF00')
edgeslegend <- c('national (35.71%)','bottom-up vertical Europeanisation (19.02%)','regional / global (other)(18.38%)','horizontal Europeanisation (13.95%)', 'top-down vertical Europeanisation (7.09%)', 'supranational Europeanisation (5.85%)')
png(filename="edgeslegendEU.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Edge type (top 5)", legend = edgeslegend, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = edgescolor)
dev.off()









