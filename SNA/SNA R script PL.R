#Steps to create node and edgelist for one-mode network (example below)

setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset")
library(readxl)
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA10clean.xlsx")
library(dplyr)
RCAdf2 <- RCAdf %>% filter(source_country == "POLAND" & !is.na(adrorg))
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
write.xlsx(as.data.frame(nodelist2), 'PL_nodelistorg_1MN.xlsx', row.names = FALSE)

#  Alternatively, to create a nodelist according to actor ID/name, do the following:
RCAdf2 <- RCAdf %>% filter(source_country == "POLAND" & !is.na(adrorg))
nodelist <- RCAdf2 %>% select(actname, actorg, act_type, act_nat, adrname, adrorg, adr_type, adr_nat)
actnodelist <- nodelist %>% select(actname, actorg,act_type,act_nat)
adrnodelist <- nodelist %>% select(adrname, adrorg,adr_type,adr_nat)
colnames(actnodelist) <-c('id', 'orgid', 'act_type', 'act_nat')
colnames(adrnodelist) <-c('id', 'orgid', 'act_type', 'act_nat')
nodelist2 <- rbind(actnodelist, adrnodelist)
nodelist3 <-unique(nodelist2)
colnames(nodelist2) <-c('id', 'orgid', 'act_type', 'act_nat')
library(xlsx)
write.xlsx(as.data.frame(nodelist3), 'PL_nodelistID_1MN.xlsx', row.names = FALSE)

# create edgelist
edgeslist <- RCAdf2 %>% select(actname,actorg,adrname, adrorg, act2adr, adreval, EUval)
edgeslist$Type = 'Directed'
write.xlsx(as.data.frame(edgeslist), 'PL_edgelist_1MN.xlsx', row.names = FALSE)


#How to add pre-assigned hex values (i.e. colors) to node attributes. This is useful because it means
# every graph I use can have the same colours for the same node attributes.
# E.g. Germany == 'red' (always) and Poland == 'yellow. 
# Firstly, one need to create a color vector specifying no. of colours they 
# wish to use. As Brewer.pal often does not have >15 colors, there is a 
# useful workround. See below...
# If one wants a range of 20 colors, one has to do the following:

library(RColorBrewer)
nb.cols <- 20
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
df$color <- col[as.numeric(as.factor(df$`act2adr scope`))]

# A similar process applies to node attributes color setting. Let's give it a 
# with the original RCA dataset for act/adr/obj nationality/country attributes.

library(readxl)
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA10clean.xlsx")
actdf <- RCAdf %>% select(actorg,act_nat)
adrdf <- RCAdf %>% select(adrorg,adr_nat)
objdf <- RCAdf %>% select(objnat)
objdf <- na.omit(objdf)

# Make a duplicate column
objdf$nat <- objdf$objnat
objdf$objnat <- tolower((objdf$objnat))
objdf2 <- objdf[!grepl("UNSPECIFIED", objdf$nat),]
objdf2 <- objdf[!grepl("unspecified", objdf$objnat),]
objdf2$objnat <- paste0(objdf2$objnat, " constituency")
#rename columns so they match then rbind the 3 dfs into one df.
colnames(adrdf) <-c('actorg','act_nat')
colnames(objdf2) <-c('actorg','act_nat')
rbind(actdf,adrdf, objdf2)
df3 <- rbind(actdf,adrdf, objdf2)
df4 <- unique(df3)
# Count no. of unique data values 
length(unique(df4$act_nat))

# Result is 80 different nationalities/country node attributes. Now I
# need to assigned colours to the 80 node attributes.
df4$colour <- ""
library(RColorBrewer)
nb.cols <- 77
col <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)
df4$colour <- col[as.numeric(as.factor(df4$act_nat))]
length(unique(df4$colour))
#Save nationality node attribute/colour df.
library(xlsx)
write.xlsx(df4, 'nodecolorattributes.xlsx')

# Then I need to mutate & match column of nodelist (for DE,IT,NL,PL) 
# with the df4 dataframe.
match(PL_nodelistorg_1MN$act_nat, df4$act_nat)
df4$colour[match(PL_nodelistorg_1MN$act_nat, df4$act_nat)]
PL_nodelistorg_1MN$colour = df4$colour[match(PL_nodelistorg_1MN$act_nat, df4$act_nat)]
library(xlsx)
write.xlsx(PL_nodelistorg_1MN,'PL_nodelist_1MN.xlsx')

# How to make a legend for nodes (e.g. PL graph) which will be used for gephi graph.
legendPL <- c('POLAND (29.79%)','EUROPEAN UNION (10.82%)','NA (8.51%)','GERMANY (7.09%)','UNITED KINGDOM (6.56%)','UNITED STATES (5.5%)', 'ITALY (4.96%)','SPAIN (3.37%)','BELARUS (2.66%)','RUSSIA (2.66%)')
PLcol <- c('#FFE428','#5C9A5C','#FFA910','#717F75','#E879A3','#EF7DB1','#B85D6F','#AF6729','#745A80','#E8D430')
png(filename="nodeslegendPL.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Node type (top 10)", legend = legendPL, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = PLcol)
dev.off()

# Matching edge attributes with colours
edgescopecol <- edgescolor <- c('#FF0000','#FF00FF','#008000','#0000FF','#FFA500','#FFFF00')
edgescopedf <- c('national','regional / global (other)', 'bottom-up vertical Europeanisation','horizontal Europeanisation','top-down vertical Europeanisation', 'supranational Europeanisation')
df1 <- as.data.frame(edgescopecol)
df2 <- as.data.frame(edgescopedf)
library(dplyr)
df3 <- df2 %>% mutate(colour = edgescopecol)
library(readxl)
PL_edgelist_1MN <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Poland/one-mode networks/PL_edgelist_1MN.xlsx")
match(PL_edgelist_1MN$`act2adr`, df3$edgescopedf)
df3$colour[match(PL_edgelist_1MN$`act2adr`, df3$edgescopedf)]
PL_edgelist_1MN$colour = df3$colour[match(PL_edgelist_1MN$`act2adr`, df3$edgescopedf)]
library(xlsx)
write.xlsx(PL_edgelist_1MN, 'PL_edgelist_1MN_2.xlsx')

# How to make a legend (e.g NL graph) for edges which will be used for gephi graph.
edgescolor <- c('#FF0000','#FF00FF','#008000','#0000FF','#FFA500','#FFFF00')
edgeslegend <- c('national (38.42%)','regional / global (other) (19.07%)', 'bottom-up vertical Europeanisation (14.41%)','horizontal Europeanisation (13.7%)','top-down vertical Europeanisation (8.9%)', 'supranational Europeanisation (5.51%)')
png(filename="edgeslegendPL.png", width=900, bg="white")
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
RCAdf2 <- RCAdf %>% filter(source_country == "POLAND" & !is.na(objnat))
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
write.xlsx(nodelist5, 'PL_nodelist_bipartite.xlsx', row.names = FALSE)

#To create edgelist ready for Gephi 
edgelist <- RCAdf3 %>% select(actorg, objnat, act2obj, EUval)
edgelist$Type <- 'Undirected'
edgelist2 <- edgelist %>% relocate(Type, .after = objnat)
colnames(edgelist2) <-c('Source','Target','Type','act2obj', 'EUeval')
write.xlsx(as.data.frame(edgelist2), 'PL_edgelist_bipartite.xlsx', row.names = FALSE)





# Then match nodes attributes to colors

library(readxl)
PL_nodelist_bipartite <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Poland/two-mode networks/PL_nodelist_bipartite.xlsx")
df4 <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Misc/nodeattributes(nat)2colorsdf2.xlsx")
View(nodeattributes_nat_2colorsdf2)
match(PL_nodelist_bipartite$nat, df4$act_nat)
df4$colour[match(PL_nodelist_bipartite$nat, df4$act_nat)]
PL_nodelist_bipartite$colour = df4$colour[match(PL_nodelist_bipartite$nat, df4$act_nat)]
library(xlsx)
write.xlsx(PL_nodelist_bipartite,'PL_nodelist_bipartite2.xlsx')

# Matching edge attributes with colours
edgescopecol <- edgescolor <- c('#FF0000','#FF00FF','#008000','#0000FF','#FFA500','#FFFF00')
edgescopedf <- c('national','regional / global (other)', 'bottom-up vertical Europeanisation','horizontal Europeanisation','top-down vertical Europeanisation', 'supranational Europeanisation')
df1 <- as.data.frame(edgescopecol)
df2 <- as.data.frame(edgescopedf)
library(dplyr)
df3 <- df2 %>% mutate(colour = edgescopecol)
library(readxl)
PL_edgelist_bipartite <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Poland/two-mode networks/PL_edgelist_bipartite.xlsx")
View(PL_edgelist_bipartite)
match(PL_edgelist_bipartite$`act2obj`, df3$edgescopedf)
df3$colour[match(PL_edgelist_bipartite$`act2obj`, df3$edgescopedf)]
PL_edgelist_bipartite$colour = df3$colour[match(PL_edgelist_bipartite$`act2obj`, df3$edgescopedf)]
library(xlsx)
write.xlsx(PL_edgelist_bipartite, 'PL_edgelist_bipartite2.xlsx')


######### MAking the legends for the bipartite graph (see below)


# How to make a legend for nodes which will be used for gephi graph.
legendPL <- c('POLAND (32.69%)','EUROPEAN UNION (9.23%)','UNITED KINGDOM (7.69%)','GERMANY (7.31%)','UNITED STATES (5.77%)','ITALY (5.77%)','BELARUS (3.08%)', 'FRANCE (2.69%)','HUNGARY (2.69%)','SWEDEN (2.31%)')
PLcol <- c('#FFE428','#5C9A5C','#E879A3','#717F75','#EF7DB1','#B85D6F','#745A80','#6A886D','#7F6D85','#AC5933')
png(filename="nodeslegendPLbipartite.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Node type (top 10)", legend = legendPL, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = PLcol)
dev.off()

# How to make a legend (e.g NL graph) for edges which will be used for gephi graph.

#'national' = '#FF0000'
#'regional / global (other)()' = '#FF00FF'
#'bottom-up vertical Europeanisation ()' = '#008000'
#'horizontal Europeanisation ()' = '#0000FF'
#'top-down vertical Europeanisation ()' =  '#FFA500'
#'supranational Europeanisation ()' = '#FFFF00'

edgescolor <- c('#FF0000', '#008000','#FF00FF', '#0000FF','#FFA500','#FFFF00')
edgeslegend <- c('national (52.08%)','bottom-up vertical Europeanisation (17.74%)','regional / global (other) (9.81%)','horizontal Europeanisation (9.06%)', 'top-down vertical Europeanisation (6.42%)', 'supranational Europeanisation (4.91%)')
png(filename="edgeslegendPLbipartite.png", width=900, bg="white")
par(mar=c(5,6,4,1)+.1)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Edge type (top 5)", legend = edgeslegend, pch = 16, pt.cex = 3, cex=1.5, bty='n', col = edgescolor)
dev.off()

#######################################################

#amend nodelist csv to include parfam node attribute column for OMN.
#Match nodelist with polfam csv.
library(readxl)
PL_nodelist_1MN <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Poland/one-mode networks/PL_nodelist_1MN.xlsx", sheet = 3)
RCA15 <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA15.xlsx")
View(RCA15)

# Match variables from aforementioned datasets
match(PL_nodelist_1MN$Label, RCA15$actorg)
RCA15$parfam[match(PL_nodelist_1MN$Label, RCA15$actorg)]
PL_nodelist_1MN$parfam = RCA15$parfam[match(PL_nodelist_1MN$Label, RCA15$actorg)]

# Then create noselist with parfam attribute
write.xlsx(PL_nodelist_1MN, 'parfam.xlsx')











