
#how to remove bracket and test within.
library(dplyr)
RCA2 <- RCAcleanedversion %>% mutate(actorg = gsub("\\s*\\([^\\)]+\\)","",as.character(RCAcleanedversion$actorg)))
RCA3 <- RCA2 %>% mutate(adrorg = gsub("\\s*\\([^\\)]+\\)","",as.character(RCA2$adrorg)))

# remove commas from every row in one column of df
actorlist3$act1 <- gsub(",","", actorlist3$act1)

# FIll in missing values from adjacent column.
RCA3$actname[is.na(RCA3$actname)] <- as.character(RCA3$actorg[is.na(RCA3$actname)])
RCA3$actorg[is.na(RCA3$actorg)] <- as.character(RCA3$actname[is.na(RCA3$actorg)])
RCA3$adrname[is.na(RCA3$adrname)] <- as.character(RCA3$adrorg[is.na(RCA3$adrname)])
RCA3$adrorg[is.na(RCA3$adrorg)] <- as.character(RCA3$adrname[is.na(RCA3$adrorg)])

# Match columns to create actor type column.

library(readr)
match(RCA3$actorg, actorlist3$act1)
actorlist3$`act1s (EUROPUB)`[match(RCA3$actorg, actorlist3$act1)]
RCA3$act_nat = actorlist3$`act1s (EUROPUB)`[match(RCA3$actorg, actorlist3$act1)]

# group_by utilisation 
RCAxlversion %>% group_by(actorg, objnat) %>% filter(source == "Corriere della Sera (Italy)" ) %>% tally(sort = TRUE) %>% na.omit()

# group-by and ggplot
RCAxlversion %>% group_by(actorg, objnat) %>% filter(source == "Corriere della Sera (Italy)" ) %>% tally(sort = TRUE) %>% na.omit() %>% ungroup() %>% slice(1:10) %>% ggplot(aes(x = actorg, y = n, fill = objnat))+ geom_col(position = "dodge")
#groupby and ggplot
RCAxlversion %>% group_by(actorg, objnat) %>% filter(source == "Corriere della Sera (Italy)" ) %>% filter(objnat == "Italy") %>% tally(sort = TRUE) %>% na.omit() %>% ungroup() %>% slice(1:10) %>% ggplot(aes(x = actorg, y = n, fill = objnat))+ geom_col(position = "dodge")

# How to amend ggplot to be in descending order
RCAxlversion %>% group_by(actorg, objnat) %>% filter(source == "Corriere della Sera (Italy)" ) %>% filter(objnat == "Italy") %>% tally(sort = TRUE) %>% na.omit() %>% ungroup() %>% slice(1:10) %>% ggplot(aes(x = reorder(actorg, -n), y = n, fill = objnat))+ geom_col(position = "dodge")

#group by act_nat and obj_nat
RCAxlversion %>% group_by(act_nat, objnat) %>% filter(objnat == "Italy") %>% tally(sort = TRUE) %>% na.omit() %>% ungroup() %>% slice(1:10) %>% ggplot(aes(x = reorder(act_nat, -n), y = n, fill = objnat))+ geom_col(position = "dodge") + coord_flip()

#groupby source and objnat
RCAxlversion %>% group_by(source, objnat) %>% tally(sort = TRUE) %>% na.omit() %>% ungroup() %>% slice(1:10) %>% ggplot(aes(x = reorder(source, -n), y = n, fill = objnat))+ geom_col(position = "dodge") + coord_flip()

## unique value of the column in R dataframe 
mydata[!duplicated(mydata$NAME), ] 

# relocate columns (example below)
RCA5 <- RCA5 %>% relocate(nprcount, .after = source)

# Removing columns using dplyr (example below)
RCA4 <- select(RCA3, -c(act_type,adr_type,act_nat, adr_nat))

#To upper case (example below)
df1$state_upper = toupper(df1$State)

# to amend character string in one column (e.g. below)
RCA7$adr_nat2[which(RCA7$adr_nat2 == "RUSSIA FEDERATION")] <- "RUSSIAN FEDERATION"

# Hot to filter for NA values and multiple values in different columns (example below)
RCA_Poland <- RCA8 %>% filter(source_country == "POLAND" & !is.na(adrorg))

# How to stack one column after another one (two columns into one)
nodes2 <- data.frame(Claimant=unlist(nodes))

# rbind with two dfs with different no. of columns
library(plyr)
nodelist <- rbind.fill(actnodelist2, objnodelist2)




