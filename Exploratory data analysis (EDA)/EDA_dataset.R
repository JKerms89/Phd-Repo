# Set file location and load packages for data manipulation

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(hrbrthemes)
library(viridis)

RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")

# Count no. of scope variables according to actor type.
library(dplyr)
natscopedist <- RCAdf %>% group_by(act_type) %>% count(act2adr, sort = TRUE) %>% filter(act2adr == 'national')
# Create a dotplot to examine national scope distribution across different types of actors.
dotchart1 <- dotchart(natscopedist$n, labels=natscopedist$act_type, cex =.7, main = "natscopefreqdist", xlab="national act2adr scope count")

# Count according to source 
natscopedlist <- RCAdf %>% count(source, act2adr, sort = TRUE) %>% filter(act2adr == 'national')
dotchart2 <- dotchart(natscopedlist$n, labels=natscopedlist$source, cex =.7, main = "natscopefreqdist", xlab="national act2adr scope count")

# Calculate mean euval according to act_nat

euvalmean <- RCAdf %>% 
  group_by(act_nat) %>% 
  dplyr::summarize(Mean = mean(EUval))


# Create barplot to see which act2adr scopes are the most prevalent

distscopes <- RCAdf %>% count(source_country, act2adr, sort = TRUE) %>% filter(!is.na(act2adr))
library(ggplot2)
# Basic barplot
p1<-ggplot(data=distscopes, aes(fill = source_country, x=act2adr, y=n)) +
  geom_bar(stat="identity") + coord_flip()

# Create barplot to see which scopes are the most prevalent according to country

distscopes2 <- RCAdf %>% count(source_country, act2adr, sort = TRUE) %>% filter(!is.na(act2adr))
library(ggplot2)
# Basic barplot
p2<-ggplot(data=distscopes2, aes(fill = act2adr, x=source_country, y=n)) +
  geom_bar(stat="identity", position = "dodge") +
  coord_flip()


# Create barplot to see which scopes are the most prevalent according to newspaper
distscopes3 <- RCAdf %>% count(source, act2adr, sort = TRUE) %>% filter(!is.na(act2adr))
library(ggplot2)
# Basic barplot
p3<-ggplot(data=distscopes3, aes(fill = act2adr, x=source, y=n)) +
  geom_bar(stat="identity", position = "dodge") + coord_flip()
  
# count of national scopes over time to create hist line graph
natscope_t <- RCAdf %>% count(time, act2adr) %>% filter(!is.na(act2adr) & act2adr =='national')
natscope_t2 <- na.omit(natscope_t)

# Plot the bar chart.
ggplot(natscope_t2, aes(x = time, y = n, colour = act2adr)) +
  geom_line(stat= "identity")

# to create nat scope freqs column
bild <- RCAdf %>% count(source, act2adr) %>% filter(!is.na(act2adr)) %>% filter(source == 'Bild') %>%
  mutate(freq = n/sum(n))

cds <- RCAdf %>% count(source, act2adr) %>% filter(!is.na(act2adr)) %>% filter(source == 'Corriere della Sera (Italy)') %>%
  mutate(freq = n/sum(n))

# Match the framing typology dataframe with RCAdf frame/justification column.
match(RCAdf$frame, framesdf$Frame)
framesdf$`Framing typology`[match(RCAdf$frame, framesdf$Frame)]
RCAdf$frame_type = framesdf$`Framing typology`[match(RCAdf$frame, framesdf$Frame)]

# Explore relation between national scopes and frame prevalence between country
frame_scope <- RCAdf %>% count(frame_type, act2obj, source_country, sort  = TRUE) %>% filter(act2obj == 'national' & !is.na(frame_type))
p4<-ggplot(data=frame_scope, aes(fill = source_country, x=frame_type, y=n)) +
  geom_bar(stat="identity", position = "dodge") + coord_flip()

# Explore relation between all scopes and frame prevalence between country
frame_scope <- RCAdf %>%
  filter(!is.na(act2obj) & !is.na(frame_type)) %>%
           count(frame_type, act2obj, source_country, sort  = TRUE) 
p5<-ggplot(data=frame_scope, aes(fill = act2obj, x=frame_type, y=n)) +
  geom_bar(stat="identity", position = "dodge") + coord_flip()

# Explore relation between scope and sovereignty frame between country
library(dplyr)
frame_scope <- RCAdf %>%
  filter(!is.na(act2obj) & frame_type == "sovereignty") %>% 
  count(frame_type, act2obj, source_country, sort  = TRUE)
p6<-ggplot(data=frame_scope, aes(fill = source_country, x=act2obj, y=n)) +
  geom_bar(stat="identity") + coord_flip() + ggtitle("type of discursive scope for the sovereignty frame")

# Explore relation between scope and freedom/equality frame between country
frame_scope2 <- RCAdf %>% count(frame_type, act2obj, source_country, sort  = TRUE) %>% filter(!is.na(act2obj) & frame_type == "protection of human dignity")
p7<-ggplot(data=frame_scope2, aes(fill = source_country, x=act2obj, y=n)) +
  geom_bar(stat="identity") + coord_flip() + ggtitle("type of discursive scope for the protection of human dignity")

# Explore relation between scope and freedom/equality frame between country
frame_scope3 <- RCAdf %>% count(frame_type, act2obj, source_country, sort  = TRUE) %>% filter(!is.na(act2obj) & frame_type == "principles, norms, values, culture & identity")
p8<-ggplot(data=frame_scope3, aes(fill = source_country, x=act2obj, y=n)) +
  geom_bar(stat="identity") + coord_flip() + ggtitle("type of discursive scope for principles, norms, values, culture & identity")

#Facet_wrap according to frame_type (filtering for instrumental as there are disproportionately high level of frames in this category)
frame_scope4 <- RCAdf %>%
  count(frame_type, act2obj, source_country, sort  = TRUE) %>%
  filter(!is.na(act2obj) & !is.na(frame_type) & frame_type != 'instrumental / utilitarian')
facetp<-ggplot(data=frame_scope4, aes(fill = source_country, x=act2obj, y=n)) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_wrap(~ frame_type)


#Create a segmented bar chart re country
library(ggplot2)
library(dplyr)
RCAfiltered <- RCAdf %>% filter(!is.na(act2obj)) 
p9 <- ggplot(RCAfiltered, aes(x = source_country, fill = act2obj)) + 
           geom_bar(position = "fill")

#Create a segmented bar chart re newspaper
library(ggplot2)
library(dplyr)
RCAfiltered <- RCAdf %>% filter(!is.na(act2obj)) 
p10 <- ggplot(RCAfiltered, aes(x = source, fill = act2obj)) + 
  geom_bar(position = "fill") + coord_flip()




#################################08/06





#Reorder segmented bar chart according to national scope prominence 
RCAfiltered <- RCAdf %>% filter(!is.na(act2obj))
p11 <- ggplot(RCAfiltered, aes(x = factor(source, 
                                          levels = c("Bild", "De Telegraaf", "Fact Poland", "Gazeta Wyborcza", "La Nazione (Italy)", "Corriere della Sera (Italy)", "NRC Handelsblad", 	
                                                     "Süddeutsche Zeitung" )),
                                          fill = act2obj)) + 
  geom_bar(position = "fill") + coord_flip()

# summarise data and create barplot of mean EUeval and act2obj variable
RCAevalfilter <- RCAdf %>% filter(EUval == -1 | EUval==1)
RCAeval <- RCAevalfilter %>% 
  group_by(act2obj) %>%
  filter(!is.na(act2obj)) %>%
  summarise(meanEUeval = mean(EUval)) %>%
  ggplot(., aes(x = act2obj, y = meanEUeval)) + geom_bar(stat = "identity") +
  coord_flip()

# summarise data and create barplot of mean EUeval and act2adr variable
RCAevalfilter <- RCAdf %>% filter(EUval == -1 | EUval==1)
RCAevalfilter %>% 
  group_by(act2adr) %>%
  filter(!is.na(act2adr)) %>%
  summarise(meanEUeval = mean(EUval)) %>%
  ggplot(., aes(x = act2adr, y = meanEUeval)) + geom_bar(stat = "identity") +
  coord_flip()



# summarise data and create barplot of mean adreval and act2obj variable
RCAevalfilter <- RCAdf %>% filter(adreval == -1 | adreval==1)
RCAevalfilter %>% 
  group_by(act2obj) %>%
  filter(!is.na(act2obj)) %>%
  summarise(meanadreval = mean(adreval)) %>%
  ggplot(., aes(x = act2obj, y = meanadreval)) + geom_bar(stat = "identity") +
  coord_flip()

# summarise data and create barplot of mean EUeval and act2adr variable
RCAevalfilter <- RCAdf %>% filter(EUval == -1 | EUval==1)
RCAevalfilter %>% 
  group_by(act2adr) %>%
  filter(!is.na(act2adr)) %>%
  summarise(meanadreval = mean(EUval)) %>%
  ggplot(., aes(x = act2adr, y = meanadreval)) + geom_bar(stat = "identity") +
  coord_flip()

# summarise data and create barplot of mean adreval and act2adr variable
RCAevalfilter <- RCAdf %>% filter(adreval == -1 | adreval==1)
RCAevalfilter %>% 
  group_by(act2adr) %>%
  filter(!is.na(act2adr)) %>%
  summarise(mean = mean(adreval)) %>%
  ggplot(., aes(x = act2adr, y = mean)) + geom_bar(stat = "identity") +
  coord_flip()

# summarise data and create barplot of mean EUeval and acttype variable
  RCAmeaneval <- RCAdf %>% 
  group_by(act_type) %>%
  filter(!is.na(actorg) & EUval == 1 | EUval == -1) %>%
  summarise(mean = mean(EUval))

RCAeval2 = RCAmeaneval[order(RCAmeaneval$mean),]
p12 <- dotchart(RCAeval2$mean, labels = RCAeval2$act_type, cex = 0.6, xlab = "mean EU evaluation by actor type")

# summarise data and create barplot of mean adreval and acttype variable
RCAmeanadreval <- RCAdf %>% 
  group_by(act_type) %>%
  filter(!is.na(actorg) & adreval == 1 | adreval == -1) %>%
  summarise(mean = mean(adreval))
  
RCAevalorder = RCAmeanadreval[order(RCAmeanadreval$mean),]

p13 <- dotchart(RCAevalorder$mean, labels = RCAevalorder$act_type, 
                cex = 0.6, xlab = "mean adreval by actor type")

# summarise data and create barplot of mean EUeval and actnat variable
objnat_EUeval <- RCAdf %>% 
  group_by(objnat) %>%
  summarise(mean = mean(EUval))

objnat_EUeval2 = objnat_EUeval[order(objnat_EUeval$mean),]
p14 <- dotchart(objnat_EUeval2$mean, labels = objnat_EUeval2$objnat, cex = 0.6, xlab = "mean EU evaluation by actor type")

# Count by country and frame type to explore 'common frames of reference' across different mediated spheres

RCAdf %>% count(frame_type, source_country) %>% 
  filter(!is.na(frame_type)) %>%
  ggplot(., aes(x = frame_type, y = n, fill = source_country)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()

# Count by frame type and act2obj
RCAdf %>% filter(frame_type == "sovereignty") %>%
                 count(act2obj, frame_type)

# Count by act2obj == national and actnat
RCAdf %>% filter(act2obj == 'national') %>% 
  count(act2obj, act_nat, sort = TRUE)

# Count most visible actors by source country
RCAdf %>% count(actorg, source_country, sort = TRUE) 

# Count by act type and scope

RCAdf %>% 
  filter(!is.na(act2adr)) %>%
  ggplot(., aes(x = act_type,fill = act2adr)) +
  geom_bar(position = "fill") +
  coord_flip()

# Count by newspaper and scope 
RCAdf %>% 
  filter(!is.na(act2adr)) %>%
  ggplot(., aes(x = source,fill = act2adr)) +
  geom_bar(position = "fill") +
  coord_flip()

# Create new column to compare between prior to and during covid.
RCA13 <-  RCAdf %>% mutate(period = ifelse(grepl("pre-Covid", `document title`), "pre-Covid", "Covid"))
# Create barplot
tcompdf <- RCA13 %>% filter(!is.na(act2obj))
p15 <- ggplot(tcompdf,aes(x = period, fill = act2obj)) +
geom_bar(position = "fill") + facet_wrap(~ source_country)

                          
# Or alternatively, create barplot wherein the period if the filler.
tcompdf <- RCAdf %>% filter(!is.na(act2obj))
p16<- ggplot(tcompdf,aes(x = act2obj, fill = period)) + 
  geom_bar(position = "fill") + coord_flip()

# As the latter is the most visually useful, I will use this for the act2adr variable
tcompdf2<- RCAdf %>% filter(!is.na(act2adr))
p17<-ggplot(tcompdf2,aes(x = act2adr, fill = period)) + 
  geom_bar(position = "fill") + coord_flip()

# As we can see, the results are similar across both scopes. National scopes are slightly more
# prominent during the crisis, and european scopes fall, in the main. 

# Count most visible actors by source country
RCA11 %>% count(actorg, source_country, sort = TRUE) 

# Count by act type and obj scope

RCAdf %>% 
  filter(!is.na(act2obj)) %>%
  ggplot(., aes(x = act_type,fill = act2obj)) +
  geom_bar(position = "fill") +
  coord_flip()

# Count by newspaper and obj scope 
RCAdf %>% 
  filter(!is.na(act2obj)) %>%
  ggplot(., aes(x = source,fill = act2obj)) +
  geom_bar(position = "fill") +
  coord_flip()

# Make new newspaper type column
RCA12 <- RCA11 %>% mutate(newsp_type = ifelse(source == "Gazeta Wyborcza" | source == "Corriere della Sera (Italy)" | source == "NRC Handelsblad" | source == "Süddeutsche Zeitung", "quality", "tabloid"))
RCA13 <- RCA12 %>% relocate(newsp_type, .after = source_country)

# Compare act2obj scopes between tabloid and quality newspapers

RCAdf %>% 
  filter(!is.na(act2obj)) %>%
  ggplot(., aes(x = newsp_type,fill = act2obj)) +
  geom_bar(position = "fill") 

# act2adr scope and format

RCAdf %>% 
  filter(!is.na(act2adr)) %>%
  ggplot(., aes(x = newsp_type, fill = act2adr)) +
  geom_bar(position = "fill")


# Comparing EU evaluation across formats

euvalmean <- RCAdf %>% 
  group_by(newsp_type) %>% 
  dplyr::summarize(MeanEUeval = mean(EUval)) %>%
  ggplot(., aes(x = newsp_type, y = MeanEUeval, fill = newsp_type)) + 
           geom_bar(stat = "identity") 

# Comparing adr evalation by format

adrvalmean <- RCAdf %>% 
  group_by(newsp_type) %>% 
  dplyr::summarize(Meanadreval = mean(adreval)) %>%
  ggplot(., aes(x = newsp_type, y = Meanadreval, fill = newsp_type)) + 
  geom_bar(stat = "identity")

# Create ggplot for frame_type according to country.
frame_scope <- RCAdf %>% filter(!is.na(frame_type))
p17 <-ggplot(data=frame_scope, aes(x=frame_type, fill = source_country)) + geom_bar(position ="fill") + coord_flip()

# OR...

frame_scope <- RCAdf %>% filter(!is.na(frame_type))
p18 <-ggplot(data=frame_scope, aes(x=source_country, fill = frame_type)) + geom_bar(position ="fill")+ coord_flip()

# From the above graph (p18), we can conclude that EU politics is similarly framed. However, that only tells us halfthe story. 
# In terms of object scope framing, framing is overwhelmingly national in scope. in other words, similar frames are used but in reference to different 'imagined communities', i.e. claiming
# to represent different constituencies. 

# Create a plot showing which actors use national act2adr scopes 
# the most prevalent. The code was more complicated here.
library(forcats)
library(dplyr)
library(ggplot2)
dfordered <- RCAdf %>% 
  filter(!is.na(act2adr)) %>%
  count(act2adr, act_type) %>%
  ungroup() %>%
  group_by(act_type) %>%
  mutate(rel.freq = n / sum(n))
dfordered2 = dfordered %>% 
  arrange(fct_relevel(act2adr, "national"), rel.freq) %>%
  mutate(act_type = fct_inorder(act_type)) 
ggplot(dfordered2, aes(x = fct_inorder(act_type), y = rel.freq, fill = act2adr)) +
  geom_bar(stat = "identity") + coord_flip()

# Create a plot showing which actors use national act2obj scopes 
# are the most prevalent.

dfordered <- RCAdf %>% 
  filter(!is.na(act2obj)) %>%
  count(act2obj, act_type) %>%
  ungroup() %>%
  group_by(act_type) %>%
  mutate(rel.freq = n / sum(n))
dfordered2 = dfordered %>% 
  arrange(fct_relevel(act2obj, "national"), rel.freq) %>%
  mutate(act_type = fct_inorder(act_type)) 
ggplot(dfordered2, aes(x = fct_inorder(act_type), y = rel.freq, fill = act2obj)) +
  geom_bar(stat = "identity") + coord_flip()


# Compare across newspapers which of them invoke national object scopes the most as a proportion of total n of scopes.

dfordered <- RCAdf %>% 
  filter(!is.na(act2obj)) %>%
  count(act2obj, source) %>%
  ungroup() %>%
  group_by(source) %>%
  mutate(rel.freq = n / sum(n))
dfordered2 = dfordered %>% 
  arrange(fct_relevel(act2obj, "national"), rel.freq) %>%
  mutate(source = fct_inorder(source)) 
ggplot(dfordered2, aes(x = fct_inorder(source), y = rel.freq, fill = act2obj)) +
  geom_bar(stat = "identity") + coord_flip()

# Compare across newspapers which of them invoke national act2adr scopes the most as a proportion of total n of scopes.

dfordered <- RCAdf %>% 
  filter(!is.na(act2adr)) %>%
  count(act2adr, source) %>%
  ungroup() %>%
  group_by(source) %>%
  mutate(rel.freq = n / sum(n))
dfordered2 = dfordered %>% 
  arrange(fct_relevel(act2adr, "national"), rel.freq) %>%
  mutate(source = fct_inorder(source)) 
ggplot(dfordered2, aes(x = fct_inorder(source), y = rel.freq, fill = act2adr)) +
  geom_bar(stat = "identity") + coord_flip()


# Compare across countries which of them invoke national object scopes the most as a proportion of total n of scopes.

dfordered <- RCAdf %>% 
  filter(!is.na(act2obj)) %>%
  count(act2obj, source_country) %>%
  ungroup() %>%
  group_by(source_country) %>%
  mutate(rel.freq = n / sum(n))
dfordered2 = dfordered %>% 
  arrange(fct_relevel(act2obj, "national"), rel.freq) %>%
  mutate(source_country = fct_inorder(source_country)) 
ggplot(dfordered2, aes(x = fct_inorder(source_country), y = rel.freq, fill = act2obj)) +
  geom_bar(stat = "identity") + coord_flip()

# Compare across countries which of them invoke national act2adr scopes the most as a proportion of total n of scopes.

dfordered <- RCAdf %>% 
  filter(!is.na(act2adr)) %>%
  count(act2adr, source_country) %>%
  ungroup() %>%
  group_by(source_country) %>%
  mutate(rel.freq = n / sum(n))
dfordered2 = dfordered %>% 
  arrange(fct_relevel(act2adr, "national"), rel.freq) %>%
  mutate(source_country = fct_inorder(source_country)) 
ggplot(dfordered2, aes(x = fct_inorder(source_country), y = rel.freq, fill = act2adr)) +
  geom_bar(stat = "identity") + coord_flip()

# Continued...
library(readxl)
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf")
RCA13 <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA13.xlsx")


# Match the issue field dataframe with iss field df column of main dataset.
match(RCA12$issfield, issuefield$issue)
issuefield$issue_field[match(RCA12$issfield, issuefield$issue)]
RCA12$issfield2 = issuefield$issue_field[match(RCA12$issfield, issuefield$issue)]

# Explore the data re issue field and discursive scope of the claim.
library(ggplot2)
RCAdf %>% filter(!is.na(act2adr)) %>%
  ggplot(., aes(x = issfield, fill = act2adr)) +
  geom_bar(position = "fill") + coord_flip()

# Put the bars in order according to national scope (issue field)

iss_act2adr <- RCAdf %>% 
  filter(!is.na(act2adr)) %>%
  count(act2adr, issfield) %>%
  ungroup() %>%
  group_by(issfield) %>%
  mutate(rel.freq = n / sum(n))
iss_act2adr2 = iss_act2adr %>% 
  arrange(fct_relevel(act2adr, "national"), rel.freq) %>%
  mutate(issfield = fct_inorder(issfield)) 
ggplot(iss_act2adr2, aes(x = fct_inorder(issfield), y = rel.freq, fill = act2adr)) +
  geom_bar(stat = "identity") + coord_flip()


# Explore the data re issue field and source country to see if 'same issues' are discussed.
library(ggplot2)
RCAdf %>% 
  ggplot(., aes(x = source_country, fill = issfield)) +
  geom_bar(position = "fill") 

# Another representation of the same variables
RCAdf %>% 
  ggplot(., aes(x = issfield, fill = source_country)) +
  geom_bar(position = "fill") + coord_flip()

# Put the bars in order according to national obj scope (issue field)
library(forcats)
iss_act2obj <- RCAdf %>% 
  filter(!is.na(act2obj)) %>%
  count(act2obj, issfield) %>%
  ungroup() %>%
  group_by(issfield) %>%
  mutate(rel.freq = n / sum(n))
iss_act2obj2 = iss_act2obj %>% 
  arrange(fct_relevel(act2obj, "national"), rel.freq) %>%
  mutate(issfield = fct_inorder(issfield)) 
ggplot(iss_act2obj2, aes(x = fct_inorder(issfield), y = rel.freq, fill = act2obj)) +
  geom_bar(stat = "identity") + coord_flip()

# ggplot to see which issues were the most prevalent by country.
library(ggplot2)
library(dplyr)
RCAdf %>% count(issfield, source_country) %>%
ggplot(., aes(x = source_country, y = n, fill = issfield)) +
         geom_bar(stat = "identity", position = "dodge")

# Alternative representation wherein the fill is source_country
RCAdf %>% count(issfield, source_country) %>%
  ggplot(., aes(x = issfield, y = n, fill = source_country)) +
  geom_bar(stat = "identity") + coord_flip()

# Alternative representation wherein the fill is source_country
RCAdf %>% count(issfield, source_country) %>%
  ggplot(., aes(x = reorder(issfield, -n), y = n, fill = n)) +
  geom_bar(stat = "identity") + coord_flip() + facet_wrap(~ source_country)


# create ggplot with frame and issue field
RCAdf %>% 
  filter(!is.na(frame_type)) %>%
  ggplot(., aes(x = issfield, fill = frame_type)) +
  geom_bar(position = "fill") + coord_flip()

# Are actors more critical / negative valence of EU in issue fields which are more 'Europeanised?'

RCAeval <- RCAdf %>% filter(EUval == -1 | EUval==1)
RCAeval %>% 
  group_by(issfield) %>%
  summarise(EUeval = mean(EUval)) %>%
  ggplot(., aes(x = reorder(issfield, -EUeval), y = EUeval, fill = EUeval)) + geom_bar(stat = "identity") +
  coord_flip()

# Include a facet wrap to compare across countries
# reorder_within and scale_x_reordered work.
# (Note that you need to set scales = "free_x" in the facet)

library(tidytext)
RCAeval %>% 
  group_by(issfield, source_country) %>%
  summarise(EUeval = mean(EUval)) %>%
  ggplot(., aes(x = reorder(issfield, -EUeval), y = EUeval, fill = EUeval)) + geom_bar(stat = "identity") +
  coord_flip() + 
  facet_wrap(~ source_country)

# If I want to reorder_within and scale_x_reordered work.
# (Note that you need to set scales = "free_x" in the facet)
# Here is the link on how to do this: https://www.r-bloggers.com/2019/12/how-to-reorder-arrange-bars-with-in-each-facet-of-ggplot/

# Use a mosaic plot to examine correlation between 3 categorical variables
# plotting categorical data - mosaic plots
# Create mosaic plot using dplyr
library(dplyr)
library(ggplot2)
library(ggmosaic)
mosaicdf <- RCAdf %>% mutate(
  act2adr = factor(act2adr),  
  source_country = factor(source_country) 
)
mosaicdf2 <- mosaicdf %>% count(act2adr, source_country) %>% filter(!is.na(act2adr))
mosaicdf2 %>% 
  ggplot() + 
  geom_mosaic(aes(x = product(source_country), fill = act2adr, weight = n)) +
  xlab("act2adr") + 
  ylab("count") + 
  ggtitle("Distribution of act_type and discursive scope in the RCA dataset")


# Here is another example (to be continued tomorrow)
#https://r-graphics.org/recipe-miscgraph-mosaic 
#https://www2.stat.duke.edu/courses/Summer17/sta101.001-2/uploads/project/BarPlots.html 

library(vcd)
library(dplyr)
RCA13 <- RCAdf %>% filter(!is.na(act2adr))
mosaic( ~ source_country + act2adr + newsp_type, data = RCA13)

#Alternative way which is colour coded.
mosaic( ~ act2adr + source_country + period, data = RCA13,
        highlighting = "period", highlighting_fill = c("lightblue", "pink"),
        direction = c("v","h","v"), labeling= labeling_border(rot_labels = c(90,0,0,0), 
                                                             just_labels = c("left", 
                                                                             "center", 
                                                                             "center", 
                                                                             "center")))

#In order to rotate the labels

labeling= labeling_border(rot_labels = c(90,0,0,0), 
                          just_labels = c("left", 
                                          "center", 
                                          "center", 
                                          "center"))

# Do the same for act2obj, country and period.
library(vcd)
library(dplyr)
RCA13 <- RCA13 %>% filter(!is.na(act2obj)) 
mosaic( ~ source_country + act2obj + period, data = RCA13,
        highlighting = "period", highlighting_fill = c("lightblue", "pink"),
        direction = c("v","h","v"), gp_varnames = gpar(fontsize = 14, fontface = 1),
        gp_labels = gpar(fontsize = 10), labeling= labeling_border(rot_labels = c(90,0,0,0), 
                                                              just_labels = c("left", 
                                                                              "center", 
                                                                              "center", 
                                                                              "center")))


# Match polfamlist with RCA13 actorg
match(RCA13$actorg, polfamlist$act1)
polfamlist$actpar[match(RCA13$actorg, polfamlist$act1)]
RCA13$parfam = polfamlist$actpar[match(RCA13$actorg, polfamlist$act1)]

# Explore the uncleaned dataset re polfamlist to see any interesting findings.
# Basic barplot
library(ggplot2)
RCAdf %>% filter(!is.na(act2obj) & !is.na(parfam)) %>%
  count(parfam, act2obj) %>%
  ungroup() %>%
  group_by(parfam) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(fill = parfam, x=act2obj, y = rel.freq)) +
  geom_bar(stat = "identity", position = "dodge") + coord_flip()

# Same again but according to frame type
library(ggplot2)
RCAdf %>% filter(!is.na(frame_type) & !is.na(parfam) & frame_type != "instrumental / utilitarian") %>%
  count(parfam, frame_type) %>%
  ungroup() %>%
  group_by(parfam) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(fill = parfam, x=frame_type, y = rel.freq)) +
  geom_bar(stat = "identity", position = "dodge") + coord_flip()



# We can see from these preliminary results that, as expected, radical TAN/conservative parties invoke
#national identity scopes more than their liberal counterparts. Obviously, findings are tentative and we need to
# still clean the parfam column.

# Boxplot basic
library(tidyverse)
library(hrbrthemes)
library(viridis)
RCAdf %>%
  filter(!is.na(act2obj) & !is.na(parfam)) %>%
  count(act2obj, source, parfam) %>%
  ungroup() %>%
  group_by(source, parfam) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=act2obj, y=rel.freq, fill=act2obj)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of actobj scopes by parfam") +
  xlab("") + coord_flip() + facet_wrap(~ source)
  
# Without facet_wrap.....

RCAdf %>%
  filter(!is.na(act2obj) & !is.na(parfam)) %>%
  count(act2obj, parfam) %>%
  ungroup() %>%
  group_by(parfam) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=act2obj, y=rel.freq, fill=act2obj)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of actobj scopes by parfam") +
  xlab("") + coord_flip() 
  
 
# Fairly conclusive evidence herein that politically affiliated actors refer
# to nationally imagined community more frequently than non-affiliated actors.
# Now for distribution boxplot according to act_type.

RCAdf %>%
  filter(!is.na(act2obj)) %>%
  count(act2obj, source_country, act_type) %>%
  ungroup() %>%
  group_by(source_country, act_type) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=act2obj, y=rel.freq, fill=act2obj)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of actobj scopes by act_type") +
  xlab("") + coord_flip() + facet_wrap(~ source_country)

# Without facet wrap....


RCAdf %>%
  filter(!is.na(act2obj)) %>%
  count(act2obj, act_type) %>%
  ungroup() %>%
  group_by(act_type) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=act2obj, y=rel.freq, fill=act2obj)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of actobj scopes by act_type") +
  xlab("") + coord_flip() 



# Then do the same for source_country
RCAdf %>%
  filter(!is.na(act2obj)) %>%
  count(act2obj, source_country) %>%
  ungroup() %>%
  group_by(source_country) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=act2obj, y=rel.freq, fill=act2obj)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of actobj scopes by source_country") +
  xlab("") + coord_flip() 

# Create boxplot to compare distribution of objscope(RECONNECT) claims and
# compare across newsp_type

RCAdf %>%
  filter(!is.na(obj_scope_RECON)) %>%
  count(obj_scope_RECON, source_country, newsp_type) %>%
  ungroup() %>%
  group_by(source_country, newsp_type) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=obj_scope_RECON, y=rel.freq, fill=obj_scope_RECON)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of objscope (RECON) by source_country") +
  xlab("") + coord_flip() + facet_wrap(~ newsp_type)


# Then do the same for source

RCAdf %>%
  filter(!is.na(act2obj)) %>%
  count(act2obj, source, source_country) %>%
  ungroup() %>%
  group_by(source, source_country) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=act2obj, y=rel.freq, fill=act2obj)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of actobj scopes by source") +
  xlab("") + coord_flip() + facet_wrap(~ source_country)

# Now without facet_wrap

RCAdf %>%
  filter(!is.na(act2obj)) %>%
  count(act2obj, source) %>%
  ungroup() %>%
  group_by(source) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=act2obj, y=rel.freq, fill=act2obj)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of actobj scopes by source") +
  xlab("") + coord_flip() 


# Then do the same for newsp_type

RCAdf %>%
  filter(!is.na(act2obj)) %>%
  count(act2obj, newsp_type, source_country) %>%
  ungroup() %>%
  group_by(newsp_type,source_country) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=act2obj, y=rel.freq, fill=act2obj)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of actobj scopes by newsp_type") +
  xlab("") + coord_flip() + facet_wrap(~ source_country)

# Without facet_wrap....


RCAdf %>%
  filter(!is.na(act2obj)) %>%
  count(act2obj, newsp_type) %>%
  ungroup() %>%
  group_by(newsp_type) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=act2obj, y=rel.freq, fill=act2obj)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of actobj scopes by newsp_type") +
  xlab("") + coord_flip() 


# Create boxplot to re objscope(RECONNECT) and source

RCAdf %>%
  filter(!is.na(obj_scope_RECON)) %>%
  count(obj_scope_RECON, source, source_country) %>%
  ungroup() %>%
  group_by(source, source_country) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=obj_scope_RECON, y=rel.freq, fill=obj_scope_RECON)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of actobj scopes by source") +
  xlab("") + coord_flip() + facet_wrap(~ source_country)

# Without facet_wrap and with jitter and source labels


RCAdf %>%
  filter(!is.na(obj_scope_RECON)) %>%
  count(obj_scope_RECON, source) %>%
  ungroup() %>%
  group_by(source) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=obj_scope_RECON, y=rel.freq, fill=obj_scope_RECON, label = source, color=obj_scope_RECON)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of objscopes by source") +
  xlab("") + geom_jitter() +
  geom_text(check_overlap = TRUE)+
  theme(legend.position="none")

# Lets do the same according to newspaper type

RCAdf %>%
  filter(!is.na(obj_scope_RECON)) %>%
  count(obj_scope_RECON, newsp_type, source_country) %>%
  ungroup() %>%
  group_by(newsp_type, source_country) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=obj_scope_RECON, y=rel.freq, fill=newsp_type, label = newsp_type)) +
  geom_bar(stat = 'identity', position ="dodge") +
  ggtitle("rel. freq of objscopes by newspaper type") + facet_wrap(~ source_country)

# Lets do the same according period

RCAdf %>%
  filter(!is.na(obj_scope_RECON)) %>%
  count(obj_scope_RECON, newsp_type, period) %>%
  ungroup() %>%
  group_by(newsp_type, period) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=obj_scope_RECON, y=rel.freq, fill=newsp_type, label = newsp_type)) +
  geom_bar(stat = 'identity', position ="dodge") +
  ggtitle("rel. freq of objscopes by newspaper type") + facet_wrap(~ period)

# Start cleaning dataset ready for parfam column.
# First we need to filter data.

parfamdf <- RCA13 %>% filter(act_region == "EUROPE" & act_type == "former states(wo)men
" | act_type ==  "government/executive" | act_type == "legislative" | act_type == "political parties" | act_type == "politicians")

# Creat parfam column based on data from actorg and actorgmisc.
match(RCA13$actorg, polfamlistmain$act1)
polfamlistmain$actpar[match(RCA13$actorg, polfamlistmain$act1)]
RCA13$parfam = polfamlistmain$actpar[match(RCA13$actorg, polfamlistmain$act1)]

# Then match for actorgmisc
match(RCA13$actorg_misc, polfamlistmain$act1)
polfamlistmain$actpar[match(RCA13$actorg_misc, polfamlistmain$act1)]
RCA13$parfam2 = polfamlistmain$actpar[match(RCA13$actorg_misc, polfamlistmain$act1)]

# Any missing values in parfam are rectified with values from parfarm2.
RCA13$parfam[is.na(RCA13$parfam)] <- RCA13$parfam2[is.na(RCA13$parfam)]



#Examine which actor types are most prevalent pre and during covid. Does crises
# benefit gov/exec as expected?
library(dplyr)
library(ggplot2)
RCAdf %>% count(act_type, period, sort = T) %>% 
  ggplot(., aes(x = act_type, y = n, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") + coord_flip()




# examine which party families benefit from crisis?
RCAdf %>% count(parfam, period, sort = T) %>% filter(!is.na(parfam)) %>%
  ggplot(., aes(x = parfam, y = n, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") + coord_flip()

# Examine period and natobj scope. The results show that public spheres become
# more 'domesticated' during crises in line with previous findings. 

RCAdf %>% count(act2adr, period, sort = T) %>% filter(!is.na(act2adr)) %>%
  ggplot(., aes(x = act2adr, y = n, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") + coord_flip()


# These findings are confirmed with the objscope. 
RCAdf %>% count(act2obj, period, sort = T) %>% filter(!is.na(act2obj)) %>%
  ggplot(., aes(x = act2obj, y = n, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") + coord_flip()

#Scopes according to party family
RCAdf %>% count(act2adr, parfam, sort = T) %>% filter(!is.na(act2adr)) %>% filter(!is.na(parfam)) %>%
  ggplot(., aes(x = act2adr, y = n, fill = parfam)) +
  geom_bar(stat = "identity", position = "dodge") + coord_flip()

#obj Scopes according to party family
RCAdf %>% count(act2obj, parfam, sort = T) %>% filter(!is.na(act2obj)) %>% filter(!is.na(parfam)) %>%
  ggplot(., aes(x = act2obj, y = n, fill = parfam)) +
  geom_bar(stat = "identity", position = "dodge") + coord_flip()

# relative obj scopes no. according to party family 
RCAdf %>% filter(!is.na(act2obj)) %>% filter(!is.na(parfam)) %>%
  ggplot(., aes(x = act2obj,  fill = parfam)) +
  geom_bar(position = "fill") + coord_flip()

# relative frames according to party family 
RCAdf %>% filter(!is.na(frame_type)) %>% filter(!is.na(parfam)) %>%
  ggplot(., aes(x = frame_type,  fill = parfam)) +
  geom_bar(position = "fill") + coord_flip()



# Create boxplot to compare no. frame type distribution by party family
library(tidyverse)
library(hrbrthemes)
library(viridis)
RCAdf %>%
  filter(!is.na(frame_type) & !is.na(parfam)) %>%
  count(frame_type, parfam) %>%
  ungroup() %>%
  group_by(parfam) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(aes(x=frame_type, y=rel.freq, fill=frame_type)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("party family frame count distribution") +
  xlab("") + coord_flip()

# With facet_wrap re source_country

RCAdf %>%
  filter(!is.na(frame_type) & !is.na(parfam)) %>%
  count(frame_type, parfam, source_country) %>%
  ungroup() %>%
  group_by(parfam, source_country) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(aes(x=frame_type, y=rel.freq, fill=frame_type)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("party family frame count distribution") +
  xlab("") + coord_flip() + facet_wrap(~ source_country)

# Now with facet_wrap as newsp_type

RCAdf %>%
  filter(!is.na(frame_type) & !is.na(parfam)) %>%
  count(frame_type, parfam, newsp_type) %>%
  ungroup() %>%
  group_by(parfam, newsp_type) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(aes(x=frame_type, y=rel.freq, fill=frame_type)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("party family frame count distribution") +
  xlab("") + coord_flip() + facet_wrap(~ newsp_type)


# With compare pre and during covid crisis.....

RCAdf %>%
  filter(!is.na(frame_type) & !is.na(parfam)) %>%
  count(frame_type, parfam, period) %>%
  ungroup() %>%
  group_by(parfam, period) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(aes(x=frame_type, y=rel.freq, fill=frame_type)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("party family frame count distribution") +
  xlab("") + coord_flip() + facet_wrap(~ period)


# Boxplot to illustrate distribution of frames across different public spheres. 

RCAdf %>%
  filter(!is.na(frame_type)) %>%
  count(frame_type, source_country) %>%
  ungroup() %>%
  group_by(source_country) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(aes(x=frame_type, y=rel.freq, fill=frame_type)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("source_country rel. freq frame count") +
  xlab("") + coord_flip()

# Are the same issues reported across country?

RCAdf %>%
  filter(!is.na(issfield)) %>%
  count(issfield, source_country) %>%
  ungroup() %>%
  group_by(source_country) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(aes(x=issfield, y=rel.freq, fill=issfield)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("source_country rel. freq issfield count") +
  xlab("") + coord_flip()

# The above boxplot shows that different public spheres report different issues..
# Tutorial on boxplots in R
library(ggplot2)
# Basic box plot to show distribution of act2adr discursive scopes according to
# party family.

bp <- RCAdf %>% 
  filter(!is.na(act2adr) & !is.na(parfam)) %>%
  count(act2adr, parfam) %>%
  ggplot(., aes(x=act2adr, y=n, fill = act2adr))+
  geom_boxplot() +
  ggtitle("abs. no of claims according to discrusive scope (act2adr) and party family")
# Change outlier, color, shape and size
bp + geom_boxplot(notch = F, outlier.colour="red", outlier.shape=8,
             outlier.size=4) + coord_flip() 
  # Box plot with mean points
bp + stat_summary(fun=mean, geom="point", shape=23, size=4) 

#Choose which items to display :
bp + scale_x_discrete(limits=c("national", "horizontal Europeanisation"))

# Box plot with dot plot
bp + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

# Box plot with jittered points
# 0.2 : degree of jitter in x direction
bp + geom_jitter(shape=16, position=position_jitter(0.2))

# Change box plot colors by groups i.e. 3 categorical variables - namely,
# act2adr, parfam, and source_country.

bp2 <- RCAdf %>% 
  filter(!is.na(act2adr) & !is.na(parfam)) %>%
  count(act2adr, parfam, source_country) %>%
  ggplot(., aes(x=act2adr, y=n, fill = source_country, label = parfam))+
  geom_boxplot() +
  ggtitle("abs. no of claims according to discursive scope (act2adr), party family and across country")

# Change the position
bp2 + geom_boxplot(position=position_dodge(1))

# Add dots
bp2 + geom_dotplot(binaxis='y', stackdir='center', binwidth = 1, position=position_dodge(1))

# add labels text to dotplots.
bp2 + # jittered text with geom_text
  geom_text(check_overlap = TRUE,
            position=position_jitter(width=0.15))+
  theme(legend.position="none")

# Change color of dots according to parfam variable.
bp2 + geom_jitter(shape=16, position=position_jitter(0.2), alpha=1, aes(colour=parfam))

# Keep to 2 variables for jitter plot as there is too much info herein. 

bp3 <- RCAdf %>% 
filter(!is.na(act2adr) & !is.na(parfam)) %>%
  count(act2adr, parfam, source_country) %>%
  ggplot(., aes(x=act2adr, y=n, fill = act2adr))+
  geom_boxplot() + 
  ggtitle("abs. no of claims according to discursive scope (act2adr), party family")

#add jitter color coded according to parfam
bp3 + geom_jitter(shape=16, size = 4, position=position_jitter(0.2), alpha=1, aes(colour=parfam))
# add facet wrap to compare across countries
bp3 + facet_wrap(~source_country) + 
  geom_jitter(shape=16, size = 4, position=position_jitter(0.2), alpha=1, aes(colour=parfam))

# See http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
# for more help on how to make a boxplot.

bp4 <- RCAdf %>% 
  filter(!is.na(act2obj) & !is.na(act_type)) %>%
  count(act2obj, act_type) %>%
  ungroup() %>%
  group_by(act_type) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=act2obj, y=rel.freq, fill = act2obj, label = act_type))+
  geom_boxplot() + 
  ggtitle("rel. no of identity claims (act2obj) by act_type") 

#add jitter color coded according to actype
bp4 + geom_jitter(shape=16, size = 2, position=position_jitter(0.2), alpha=1, aes(colour=act_type)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# add facet wrap to compare across countries
bp4 + facet_wrap(~source_country) + 
  geom_jitter(shape=16, size = 2, position=position_jitter(0.2), alpha=1, aes(colour=act_type)) 

#Choose which items to display and add labels
bp4 + scale_x_discrete(limits=c("national"))+
  geom_text(aes(label = act_type))


#######################################################################
# Create a two mode network sub graph for nodes containing parfam attribute to
# see how different party families discursively construct europe.

library(readxl)
library(dplyr)
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA15.xlsx")
RCAdf2 <- RCAdf %>% filter(!is.na(objnat) & !is.na(parfam))
RCAdf2$objnat <- paste0(RCAdf2$objnat, " constituency")
RCAdf3 <- RCAdf2[!grepl("UNSPECIFIED constituency", RCAdf2$objnat),]
RCAdf3$objnat <- tolower((RCAdf3$objnat))
actnodelist <- RCAdf3 %>% select(actorg, parfam)
actnodelist2 <- unique(actnodelist)
colnames(actnodelist2) <-c('id','parfam')
objnodelist <- RCAdf3 %>% select(objnat, objtype)
objnodelist2 <- unique(objnodelist)
colnames(objnodelist2) <-c('id','parfam')
library(plyr)
nodelist <- rbind.fill(actnodelist2, objnodelist2)
nodelist2 <- nodelist %>%  mutate(parfam = ifelse(grepl("constituency", id), "N/A", parfam))
nodelist3 <- unique(nodelist2)
nodelist4 <- nodelist3 %>%  mutate(Mode = ifelse(grepl("constituency", id), "2", "1"))
nodelist5 <- nodelist4 %>% relocate(Mode, .after = id)
colnames(nodelist5) <-c('Label','mode', 'parfam')
library(xlsx)
write.xlsx(nodelist5, 'bipartite_parfam.xlsx', row.names = FALSE)

#To create edgelist ready for Gephi 
edgelist <- RCAdf3 %>% select(actorg, objnat, act2obj, EUval)
edgelist$Type <- 'Undirected'
edgelist2 <- edgelist %>% relocate(Type, .after = objnat)
colnames(edgelist2) <-c('Source','Target','Type','act2obj', 'EUeval')
write.xlsx(as.data.frame(edgelist2), 'edgelist_bipartite_parfam.xlsx', row.names = FALSE)

# Matching edge attributes with colours
edgescopecol <- edgescolor <- c('#FF0000','#FF00FF','#008000','#0000FF','#FFA500','#FFFF00')
edgescopedf <- c('national','regional / global (other)', 'bottom-up vertical Europeanisation','horizontal Europeanisation','top-down vertical Europeanisation', 'supranational Europeanisation')
df1 <- as.data.frame(edgescopecol)
df2 <- as.data.frame(edgescopedf)
library(dplyr)
df3 <- df2 %>% mutate(colour = edgescopecol)
library(readxl)
edgelist_bipartite_parfam <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/Euronet/edgelist_bipartite_parfam.xlsx")
View(edgelist_bipartite_parfam)
match(edgelist_bipartite_parfam$`act2obj`, df3$edgescopedf)
df3$colour[match(edgelist_bipartite_parfam$`act2obj`, df3$edgescopedf)]
edgelist_bipartite_parfam$colour = df3$colour[match(edgelist_bipartite_parfam$`act2obj`, df3$edgescopedf)]
library(xlsx)
write.xlsx(edgelist_bipartite_parfam, 'edgelist_bipartite_parfam2.xlsx')



#######################################################################

# Carry out QTA of newspaper text.
#Quanteda analysis
library(quanteda)
library(quanteda.corpora)
library(tm)
data <- data.frame(text=RCAdf$text, stringsAsFactors=FALSE)
#assign unique id
data$doc_id <- paste(RCAdf$`statement ID`, RCAdf$source, RCAdf$time, RCAdf$source_country, RCAdf$period, sep =  "_")

#Convert to corpus
corp2 <- corpus(data)

#Clean corpus and create dfm
dfm <- dfm(corp2, remove = stopwords("english"),
           remove_punct = TRUE, remove_numbers = TRUE)
dfm <- dfm_tolower(dfm, keep_acronyms = TRUE)

#One thing we can do with this dfm is to generate a frequency graph using the
#topfeatures function. For this, we first have to save the 50 most frequently
#occurring words in our texts:

features <- topfeatures(dfm, 50)


#We then have to transform this object into a data frame, and sort it by decreasing
#frequency:

features_plot <- data.frame(list(term = names(features),frequency = unname(features)))
features_plot$term <- with(features_plot, reorder(term, -frequency))

#Then we can plot the results:

library(ggplot2)
ggplot(features_plot) +
  geom_point(aes(x=term, y=frequency)) +
  theme_classic()+
  theme(axis.text.x=element_text(angle=90, hjust=1))

#Create wordcloud
wordcloud_dfm_trim <- dfm_trim(dfm, min_termfreq = 100)
textplot_wordcloud(wordcloud_dfm_trim)

#Dictionary analysis
# import the Laver-Garry dictionary from Provalis Research
dictfile <- tempfile()
download.file("https://provalisresearch.com/Download/LaverGarry.zip",
              dictfile, mode = "wb")
unzip(dictfile, exdir = (td <- tempdir()))
dictlg <- dictionary(file = paste(td, "LaverGarry.cat", sep = "/"))
head(dfm(data_corpus_inaugural, dictionary = dictlg))
dictionary_results <- dfm_lookup(dfm, dictlg)

#Conducting sentiment analysis
sent_dfm <-dfm_lookup(dfm, data_dictionary_LSD2015)
sentiment <- convert(sent_dfm, to="data.frame")
sentiment_difference <- sentiment$positive - sentiment$negative
sentiment_difference <- as.data.frame(sentiment_difference)
View(sentiment_difference)
sentiment2 <- cbind(sentiment, sentiment_difference)
sentiment_ratio <- (sentiment2$positive/(sentiment2$positive +
                                           sentiment2$negative))
sentiment3 <- cbind(sentiment2, sentiment_ratio)

# Mutate to add sentiment analysis column in dataframe.
RCA16 <- RCA15 %>% mutate(sentiment_diff = sentiment3$sentiment_difference)
RCA17 <- RCA16 %>% mutate(sentiment_ratio = sentiment3$sentiment_ratio)

#Topic modelling
library(topicmodels)
library(quanteda)
library(quanteda.corpora)
dtm <- convert(dfm, to = "topicmodels")
burnin <- 2000
iter <- 1000
thin <- 200
seed <- list(42, 5, 24, 158, 2500)
nstart <- 5
best <- TRUE
lda17 <- LDA(dtm, k = 17, method = "Gibbs",
             control = list(burnin = burnin, iter = iter, thin = thin,
                            seed = seed, nstart = nstart, best = best))

terms(lda17, 17)
library(tidytext)
library(dplyr)
library(ggplot2)

lda10_topics <- tidy(lda17, matrix = "beta")
lda10_topterms <- lda10_topics %>% group_by(topic) %>%
  top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)

lda10_topterms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") + coord_flip()





##############################################



# calculate mean sentiment_diff according to paper.
# basic ggplots to examine sentiment data (see this link for more: https://stackoverflow.com/questions/11562656/calculate-the-mean-by-group)
senti_source <- aggregate(sentiment_diff ~ source, RCAdf, mean )
#Basic barplot to visualise sentiment according to source
p <- ggplot(senti_source, aes(x=source, y=sentiment_diff, fill = source)) + 
  geom_bar(stat = "identity") + coord_flip()

# calculate mean sentiment_diff according to country
senti_country <- aggregate(sentiment_diff ~ source_country, RCAdf, mean )
p2 <- ggplot(senti_country, aes(x=source_country, y=sentiment_diff, fill = source_country)) + 
  geom_bar(stat = "identity") + coord_flip()

#Compare EUvalencemean according to parfam
eval_pfam <- aggregate(EUval ~ parfam, RCAdf, mean )
p3 <- ggplot(eval_pfam, aes(x=parfam, y=EUval, fill = parfam)) + 
  geom_bar(stat = "identity")


#Compare EUvalencemean according to actype
eval_act <- aggregate(EUval ~ act_type, RCAdf, mean )
p4 <- ggplot(eval_act, aes(x=act_type, y=EUval)) + 
  geom_point(stat = "identity") + coord_flip()
#Rearranged according to EUeval.
eval_act2 = eval_act[order(eval_act$EUval),]
dotchart(eval_act2$EUval, labels = eval_act2$act_type, cex = 0.6, xlab = "mean EU evaluation by actor type")

#Scatterplot to examine relation between sentiment and EUval by source
sent_source <- aggregate(sentiment_diff ~ source, RCAdf, mean)
EUval_source <- aggregate(EUval ~ source, RCAdf, mean)
df <- cbind(EUval_source, sent_source)
colnames(df) <- make.unique(names(df))
p <- ggplot(df, aes(x=EUval, y=sentiment_diff)) + geom_point() + 
ggtitle("scatterplot to examine correlation between sentiment and EU evaluation by source") 
  

#Scatterplot to examine relation between sentiment and EUval by country (https://www.r-graph-gallery.com/272-basic-scatterplot-with-ggplot2.html)
sent_source <- aggregate(sentiment_diff ~ source_country, RCAdf, mean)
EUval_source <- aggregate(EUval ~ source_country, RCAdf, mean)
df <- cbind(EUval_source, sent_source)
colnames(df) <- make.unique(names(df))
p2 <- ggplot(df, aes(x=EUval, y=sentiment_diff)) + geom_point() + 
  ggtitle("scatterplot to examine correlation between sentiment and EU evaluation by country") 

# Compare relative frequency re national obj scopes with EUvalmean by acttype
# using scatterplot

df <- RCAdf %>%
  count(act2obj, act_type) %>%
  ungroup() %>%
  group_by(act_type) %>%
  mutate(rel.freq = n / sum(n)) %>% 
  filter(act2obj == "national" & !is.na(act2obj) & !is.na(act_type))
df2 <- aggregate(EUval ~ act_type, RCAdf, mean)
df3 <- df2 %>% semi_join(df)
df4 <- cbind(df, df3)
p3 <- ggplot(df4, aes(x=EUval, y=rel.freq)) + geom_point()
library("ggpubr")
ggscatter(df4, x = "EUval", y = "rel.freq", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "EUval", ylab = "rel.freq of national act2obj scopes by actor type")


#Pearson correlation test
res <- cor.test(df4$rel.freq,  df4$EUval,
                method = "pearson")
# Extract the p.value
res$p.value
# Extract the correlation coefficient
res$estimate
# Shapiro-Wilk normality test for mpg
shapiro.test(df4$rel.freq) 
# Shapiro-Wilk normality test for wt
shapiro.test(df4$EUval) 
#Visual inspection of the data normality using Q-Q plots (quantile-quantile plots). Q-Q plot draws the correlation between a given sample and the normal distribution.
library("ggpubr")
# mpg
ggqqplot(df4$rel.freq, ylab = "rel.freq")
# wt
ggqqplot(df4$EUval, ylab = "EUval")

#Kendall rank correlation test
#The Kendall rank correlation coefficient or Kendall's tau statistic is used to estimate a rank-based measure of association. This test may be used if the data do not necessarily come from a bivariate normal distribution.
res2 <- cor.test(df4$rel.freq, df4$EUval,  method="kendall")
res2

#Spearman rank correlation coefficient
#Spearman's rho statistic is also used to estimate a rank-based measure of association. This test may be used if the data do not come from a bivariate normal distribution.

res2 <- cor.test(df4$rel.freq, df4$EUval,  method="spearman")
res2





# Compare relative frequency re act2adr scopes with EUvalmean by acttype
# using scatterplot

df <- RCAdf %>%
  count(act2adr, act_type) %>%
  ungroup() %>%
  group_by(act_type) %>%
  mutate(rel.freq = n / sum(n)) %>% 
  filter(act2adr == "national" & !is.na(act2adr) & !is.na(act_type)) 
df2 <- aggregate(EUval ~ act_type, RCAdf, mean)
df3 <- df2 %>% semi_join(df)
df4<- cbind(df, df3)
p3 <- ggplot(df4, aes(x=EUval, y=rel.freq)) + geom_point()
library("ggpubr")
ggscatter(df4, x = "EUval", y = "rel.freq", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "EUval", ylab = "rel.freq of national act2adr scopes by actor type")
  

# Examine correlation between sentiment and no. of national object scopes. 

df <- RCAdf %>%
  count(act2obj, act_type) %>%
  ungroup() %>%
  group_by(act_type) %>%
  mutate(rel.freq = n / sum(n)) %>% 
  filter(act2obj == "national" & !is.na(act2obj) & !is.na(act_type)) 
df2 <- aggregate(sentiment_diff ~ act_type, RCAdf, mean)
df3 <- df2 %>% semi_join(df)
df4<- cbind(df, df3)
p3 <- ggplot(df4, aes(x=sentiment_diff, y=rel.freq)) + geom_point()
library("ggpubr")
ggscatter(df4, x = "sentiment_diff", y = "rel.freq", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "sentiment_diff", ylab = "rel.freq of national act2adr scopes / sentiment by actor type")



# Explore correlation between adreval and no. of national obj scopes. 

df <- RCAdf %>%
  count(act2obj, act_type) %>%
  ungroup() %>%
  group_by(act_type) %>%
  mutate(rel.freq = n / sum(n)) %>% 
  filter(act2obj == "national" & !is.na(act2obj) & !is.na(act_type)) 
df2 <- aggregate(adreval ~ act_type, RCAdf, mean)
df3 <- df2 %>% semi_join(df)
df4<- cbind(df, df3)
p3 <- ggplot(df4, aes(x=adreval, y=rel.freq)) + geom_point()  
ggscatter(df4, x = "adreval", y = "rel.freq", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "adreval", ylab = "rel.freq of national act2obj scopes / adreval by actor type")




################### To continue 10/06 ##############


# Explore correlation between adreval and no. of national act2adr scopes.

df <- RCAdf %>%
  count(act2adr, act_type) %>%
  ungroup() %>%
  group_by(act_type) %>%
  mutate(rel.freq = n / sum(n)) %>% 
  filter(act2adr == "national" & !is.na(act2adr) & !is.na(act_type)) 
df2 <- aggregate(adreval ~ act_type, df, mean)
df3 <- df2 %>% semi_join(df)
df4<- cbind(df, df3)
p3 <- ggplot(df4, aes(x=adreval, y=rel.freq)) + geom_point()
# To test correlation using pearsons
library("ggpubr")
ggscatter(df4, x = "adreval", y = "rel.freq", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "adreval", ylab = "rel.freq of national scopes / act2adr by actor type")

#Pearson correlation test
res <- cor.test(df4$rel.freq,  df4$adreval,
                method = "pearson")
res

# see http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r#:~:text=Pearson%20correlation%20(r)%2C%20which,named%20the%20linear%20regression%20curve 
# for more info on scatterplots and pearsons coefficients. 
# Explore correlation between adreval and no. of national obj scopes to explore
# the national identity + negative framing thesis. 

df <- RCAdf %>%
  count(act2obj, act_type) %>%
  ungroup() %>%
  group_by(act_type) %>%
  mutate(rel.freq = n / sum(n)) %>% 
  filter(act2obj == "national" & !is.na(act2obj) & !is.na(act_type)) 
df2 <- aggregate(adreval ~ act_type, df, mean)
df3 <- df2 %>% semi_join(df)
df4<- cbind(df, df3)
p3 <- ggplot(df4, aes(x=adreval, y=rel.freq)) + geom_point()
library("ggpubr")
ggscatter(df4, x = "adreval", y = "rel.freq", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "adreval", ylab = "rel.freq of national scopes")
#Pearson correlation test
res <- cor.test(df4$rel.freq,  df4$adreval,
                method = "pearson")

# See http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r#:~:text=Pearson%20correlation%20(r)%2C%20which,named%20the%20linear%20regression%20curve. 
# for more. 

# Examine if correlation between EUval and rel.freq of national scopes (by parfam)

df <- RCAdf %>%
  count(act2obj, parfam) %>%
  ungroup() %>%
  group_by(parfam) %>%
  mutate(rel.freq = n / sum(n)) %>% 
  filter(act2obj == "national" & !is.na(act2obj) & !is.na(parfam)) 
df2 <- aggregate(EUval ~ parfam, RCAdf, mean)
df3 <- df2 %>% semi_join(df)
df4<- cbind(df, df3)
p3 <- ggplot(df4, aes(x=EUval, y=rel.freq)) + geom_point()
ggscatter(df4, x = "EUval", y = "rel.freq", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "EUval", ylab = "rel.freq of national scopes / EUval by parfam")
#Pearson correlation test
res <- cor.test(df4$rel.freq,  df4$adreval,
                method = "pearson")

# Examine if correlation between EUval and rel.freq of national scopes (by actorg)
# for top 20(n) orgs only. 

df <- RCAdf %>%
  count(act2obj, actorg) %>%
  ungroup() %>%
  group_by(actorg) %>%
  mutate(rel.freq = n / sum(n)) %>% 
  filter(act2obj == "national" & !is.na(act2obj) & n >= 5) 
df2 <- aggregate(EUval ~ actorg, RCAdf, mean)
df3 <- df2 %>% semi_join(df)
df4<- cbind(df, df3)
p3 <- ggplot(df4, aes(x=EUval, y=rel.freq)) + geom_point() + 
  #Add labels to the ggplot
geom_text(
  label=df4$actorg...2, 
  check_overlap = T)

# Examine if correlation between EUval and rel.freq of national act2adr scopes (by actorg)
# for top 20(n) orgs only. 

df <- RCAdf %>%
  count(act2adr, actorg) %>%
  ungroup() %>%
  group_by(actorg) %>%
  mutate(rel.freq = n / sum(n)) %>% 
  filter(act2adr == "national" & !is.na(act2adr) & n >= 5) 
df2 <- aggregate(EUval ~ actorg, RCAdf, mean)
df3 <- df2 %>% semi_join(df)
df4<- cbind(df, df3)
p3 <- ggplot(df4, aes(x=EUval, y=rel.freq)) + geom_point() + 
  #Add labels to the ggplot
  geom_text(
    label=df4$actorg...2, 
    check_overlap = T)

#Measure exclusive identity (conflict framing) compared to inc. identity,
# using adreval and scope variables. We can see clearly from results that
# identity-conflict framing thesis is confirmed. National scopes and negative
# evaluations are more prevalent. Values are relative frequency. 

library(dplyr)
library(ggplot2)
library(readxl)
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA17.xlsx")
RCAdf %>% 
  filter(!is.na(act2obj) & adreval !=0) %>%
  ggplot(., aes(x = adreval, fill = act2obj)) +
geom_bar(position = "fill")

# Position is "dodge" and absolute values
RCAdf %>% 
  filter(!is.na(act2obj) & adreval !=0) %>%
  count(act2obj, adreval) %>%
  ggplot(., aes(x = as.factor(adreval), y = n, fill = act2obj)) +
  geom_bar(stat = "identity", position = "dodge") + coord_flip() +
  xlab("adreval") + ylab("no. of national object scopes")

# Another visualisation filtering for national scope only. 
xlabels <- c("negative valence", "positive valence")
RCAdf %>% 
  filter(!is.na(act2obj) & act2obj == 'national' & adreval !=0) %>%
  count(act2obj, adreval) %>%
  ggplot(., aes(x = as.factor(adreval), y = n, fill = as.factor(adreval))) +
  geom_bar(stat = "identity") +
# How to change x and y axis labels (http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles for more info)
  xlab("national identity") + ylab("n") +
  labs(fill = "adreval") + 
  scale_x_discrete(labels= xlabels)

#Add act_scope (a la RECONNECT) column to dataset
RCA2 <- RCA %>% mutate(act_scope_RECON = ifelse(act_region == 'EUROPE' & act_nat == 'EUROPEAN UNION', 'EU Supranational', ifelse(act_region == 'EUROPE' & act_nat != 'EUROPEAN UNION' & source_country == act_nat, 'Own Country, National',ifelse(act_region == 'EUROPE' & act_nat != 'EUROPEAN UNION', 'Other EU Member State', 'Global / Regional'))))
RCA2 <- RCA2 %>% relocate(act_scope_RECON, .after = act_scope)

#Use summarise to compare sentimence by newspaper. Interestingly, quality newspapers
# were found to have a more negative tone than tabloid newspapers

df <- summarise(group_by(RCAdf, newsp_type),
                mean_sent = mean(sentiment_diff))

#Unsurprisingly, the crisis negatively impacted on sentimance. 

df2 <- summarise(group_by(RCAdf, newsp_type, period),
                mean_sent = mean(sentiment_diff))

# Country-level differences in sentiment
df3 <- summarise(group_by(RCAdf, source_country),
                 mean_sent = mean(sentiment_diff))

# Sentiment according to newspaper
df4 <- summarise(group_by(RCAdf, source),
                 mean_sent = mean(sentiment_diff))


################# VISIBILITY OF ACTORS###############################
############ CLAIMANTS################################################
#Measure and plot the visibility distribution of actors (claimants only)
df4 <- RCAdf %>% 
  group_by(actname) %>%
  summarise(n = n())
dfmean <- summarise(df4, mean_n = mean(n))
# Average number of claims per actor is 1.8
df4 <- RCAdf %>% 
  group_by(actorg) %>%
  summarise(n = n())
dfmean <- summarise(df4, mean_n = mean(n))
#When claimants are aggregated, the number increases to 2.7 per actor.
# Lets compare average no. claims by claimant region

df <- RCAdf %>% 
  count(act_scope) 

# The dataframe above shows that national-level actors dominate the show as claimants. In excess
# of 2800 claims vis-a-vis 600 for EU-level actors.

df <- RCAdf %>% 
  count(act_scope_RECON) 

# And of those circa 2800 national claims, 1680 are domestic (same country as where
# newspaper is published vis-a-vis 1100). In other words, 65% are 'own country national'.
# We can thus tentatively conclude that EU actors are relatively visible but national actors
# are overwhelmingly so.

df <- RCAdf %>% 
  count(parfam, sort = T)

# This dataframe above show the distibution of claims by party family. 

df <- RCAdf %>% 
  count(act_type, sort = T)

# Unsurprisingly, gov/exec actors dominate in terms of agency. 
# Lets include the variable of crisis to see changes in representation.
library(ggplot2)
df <- RCAdf %>% 
  count(act_type, period, sort = T) 
p <- ggplot(data = df, aes(x = period, y = n, fill = act_type)) +
  geom_bar(stat = "identity", position ="dodge") + coord_flip()


#Measure and plot the visibility distribution of actors (claimants only)
df5 <- RCAdf %>% 
  group_by(actname) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq)) %>%
  top_n(48)

# Dot chart of a single numeric vector
dotchart(df5$n, labels = df5$actname,
         cex = 0.9, xlab = "No. of claims (top 50)")

# Plot and color by groups cyl
df5$actscope <- RCAdf$act_scope[match(df5$actname, RCAdf$actname)]
df5$actscope[which(df5$actname == "Von der Leyen Ursula")] = "EUROPEAN UNION"
df5$actscope[which(df5$actname == "Gentiloni Paolo")] = "EUROPEAN UNION"
length(unique(df5$actscope))
library(RColorBrewer)
grps <- as.factor(df5$actscope)
my_cols <- c("Blue","Red", "Black")
dotchart(df5$n, labels = df5$actname,
         gcolor = my_cols,
         color = my_cols[grps],
         cex = 0.9, pch = 19, xlab = "No. of claims (top 50)")

# Do the same for the actorg variable.
#Measure and plot the visibility distribution of act organisations (claimants only)
df5 <- RCAdf %>% 
  group_by(actorg) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq)) %>%
  top_n(50)

# Dot chart of a single numeric vector
dotchart(df5$n, labels = df5$actorg,
         cex = 0.9, xlab = "No. of claims (top 50)")

# Plot and color by groups cyl
df5$actscope <- RCAdf$act_scope[match(df5$actorg, RCAdf$actorg)]
length(unique(df5$actscope))
library(RColorBrewer)
grps <- as.factor(df5$actscope)
my_cols <- c("Blue","Red", "Black")
dotchart(df5$n, labels = df5$actorg,
         gcolor = my_cols,
         color = my_cols[grps],
         cex = 0.9, pch = 19, xlab = "No. of claims (top 50)")


# Lets do a count of actorgs by country and facet wrap (for more on qplot, see this link: http://www.sthda.com/english/wiki/qplot-quick-plot-with-ggplot2-r-software-and-data-visualization)
df <- RCAdf %>% 
  count(actorg, source_country) %>%
  ungroup() %>%
  group_by(source_country) %>%
  top_n(10) 
# Assign colors by actscope 
df$actscope <- RCAdf$act_scope[match(df$actorg, RCAdf$actorg)]
length(unique(df$actscope))
grps <- as.factor(df$actscope)
my_cols <- c("Blue","Black")
q <- qplot(x = n, y = reorder(actorg, n), data = df, 
           color = factor(grps), geom = "point") +   
  facet_grid(source_country ~., scales = "free_y")

# OR, assign colors by actscope (as per RECONNECT)
df$actscope <- RCAdf$act_scope_RECON[match(df$actorg, RCAdf$actorg)]
length(unique(df$actscope))
grps <- as.factor(df$actscope)
q <- qplot(x = n, y = reorder(actorg, n), data = df, 
           color = factor(grps), geom = "point") +   
  facet_grid(source_country ~., scales = "free_y")

# Now lets see which actors were most visible as claimants pre and during covid, color
# coded by actor type.
df <- RCAdf %>% 
  count(actname, period) %>%
  ungroup() %>%
  group_by(period) %>%
  top_n(20)
#Assign colors by acttype
df$acttype <- RCAdf$act_type[match(df$actname, RCAdf$actname)]
length(unique(df$acttype))
grps <- as.factor(df$acttype)
q <- qplot(x = n, y = reorder(actname, n), data = df, 
           color = factor(grps), geom = "point") +   
  facet_grid(period ~., scales = "free_y")

# The below plot clearly illustrates that gov/exec 'win' and politicians/parties 'lose'
# in terms of agency as claimants (political entreprenuers)

df <- RCAdf %>% 
  count(act_type, period) %>%
  ungroup() %>%
  group_by(period) %>%
  top_n(10)
q <- qplot(x = n, y = reorder(act_type, n), data = df, geom = "point") +   
  facet_grid(period ~., scales = "free_y")


#Compare actscope visbility by source. The results below clearly show that tabloids
# are more parochial in terms of claimantactor visibility. More nationalised / domesticated
# in this respect.

df <- RCAdf %>% 
  count(act_scope_RECON, 
        newsp_type, sort = T) %>%
  ungroup() %>%
  group_by(newsp_type) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(act_scope_RECON, n), data = df, 
           color = factor(act_scope_RECON), geom = "point") +   
  facet_grid(newsp_type  ~., scales = "free_y")

# Lets now compare across different newspapers....

df <- RCAdf %>% 
  count(act_scope_RECON, 
        source, sort = T) %>%
  ungroup() %>%
  group_by(source) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(act_scope_RECON, n), data = df, 
           color = factor(act_scope_RECON), geom = "point") +   
  facet_grid(source  ~., scales = "free_y")

# Lets compare across countries....

df <- RCAdf %>% 
  count(act_scope_RECON, 
        source_country, sort = T) %>%
  ungroup() %>%
  group_by(source_country) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(act_scope_RECON, n), data = df, 
           color = factor(act_scope_RECON), geom = "point") +   
  facet_grid(source_country  ~., scales = "free_y")

# And compare pre and during covid

df <- RCAdf %>% 
  count(act_scope_RECON, 
        period, sort = T) %>%
  ungroup() %>%
  group_by(period) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(act_scope_RECON, n), data = df, 
           color = factor(act_scope_RECON), geom = "point") +   
  facet_grid(period  ~., scales = "free_y")




############# NOW LETS EXAMINE VISIBILITY RE THE ADDRESSEE VARIABLE################
######################## ADDRESSEES###############################################


df4 <- RCAdf %>% 
  group_by(adrname) %>%
  summarise(n = n())
dfmean <- summarise(df4, mean_n = mean(n))
df4 <- RCAdf %>% 
  group_by(adrorg) %>%
  summarise(n = n())
dfmean <- summarise(df4, mean_n = mean(n))

# Lets compare average no. claims by addressee region

df <- RCAdf %>% 
  count(adr_scope) 


df <- RCAdf %>% 
  count(adr_scope_RECON) 


df <- RCAdf %>% 
  count(parfam, sort = T)
 

df <- RCAdf %>% 
  count(adr_type, sort = T)

library(ggplot2)
df <- RCAdf %>% 
  count(adr_type, period, sort = T) 
p <- ggplot(data = df, aes(x = period, y = n, fill = adr_type)) +
  geom_bar(stat = "identity", position ="dodge") + coord_flip()


#Measure and plot the visibility distribution of addressees
df5 <- RCAdf %>% 
  group_by(adrname) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq)) %>%
  top_n(48)

# Dot chart of a single numeric vector
dotchart(df5$n, labels = df5$adrname,
         cex = 0.9, xlab = "No. of claims (top 50)")

# Plot and color by groups cyl
df5$adrscope <- RCAdf$adr_scope[match(df5$adrname, RCAdf$adrname)]
df5$adrscope[which(df5$adrname == "Von der Leyen Ursula")] = "EUROPEAN UNION"
df5$adrscope[which(df5$adrname == "Gentiloni Paolo")] = "EUROPEAN UNION"
length(unique(df5$adrscope))
library(RColorBrewer)
grps <- as.factor(df5$adrscope)
my_cols <- c("Blue","Red", "Black")
dotchart(df5$n, labels = df5$adrname,
         gcolor = my_cols,
         color = my_cols[grps],
         cex = 0.9, pch = 19, xlab = "No. of claims (top 50)")

# Do the same for the adrorg variable.
#Measure and plot the visibility distribution of adr organisations (claimants only)
df5 <- RCAdf %>% 
  group_by(adrorg) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq)) %>%
  top_n(50)

# Dot chart of a single numeric vector
dotchart(df5$n, labels = df5$adrorg,
         cex = 0.9, xlab = "No. of claims (top 50)")

# Plot and color by groups cyl
df5$adrscope <- RCAdf$adr_scope[match(df5$adrorg, RCAdf$adrorg)]
length(unique(df5$adrscope))
library(RColorBrewer)
grps <- as.factor(df5$adrscope)
my_cols <- c("Blue","Red", "Black")
dotchart(df5$n, labels = df5$adrorg,
         gcolor = my_cols,
         color = my_cols[grps],
         cex = 0.9, pch = 19, xlab = "No. of claims (top 50)")


# Lets do a count of adrorgs by country and facet wrap (for more on qplot, see this link: http://www.sthda.com/english/wiki/qplot-quick-plot-with-ggplot2-r-software-and-data-visualization)
df <- RCAdf %>% 
  filter(!is.na(adrorg)) %>%
  count(adrorg, source_country) %>%
  ungroup() %>%
  group_by(source_country) %>%
  top_n(10) 
# Assign colors by adrscope 
df$adrscope <- RCAdf$adr_scope[match(df$adrorg, RCAdf$adrorg)]
length(unique(df$adrscope))
grps <- as.factor(df$adrscope)
my_cols <- c("Blue","Black")
q <- qplot(x = n, y = reorder(adrorg, n), data = df, 
           color = factor(grps), geom = "point") +   
  facet_grid(source_country ~., scales = "free_y")

# OR, assign colors by adrscope (as per RECONNECT)
df$adrscope <- RCAdf$adr_scope_RECON[match(df$adrorg, RCAdf$adrorg)]
length(unique(df$adrscope))
grps <- as.factor(df$adrscope)
q <- qplot(x = n, y = reorder(adrorg, n), data = df, 
           color = factor(grps), geom = "point") +   
  facet_grid(source_country ~., scales = "free_y")

# Now lets see which addressees were most visible as claimants pre and during covid, color
# coded by adr type.
df <- RCAdf %>% 
  filter(!is.na(adrorg)) %>%
  count(adrname, period) %>%
  ungroup() %>%
  group_by(period) %>%
  top_n(20)
#Assign colors by adrtype
df$adrtype <- RCAdf$adr_type[match(df$adrname, RCAdf$adrname)]
length(unique(df$adrtype))
grps <- as.factor(df$adrtype)
q <- qplot(x = n, y = reorder(adrname, n), data = df, 
           color = factor(grps), geom = "point") +   
  facet_grid(period ~., scales = "free_y")

# The below plot clearly illustrates that gov/exec 'win' and politicians/parties 'lose'
# in terms of agency as claimants (political entreprenuers)

df <- RCAdf %>% 
  filter(!is.na(adrorg)) %>%
  count(adr_type, period) %>%
  ungroup() %>%
  group_by(period) %>%
  top_n(10)
q <- qplot(x = n, y = reorder(adr_type, n), data = df, geom = "point") +   
  facet_grid(period ~., scales = "free_y")


#Compare adrscope visbility by source. The results below clearly show that tabloids
# are more parochial in terms of adr visibility. More nationalised / domesticated
# in this respect.

df <- RCAdf %>% 
  filter(!is.na(adrorg)) %>%
  count(adr_scope_RECON, 
        newsp_type, sort = T) %>%
  ungroup() %>%
  group_by(newsp_type) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(adr_scope_RECON, n), data = df, 
           color = factor(adr_scope_RECON), geom = "point") +   
  facet_grid(newsp_type  ~., scales = "free_y")

# Lets now compare across different newspapers....

df <- RCAdf %>% 
  filter(!is.na(adrorg)) %>%
  count(adr_scope_RECON, 
        source, sort = T) %>%
  ungroup() %>%
  group_by(source) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(adr_scope_RECON, n), data = df, 
           color = factor(adr_scope_RECON), geom = "point") +   
  facet_grid(source  ~., scales = "free_y")

# Lets compare across countries....

df <- RCAdf %>% 
  filter(!is.na(adrorg)) %>%
  count(adr_scope_RECON, 
        source_country, sort = T) %>%
  ungroup() %>%
  group_by(source_country) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(adr_scope_RECON, n), data = df, 
           color = factor(adr_scope_RECON), geom = "point") +   
  facet_grid(source_country  ~., scales = "free_y")

# And compare pre and during covid

df <- RCAdf %>% 
  filter(!is.na(adrorg)) %>%
  count(adr_scope_RECON, 
        period, sort = T) %>%
  ungroup() %>%
  group_by(period) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(adr_scope_RECON, n), data = df, 
           color = factor(adr_scope_RECON), geom = "point") +   
  facet_grid(period  ~., scales = "free_y")


#####################################################################
######### CLAIMANTS AND ADDRESSEEES AGGREGATED#######################
#Measure and plot the visibility distribution of actors (both C's and A's)
#Measure and plot the visibility distribution of Cs and As
library(ggplot2)
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
dfact <- RCAdf %>% select(actname,actorg, act_type, act_nat,act_scope,act_scope_RECON, period, source_country,newsp_type, source)
dfadr <- RCAdf %>% select(adrname,adrorg, adr_type, adr_nat,adr_scope,adr_scope_RECON, period, source_country, newsp_type, source)
colnames(dfact)<-c('actname', 'actorg', 'act_type', 'act_nat', 'act_scope','act_scope_RECON', 'period', 'source_country', 'newsp_type', 'source')
colnames(dfadr)<-c('actname', 'actorg', 'act_type', 'act_nat', 'act_scope','act_scope_RECON', 'period', 'source_country', 'newsp_type', 'source')
RCAdf <- rbind(dfact, dfadr)
RCAdf <- RCAdf %>% filter(!is.na(actorg))

df4 <- RCAdf %>% 
  filter(!is.na(actorg)) %>%
  group_by(actname) %>%
  summarise(n = n())
dfmean <- summarise(df4, mean_n = mean(n))
# Average number of claims per actor is 2.61
df4 <- RCAdf %>% 
  filter(!is.na(actorg)) %>%
  group_by(actorg) %>%
  summarise(n = n())
dfmean <- summarise(df4, mean_n = mean(n))
#When claimants are aggregated, the number increases to 3.83 per actor.
# Lets compare average no. claims by claimant region

df <- RCAdf %>% 
  filter(!is.na(actorg)) %>%
  count(act_scope) 

# The dataframe above shows that national-level actors dominate the show as claimants. In excess
# of 4417 claims vis-a-vis 1415 for EU-level actors.

df <- RCAdf %>% 
  filter(!is.na(actorg)) %>%
  count(act_scope_RECON) 

# And of those circa 6000 national claims, 2481 are domestic (same country as where
# newspaper is published vis-a-vis 1415). In other words, 45-50% are 'own country national'.

df <- RCAdf %>% 
  count(parfam, sort = T)

# This dataframe above show the distibution of claims by party family. 

df <- RCAdf %>% 
  count(act_type, sort = T)

# Unsurprisingly, gov/exec actors dominate in terms of agency (almost half claims (2894 vis
# -a-vis only 717 for political parties)). 

# Lets include the variable of crisis to see changes in representation.
library(ggplot2)
RCAdf %>% filter(!is.na(actorg)) %>%
  count(act_type, period, sort = T) %>%
  ggplot(., aes(x = period, y = n, fill = act_type)) +
  geom_bar(stat = "identity", position ="dodge") + coord_flip()


#Measure and plot the visibility distribution of actors (claimants only)
df5 <- RCAdf %>% 
  group_by(actname) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq)) %>%
  top_n(48)

# Dot chart of a single numeric vector
dotchart(df5$n, labels = df5$actname,
         cex = 0.9, xlab = "No. of claims (top 50)")

# Plot and color by groups cyl
df5$actscope <- RCAdf$act_scope[match(df5$actname, RCAdf$actname)]
df5$actscope[which(df5$actname == "Von der Leyen Ursula")] = "EUROPEAN UNION"
df5$actscope[which(df5$actname == "Gentiloni Paolo")] = "EUROPEAN UNION"
length(unique(df5$actscope))
library(RColorBrewer)
grps <- as.factor(df5$actscope)
my_cols <- c("Blue","Red", "Black")
dotchart(df5$n, labels = df5$actname,
         gcolor = my_cols,
         color = my_cols[grps],
         cex = 0.9, pch = 19, xlab = "No. of claims (top 50)")

# Do the same for the actorg variable.
#Measure and plot the visibility distribution of act organisations (claimants only)
df5 <- RCAdf %>% 
  group_by(actorg) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq)) %>%
  top_n(50)

# Dot chart of a single numeric vector
dotchart(df5$n, labels = df5$actorg,
         cex = 0.9, xlab = "No. of claims (top 50)")

# Plot and color by groups cyl
df5$actscope <- RCAdf$act_scope[match(df5$actorg, RCAdf$actorg)]
length(unique(df5$actscope))
library(RColorBrewer)
grps <- as.factor(df5$actscope)
my_cols <- c("Blue","Red", "Black")
dotchart(df5$n, labels = df5$actorg,
         gcolor = my_cols,
         color = my_cols[grps],
         cex = 0.9, pch = 19, xlab = "No. of claims (top 50)")


# Lets do a count of actorgs by country and facet wrap (for more on qplot, see this link: http://www.sthda.com/english/wiki/qplot-quick-plot-with-ggplot2-r-software-and-data-visualization)
df <- RCAdf %>% 
  count(actorg, source_country) %>%
  ungroup() %>%
  group_by(source_country) %>%
  top_n(10) 
# Assign colors by actscope 
df$actscope <- RCAdf$act_scope[match(df$actorg, RCAdf$actorg)]
length(unique(df$actscope))
grps <- as.factor(df$actscope)
my_cols <- c("Blue","Black")
q <- qplot(x = n, y = reorder(actorg, n), data = df, 
           color = factor(grps), geom = "point") +   
  facet_grid(source_country ~., scales = "free_y")

# OR, assign colors by actscope (as per RECONNECT)
df$actscope <- RCAdf$act_scope_RECON[match(df$actorg, RCAdf$actorg)]
length(unique(df$actscope))
grps <- as.factor(df$actscope)
q <- qplot(x = n, y = reorder(actorg, n), data = df, 
           color = factor(grps), geom = "point") +   
  facet_grid(source_country ~., scales = "free_y")

# Now lets see which actors were most visible as claimants pre and during covid, color
# coded by actor type.
df <- RCAdf %>% 
  count(actname, period) %>%
  ungroup() %>%
  group_by(period) %>%
  top_n(20)
#Assign colors by acttype
df$acttype <- RCAdf$act_type[match(df$actname, RCAdf$actname)]
length(unique(df$acttype))
grps <- as.factor(df$acttype)
q <- qplot(x = n, y = reorder(actname, n), data = df, 
           color = factor(grps), geom = "point") +   
  facet_grid(period ~., scales = "free_y")

# The below plot clearly illustrates that gov/exec 'win' and politicians/parties 'lose'
# in terms of agency as claimants (political entreprenuers)

df <- RCAdf %>% 
  count(act_type, period) %>%
  ungroup() %>%
  group_by(period) %>%
  top_n(10)
q <- qplot(x = n, y = reorder(act_type, n), data = df, geom = "point") +   
  facet_grid(period ~., scales = "free_y")


#Compare actscope visbility by source. The results below clearly show that tabloids
# are more parochial in terms of claimantactor visibility. More nationalised / domesticated
# in this respect.

df <- RCAdf %>% 
  count(act_scope_RECON, 
        newsp_type, sort = T) %>%
  ungroup() %>%
  group_by(newsp_type) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(act_scope_RECON, n), data = df, 
           color = factor(act_scope_RECON), geom = "point") +   
  facet_grid(newsp_type  ~., scales = "free_y")

# Lets now compare across different newspapers....

df <- RCAdf %>% 
  count(act_scope_RECON, 
        source, sort = T) %>%
  ungroup() %>%
  group_by(source) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(act_scope_RECON, n), data = df, 
           color = factor(act_scope_RECON), geom = "point") +   
  facet_grid(source  ~., scales = "free_y")

# Lets compare across countries....

df <- RCAdf %>% 
  count(act_scope_RECON, 
        source_country, sort = T) %>%
  ungroup() %>%
  group_by(source_country) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(act_scope_RECON, n), data = df, 
           color = factor(act_scope_RECON), geom = "point") +   
  facet_grid(source_country  ~., scales = "free_y")

# And compare pre and during covid

df <- RCAdf %>% 
  count(act_scope_RECON, 
        period, sort = T) %>%
  ungroup() %>%
  group_by(period) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(act_scope_RECON, n), data = df, 
           color = factor(act_scope_RECON), geom = "point") +   
  facet_grid(period  ~., scales = "free_y")

# Compare act_scope across news_type

df <- RCA19 %>%
count(act_scope_RECON,
newsp_type, sort = T) %>%
ungroup() %>%
group_by(newsp_type) %>%
mutate(freq = n / sum(n))
q <- qplot(x = freq, y = reorder(act_scope_RECON, n), data = df,
color = factor(act_scope_RECON), geom = "point") +
facet_grid(newsp_type  ~., scales = "free_y")

###############################################################################
##########OBJECT###############################################################
#Examine object visibility 
#Measure and plot visiblity of object_scopes
# Lets do a count of objs by country and facet wrap (for more on qplot, see this link: http://www.sthda.com/english/wiki/qplot-quick-plot-with-ggplot2-r-software-and-data-visualization)
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
df <- RCAdf %>% 
  filter(!is.na(objs) & !is.na(obj_scope_RECON)) %>% 
  count(objs, source_country, obj_scope_RECON) %>%
  ungroup() %>%
  group_by(source_country) %>%
  top_n(10) 
q <- qplot(x = n, y = reorder(objs, n), data = df, 
           color = factor(obj_scope_RECON), geom = "point") +   
  facet_grid(source_country ~., scales = "free_y")

# Do the same but according to objnat

df <- RCAdf %>% 
  filter(!is.na(objs) & !is.na(obj_scope_RECON)) %>% 
  count(objnat, source_country, obj_scope_RECON) %>%
  ungroup() %>%
  group_by(source_country) %>%
  top_n(10) 
q <- qplot(x = n, y = reorder(objnat, -n), data = df, 
           color = factor(obj_scope_RECON), geom = "point") +   
  facet_grid(source_country ~., scales = "free_y")


#Measure and plot the visibility distribution of object / constituencies

df4 <- RCAdf %>% 
  group_by(objs) %>%
  summarise(n = n())
dfmean <- summarise(df4, mean_n = mean(n))

# Average number of objects is 6.

df4 <- RCAdf %>% 
  group_by(objnat) %>%
  summarise(n = n())
dfmean <- summarise(df4, mean_n = mean(n))
#When objects are aggregated by nationality, the number increases to 73 times per object.
# Lets compare average no. obj claims by obj region

df <- RCAdf %>% 
  count(obj_scope) 

# The dataframe above shows that national objects dominate. In excess
# of 724 claims vis-a-vis 420 for EU-level / supranational identity.

df <- RCAdf %>% 
  count(obj_scope_RECON) 

# And of those circa 1200 claims, 470 are domestic (same country as where
# newspaper is published vis-a-vis 424 for EU). In other words, 55% are 'own country national'.
# This dataframe above show the distibution of claims by party family. 

df <- RCAdf %>% 
  count(objtype, sort = T)

# Unsurprisingly, 'polity' and 'citizens / taxpayers' is most common object type. 
# Lets include the variable of crisis to see changes in representation.

library(ggplot2)
df <- RCAdf %>% filter(!is.na(objtype)) 
p <- ggplot(data = df, aes(x = period, fill = objtype)) +
  geom_bar(position = "fill")


#Measure and plot the visibility distribution of objects
df5 <- RCAdf %>% 
  filter(!is.na(objs)) %>%
  group_by(objs) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq)) %>%
  top_n(48)

# Dot chart of a single numeric vector
dotchart(df5$freq, labels = df5$objs,
         cex = 0.9, xlab = "No. of claims (top 50)")

# Plot and color by groups cyl
df5$objscope <- RCAdf$obj_scope[match(df5$objs, RCAdf$objs)]
grps <- as.factor(df5$objscope)
my_cols <- c("Blue","Red", "Black")
dotchart(df5$freq, labels = df5$objs,
         gcolor = my_cols,
         color = my_cols[grps],
         cex = 0.9, pch = 19, xlab = "No. of claims (top 50)")

# Do the same for the objnat variable.
#Measure and plot the visibility distribution of orgnats.

df5 <- RCAdf %>%
  filter(!is.na(objnat)) %>%
  group_by(objnat) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq)) %>%
  top_n(20)

# Dot chart of a single numeric vector
dotchart(df5$n, labels = df5$objnat,
         cex = 0.9, xlab = "No. of claims (top 20)")

# Plot and color by groups cyl
df5$objscope <- RCAdf$obj_scope[match(df5$objnat, RCAdf$objnat)]
grps <- as.factor(df5$objscope)
my_cols <- c("Blue","Red", "Black")
dotchart(df5$freq, labels = df5$objnat,
         gcolor = my_cols,
         color = my_cols[grps],
         cex = 0.9, pch = 19, xlab = "No. of claims (top 20)")


# Lets do a count of actnats by country and facet wrap (for more on qplot, see this link: http://www.sthda.com/english/wiki/qplot-quick-plot-with-ggplot2-r-software-and-data-visualization)
df <- RCAdf %>% 
  filter(!is.na(objnat)) %>%
  count(objnat, source_country) %>%
  ungroup() %>%
  group_by(source_country) %>%
  top_n(10) 
# Assign colors by objnat
df$objscope <- RCAdf$obj_scope[match(df$objnat, RCAdf$objnat)]
length(unique(df$objscope))
grps <- as.factor(df$objscope)
my_cols <- c("Blue","Black", "red")
q <- qplot(x = n, y = reorder(objnat, n), data = df, 
           color = factor(grps), geom = "point") +   
  facet_grid(source_country ~., scales = "free_y")

# OR, assign colors by objscope (as per RECONNECT)
df$objscope <- RCAdf$obj_scope_RECON[match(df$objnat, RCAdf$objnat)]
length(unique(df$objscope))
grps <- as.factor(df$objscope)
q <- qplot(x = n, y = reorder(objnat, n), data = df, 
           color = factor(grps), geom = "point") +   
  facet_grid(source_country ~., scales = "free_y")

# Now lets see which objs were most visible pre and during covid, color
# coded by obj_type.

df <- RCAdf %>% 
  filter(!is.na(objtype) & !is.na(objs)) %>%
  count(objs, period) %>%
  ungroup() %>%
  group_by(period) %>%
  top_n(20)
#Assign colors by objtype
df$objtype <- RCAdf$objtype[match(df$objs, RCAdf$objs)]
grps <- as.factor(df$objtype)
q <- qplot(x = n, y = reorder(objs, n), data = df, 
           color = factor(grps), geom = "point") +   
  facet_grid(period ~., scales = "free_y")

###

df <- RCAdf %>% 
  filter(!is.na(objtype)) %>%
  count(objtype, period) %>%
  ungroup() %>%
  group_by(period) %>%
  top_n(10)
q <- qplot(x = n, y = reorder(objtype, n), data = df, geom = "point") +   
  facet_grid(period ~., scales = "free_y")


#######

df <- RCAdf %>% 
  filter(!is.na(obj_scope_RECON)) %>%
  count(obj_scope_RECON, 
        newsp_type, sort = T) %>%
  ungroup() %>%
  group_by(newsp_type) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(obj_scope_RECON, n), data = df, 
           color = factor(obj_scope_RECON), geom = "point") +   
  facet_grid(newsp_type  ~., scales = "free_y")

# Lets now compare across different newspapers....

df <- RCAdf %>%
  filter(!is.na(obj_scope_RECON)) %>%
  count(obj_scope_RECON, 
        source, sort = T) %>%
  ungroup() %>%
  group_by(source) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(obj_scope_RECON, n), data = df, 
           color = factor(obj_scope_RECON), geom = "point") +   
  facet_grid(source  ~., scales = "free_y")

# Lets compare across countries....

df <- RCAdf %>% 
  filter(!is.na(obj_scope_RECON)) %>%
  count(obj_scope_RECON, 
        source_country, sort = T) %>%
  ungroup() %>%
  group_by(source_country) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(obj_scope_RECON, n), data = df, 
           color = factor(obj_scope_RECON), geom = "point") +   
  facet_grid(source_country  ~., scales = "free_y")

# And compare pre and during covid

df <- RCAdf %>% 
  filter(!is.na(obj_scope_RECON)) %>%
  count(obj_scope_RECON, 
        period, sort = T) %>%
  ungroup() %>%
  group_by(period) %>% 
  mutate(freq = n / sum(n))

q <- qplot(x = freq, y = reorder(obj_scope_RECON, n), data = df, 
           color = factor(obj_scope_RECON), geom = "point") +   
  facet_grid(period  ~., scales = "free_y")

############## More data visualisations 18/06/21 ##################
library(dplyr)
RCAdf %>% 
  filter(!is.na(act2obj)) %>%
  aggregate(EUval ~ act2obj, ., mean)

# The above shows that horizontal europeanisation followed by national scopes
# are the most critical of the EU.

###################### charts still to add to results word document #############
         
library(dplyr)
mean <- RCAdf %>% 
  filter(!is.na(act2obj)) %>%
  aggregate(EUval ~ act_type, ., mean) 
n <- RCAdf %>%
  count(act2obj, act_type) %>%
  ungroup() %>%
  group_by(act_type) %>%
  mutate(rel.freq = n / sum(n)) %>% 
  filter(act2obj == "national" & !is.na(act2obj) & !is.na(act_type))
df2 <- mean %>% semi_join(n)
df2 <- df2 %>% left_join(n)

# Scatterplot
  ggplot(df2, aes(x=EUval, y=rel.freq)) + 
  geom_point(aes(col=act_type, size=n)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="scatterplot to examine correlation b/w EU valence and no. of national identity scopes", 
       y="rel.freq", 
       x="meanEUval", 
       title="Scatterplot")

  
##### Now create scatterplot re parfam ##########
  
  library(dplyr)
  library(ggplot2)
  library(readxl)
  library(tidyverse)
  library(hrbrthemes)
  library(viridis)
  RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
  mean <- RCAdf %>% 
    filter(!is.na(act2obj)) %>%
    aggregate(EUval ~ parfam, ., mean) 
  n <- RCAdf %>%
    count(act2obj, parfam) %>%
    ungroup() %>%
    group_by(parfam) %>%
    mutate(rel.freq = n / sum(n)) %>% 
    filter(act2obj == "national" & !is.na(act2obj) & !is.na(parfam) & n > 5)
  df2 <- mean %>% semi_join(n)
  df2 <- df2 %>% left_join(n)
  # Scatterplot
  ggplot(df2, aes(x=EUval, y=rel.freq)) + 
    geom_point(aes(col=parfam, size=n)) + 
    geom_smooth(method="loess", se=F) + 
    labs(subtitle="scatterplot to examine correlation b/w EU valence and no. of national identity scopes", 
         y="rel.freq", 
         x="meanEUval", 
         title="Scatterplot")
  
  
######## http://www.sthda.com/english/wiki/be-awesome-in-ggplot2-a-practical-guide-to-be-highly-effective-r-software-and-data-visualization

  # Histogram  plot
  # Change histogram fill color by group (sex)
  RCAdf <- RCAdf %>% filter(!is.na(act2obj) & EUval != 0)
  qplot(EUval, data = RCAdf, geom = "histogram",
        fill = act2obj)
 
######## Discrete variables analysis
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCAdf <- RCAdf %>% filter(!is.na(act2adr))
data(RCAdf)
b <- ggplot(RCAdf, aes(act2adr)) 
b + geom_bar() + coord_flip()
b + geom_bar(fill = "steelblue", color ="steelblue") +
  theme_minimal() + coord_flip()

#### Two discrete variables act2adr and act_type
ggplot(RCAdf, aes(act2adr, act_type)) +
  geom_jitter(aes(color = act_type), size = 0.5)

### discrete variable analysis of object
RCAdf <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCAdf <- RCAdf %>% filter(!is.na(act2obj))
data(RCAdf)
b <- ggplot(RCAdf, aes(act2obj)) 
b + geom_bar() + coord_flip()
b + geom_bar(fill = "steelblue", color ="steelblue") +
  theme_minimal() + coord_flip()

##Two discrete variables act2obj and act_type
ggplot(RCAdf, aes(act2obj, act_type)) +
  geom_jitter(aes(color = act_type), size = 0.5)
# The plot above clearly shows that gov actors, political parties, and the media are drivers or national identity scopes.
  
##
ggplot(RCAdf, aes(act2obj, parfam)) +
  geom_jitter(aes(color = parfam), size = 0.5)

## We can see that Poland and Italy are most nationalistic
ggplot(RCAdf, aes(act2obj, source_country)) +
  geom_jitter(aes(color = source_country), size = 0.5)

# By frame_type 
ggplot(RCAdf, aes(act2obj, frame_type)) +
  geom_jitter(aes(color = frame_type), size = 0.5)

# By issue
ggplot(RCAdf, aes(act2obj, issfield)) +
  geom_jitter(aes(color = issfield), size = 0.5)

# By issue field and frame
ggplot(RCAdf, aes(frame_type, issfield)) +
  geom_jitter(aes(color = issfield), size = 0.5)

# By issue field and country
ggplot(RCAdf, aes(source_country, issfield)) +
  geom_jitter(aes(color = issfield), size = 0.5)

# density plot re identity scope and source_country 
RCAdf %>% 
  count(act2obj, source_country) %>%
  ggplot(., aes(n, fill = source_country)) +
  geom_density()

# density plot re time and source_country 
RCAdf %>% 
  count(time, source_country) %>%
  ggplot(., aes(n, fill = source_country)) +
  geom_density()

# freq polygons - If you want the y-axis to 
# represent count rather than density, try geom_freqpoly().

RCAdf %>% 
  count(issfield, source_country) %>%
  ggplot(., aes(n, color = source_country)) +
  geom_freqpoly()

# https://psyteachr.github.io/msc-data-skills/ggplot.html 


#Calculate no. of identity scopes according to nationality of political actors

df2 <- RCAdf %>% 
  filter(act_region == "EUROPE") %>%
  count(act2obj, act_nat, sort = T) %>%
  ungroup() %>%
  group_by(act_nat) %>%
  mutate(rel.freq = n / sum(n)) %>% 
  filter(!is.na(act2obj) & act2obj == 'national')

# Or alternatively, filter by 'bottom-up vertical Europeanisation' (i.e.)
# national actors invoking a European identity...

df2 <- RCAdf %>% 
  filter(act_region == "EUROPE") %>%
  count(act2obj, act_nat, sort = T) %>%
  ungroup() %>%
  group_by(act_nat) %>%
  mutate(rel.freq = n / sum(n)) %>% 
  filter(!is.na(act2obj) & act2obj == 'bottom-up vertical Europeanisation')

# Then the next step is to observe recent eurobarometer trends re European identity 
# and create a scatter plot to see if there is a relation between the EU-feeling
# of a member state and proclivity of political actors to invoke a EU-identity...
# file:///C:/Users/kerme/Downloads/ANNEX-Spring_2019_Standard_Eurobarometer.pdf.pdf

act2obj_actnat <- read_csv("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/act2obj_actnat.csv")
x <- act2obj_actnat$`EU identity`
y <- act2obj_actnat$rel.freq
plot(x, y, main = "Main title",
     xlab = "EU identity", ylab = "rel.freq",
     pch = 19, frame = FALSE)

# The same for national...
# https://www.businessinsider.com/survey-data-on-how-europeans-identify-themselves-2016-6?r=US&IR=T

df2 <- RCAdf %>% 
  filter(act_region == "EUROPE") %>%
  count(act2obj, act_nat, sort = T) %>%
  ungroup() %>%
  group_by(act_nat) %>%
  mutate(rel.freq = n / sum(n)) %>% 
  filter(!is.na(act2obj) & act2obj == 'national')

write.csv(df2, 'act2objact_nat_nationalisedties.csv')
act2obj_actnat <- read_csv("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/act2objact_nat_nationalisedties.csv")
x <- act2obj_actnat$`national identity`
y <- act2obj_actnat$rel.freq
plot(x, y, main = "Main title",
     xlab = "national identity", ylab = "rel.freq",
     pch = 19, frame = FALSE)

# The results show there is no correlation between national we-feeling and no. of discursive national identity scopes...








#########distribution of actobj scopes by newsp_type#########
library(tidyverse)
library(hrbrthemes)
library(viridis)

RCA19 %>%
  filter(!is.na(act_scope_RECON)) %>%
  count(act_scope_RECON, newsp_type, source_country) %>%
  ungroup() %>%
  group_by(newsp_type,source_country) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=act_scope_RECON, y=rel.freq, fill=act_scope_RECON)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of actscopes by newsp_type") +
  xlab("") + coord_flip() 


#########distribution of obj scopes by newsp_type#########

RCA19 %>%
  filter(!is.na(obj_scope_RECON)) %>%
  count(obj_scope_RECON, newsp_type, source_country) %>%
  ungroup() %>%
  group_by(newsp_type,source_country) %>%
  mutate(rel.freq = n / sum(n)) %>%
  ggplot(., aes(x=obj_scope_RECON, y=rel.freq, fill=obj_scope_RECON)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("distribution of objscopes by newsp_type") +
  xlab("") + coord_flip() + facet_wrap(~ source_country)

##########

RCA20 <- RCA19 %>%  mutate(bool_act2obj = ifelse(grepl("Europeanisation", act2obj), "1", ifelse(grepl("national", act2obj), "-1", "0")))
RCA21 <- RCA20 %>%  mutate(bool_act2adr = ifelse(grepl("Europeanisation", act2adr), "1", ifelse(grepl("national", act2adr), "-1", "0")))
RCA21$bool_act2obj <- as.numeric(RCA21$bool_act2obj)
RCA21$bool_act2adr <- as.numeric(RCA21$bool_act2adr)

#Scatterplot to examine relation between sentiment and EUval by source
act2obj_source_bool <- aggregate(bool_act2obj ~ source, RCA21, mean)
act2adr_source_bool <- aggregate(bool_act2adr ~ source, RCA21, mean)
df <- cbind(act2obj_source_bool, act2adr_source_bool)
colnames(df) <- make.unique(names(df))
p <- ggplot(df, aes(x=bool_act2obj, y=bool_act2adr)) + geom_point() + 
  ggtitle("scatterplot to examine correlation between structural and normative Europeanisation") 







                         

                     
############## Create a 3D scatterplot
library(rgl)
RCA22 <- RCA21 %>%  mutate(bool_act = ifelse(grepl("EU", act_scope_RECON), "1", ifelse(grepl("National", act_scope_RECON), "-1", "0")))
RCA22 <- RCA22 %>%  mutate(bool_adr = ifelse(grepl("EU", adr_scope_RECON), "1", ifelse(grepl("National", adr_scope_RECON), "-1", "0")))
RCA22 <- RCA22 %>%  mutate(bool_obj = ifelse(grepl("EU", obj_scope_RECON), "1", ifelse(grepl("National", obj_scope_RECON), "-1", "0")))
RCA22$bool_obj <- as.numeric(RCA22$bool_obj)
RCA22$bool_act <- as.numeric(RCA22$bool_act)
RCA22$bool_adr <- as.numeric(RCA22$bool_adr)
write.xlsx2(RCA22, "RCA22.xlsx")

## For country
act_bool <- aggregate(bool_act ~ source_country, RCAadrobjfilt, mean)
adr_bool <- aggregate(bool_adr ~ source_country, RCAadrobjfilt, mean)
obj_bool <- aggregate(bool_obj ~ source_country, RCAadrobjfilt, mean)
df <- cbind(act_bool, adr_bool, obj_bool)
colnames(df) <- make.unique(names(df))
plot3d( 
  x=df$bool_act, y=df$bool_adr, z=df$bool_obj, 
  type = 's', 
  radius = .1,
  xlab="act_scope", ylab="adr_scope", zlab="obj_scope")




## For source

act_bool <- aggregate(bool_act ~ source, RCAadrobjfilt, mean)
adr_bool <- aggregate(bool_adr ~ source, RCAadrobjfilt, mean)
obj_bool <- aggregate(bool_obj ~ source, RCAadrobjfilt, mean)
df <- cbind(act_bool, adr_bool, obj_bool)
colnames(df) <- make.unique(names(df))

plot3d( 
  x=df$bool_act, y=df$bool_adr, z=df$bool_obj, 
  type = 's', 
  radius = .1,
  xlab="act_scope", ylab="adr_scope", zlab="obj_scope")






######OR USE SCATTERPLOT3D PACKAGE
install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
# Basic 3d graphics
scatterplot3d(df[,c(2,4,6)])

# Change the angle of point view
scatterplot3d(df[,c(2,4,6)], angle = 55)

# Change main title and axis labels 
scatterplot3d(df[,c(2,4,6)], main="3D Scatter Plot",
              xlab = "act_scope",
              ylab = "adr_scope",
              zlab = "obj_scope")

# Change the shape and the color of points
# Change main title and axis labels 
scatterplot3d(df[,c(2,4,6)], main="3D Scatter Plot",
              xlab = "act_scope",
              ylab = "adr_scope",
              zlab = "obj_scope", pch = 16, color="steelblue")


#Add bars
scatterplot3d(df[,c(2,4,6)], main="3D Scatter Plot",
              xlab = "act_scope",
              ylab = "adr_scope",
              zlab = "obj_scope", pch = 16, color="steel blue", type="h")


# Add point labels
s3d <- scatterplot3d(df[,c(2,4,6)], main="3D Scatter Plot",
              xlab = "act_scope",
              ylab = "adr_scope",
              zlab = "obj_scope", pch = 16, color="steel blue", type="h")
text(s3d$xyz.convert(df[,c(2,4,6)]), labels = df$source,
     cex= 0.7, col = "steelblue")

# Add regression plane

my.lm <- lm(df$bool_obj ~ df$bool_act + df$bool_adr)
s3d$plane3d(my.lm)


#Add grids on the different factes of scatterplot3d graphics

# 1. Source the function
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
# 2. 3D scatter plot
scatterplot3d(df[,c(2,4,6)], pch = 16, grid=FALSE, box=FALSE)
# 3. Add grids
addgrids3d(df[,c(2,4,6)], grid = c("xy", "xz", "yz"))



###The problem on the above plot is that the grids are drawn over the points.
# 1. Source the function
source('~/hubiC/Documents/R/function/addgrids3d.r')

# 2. Empty 3D scatter plot using pch=""

s3d <- scatterplot3d(df[,c(2,4,6)], pch = "", grid=FALSE, box=FALSE)
# 3. Add grids
addgrids3d(df[,c(2,4,6)], grid = c("xy", "xz", "yz"))
# 4. Add points
s3d$points3d(df[,c(2,4,6)], pch = 16)



## For source

act_bool <- aggregate(bool_act ~ parfam, RCA22, mean)
adr_bool <- aggregate(bool_adr ~ parfam, RCA22, mean)
obj_bool <- aggregate(bool_obj ~ parfam, RCA22, mean)
df <- cbind(act_bool, adr_bool, obj_bool)
colnames(df) <- make.unique(names(df))
# Add point labels
s3d <- scatterplot3d(df[,c(2,4,6)], main="3D Scatter Plot",
                     xlab = "act_scope",
                     ylab = "adr_scope",
                     zlab = "obj_scope", pch = 16, color="steel blue", type="h")
text(s3d$xyz.convert(df[,c(2,4,6)]), labels = df$parfam,
     cex= 0.7, col = "steelblue")

# Add regression plane

my.lm <- lm(df$bool_obj ~ df$bool_act + df$bool_adr)
s3d$plane3d(my.lm)


# For act_type

act_bool <- aggregate(bool_act ~ act_type, RCA22, mean)
adr_bool <- aggregate(bool_adr ~ act_type, RCA22, mean)
obj_bool <- aggregate(bool_obj ~ act_type, RCA22, mean)
df <- cbind(act_bool, adr_bool, obj_bool)
colnames(df) <- make.unique(names(df))
# Add point labels
s3d <- scatterplot3d(df[,c(2,4,6)], main="3D Scatter Plot",
                     xlab = "act_scope",
                     ylab = "adr_scope",
                     zlab = "obj_scope", pch = 16, color="steel blue", type="h")
text(s3d$xyz.convert(df[,c(2,4,6)]), labels = df$act_type,
     cex= 0.7, col = "steelblue")

# Add regression plane

my.lm <- lm(df$bool_obj ~ df$bool_act + df$bool_adr)
s3d$plane3d(my.lm)
