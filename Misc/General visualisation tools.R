#Extract Month and Year From Date in R
df4$Month_Yr <- format(as.Date(df4$date), "%Y-%m")

# Count no. of articles by month
df5 <- df4 %>% group_by(Month_Yr) %>% count() 

#Create a simple ggplot
p1 <- ggplot(data=df5, aes(x=Month_Yr, y=n, group = 1)) +
  geom_line()+
  geom_point()

# Count no. of articles by paper and by month
df6 <- df4 %>% group_by(Month_Yr, source) %>% count()

#Create a ggplot
p2 <- ggplot(data=df6, aes(x=Month_Yr, y=n, group=source, color = source)) +
  geom_line()+
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  

#Create a ggplot with smooth & faceted, removing the x labels, legend, and changing the y scale.
p3 <- ggplot(data=df6, aes(x=Month_Yr, y=n, group = source)) +
  geom_line()+
  geom_smooth() + 
  geom_point() +
  ylim(0, 40) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_wrap(~source, ncol = 4)

# Create 'country' column
df4newcol  <- df4 %>%  mutate(country = ifelse(source == "Fact Poland" | source == "Gazeta Wyborcza", "Poland", ifelse(source == "Bild" | source == "Süddeutsche Zeitung", "Germany", ifelse(source == "De Telegraaf" | source == "NRC Handelsblad", "Netherlands", ifelse(source == "Corriere della Sera (Italy)" | source == "La Nazione (Italy)", "Italy", NA)))))

#Search no. of NA values in a column
sum(is.na(df4$country))

# Filter by date for pre-covid and during covid dataset.
pc <- filter(df4newcol, date >= as.Date("2019-03-15"), date <= as.Date("2020-02-25"))
dc <- filter(df4newcol, date >= as.Date("2020-02-26"), date <= as.Date("2021-01-25"))

#Bind rows and add to main dataset
dfnew <- bind_rows(pc %>% mutate(period = "per-covid"),
dc %>% mutate(period = "during covid"))

df7 <- dfnew %>% group_by(period) %>% count() 
p4 <-ggplot(data=df7, aes(reorder(x=period, y=n))) +
geom_bar(stat="identity")

#Count no. of articles by period
df8 <- dfnew %>% count(period) %>% arrange(desc(n))

library(ggplot2)
# Basic barplot to compare pre to during covid
p5 <-ggplot(data=df8, aes(x=period, y=n)) +
  geom_bar(stat="identity")

# Period comparison geom_line 
df9 <- dfnew %>% group_by(period, Month_Yr) %>%  count(period) %>% arrange(Month_Yr)
p6 <- ggplot(data=df9, aes(x=Month_Yr, y=n, group=period, color = period)) +
  geom_line()+
  geom_point()

# Period comparison geom_line with facet
p7 <- ggplot(data=df9, aes(x=Month_Yr, y=n, group = period, color = period)) +
  geom_line()+
  geom_point() + facet_wrap(~period, ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Country comparison geom_line
df10 <- dfnew %>% group_by(country, Month_Yr) %>%  count(country) %>% arrange(Month_Yr)
p8 <- ggplot(data=df10, aes(x=Month_Yr, y=n, group=country, color = country)) +
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Country comparison with facet
p9 <- ggplot(data=df10, aes(x=Month_Yr, y=n, group=country, color = country)) +
  geom_line()+
  geom_point()+
  geom_smooth()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_wrap(~ country, ncol = 4) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

# Mean no. of articles 
df_av <- df10 %>%
  group_by(country) %>%
  summarize(mean = mean(n, na.rm = TRUE))
library(ggplot2)
# Basic barplot
p10 <-ggplot(data=df_av2, aes(x=country, y=mean)) +
  geom_bar(stat="identity", color="blue") +
  coord_flip() +
  geom_text(aes(label=mean), vjust=1.6, color="white", size=3.5)

#Round numbers to two decimal places
format(round(x, 2), nsmall = 2)

#OR with mutate and round function
df_av2 <- df_av %>% mutate(mean = round(df_av$mean, digits = 2))
round(df_av$mean, digits = 2)
  
# Basic barplot
p11 <-ggplot(data=df_av2, aes(x=country, y=mean, fill = country)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=mean), vjust=1.6, color="black", size=3.5)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


