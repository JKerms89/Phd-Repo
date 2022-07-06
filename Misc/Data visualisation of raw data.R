load("PhDdf_final.Rda")
df_final2 <- df_final %>% group_by(source,date) %>% count()

#Barplot from raw data

p2 <- ggplot(df_final2, aes(x=source, y=n, fill = source)) +
geom_bar(stat="identity") +
coord_flip()

# To calculate and represent the no. of days articles were found. For example, 
# we can see from the data that circa only 50% of the time was there an 
#article published in bild from the sample of 45 days.

ggplot(df_2, aes(x=factor(source))) +
geom_bar(stat="count", width=0.7, fill="steelblue")+
theme_minimal()+
coord_flip()

# Amend date format
df_2 <- df_final2 %>% mutate(date = as.Date(date, format = "%y/%m/%d"))

#Amend date format to inc. days of the week.
df_3 <- df_final %>% mutate(day = weekdays(as.Date(date,format = '%y-%m-%d')))

# Filter to remove Sundays from dataset.
df4 <- df_3 %>% filter(day != "Sunday") 

# Stacked barplot with multiple groups
ggplot(data=df_2, aes(x=date, y=n, fill=source)) + 
geom_bar(stat = "identity", width = 10)

# Use position=position_dodge()
ggplot(data=df_2, aes(x=date, y=n, fill=source)) +
  geom_bar(stat="identity", width = 10, position=position_dodge())

# Create a histogram
p7 <- ggplot(df_final, aes(x = source, fill = source)) +
geom_histogram(stat = "count") + coord_flip()

# Create a histogram with a color gradient
p7 <- ggplot(df_final, aes(x = source, fill = ..count..)) +
geom_histogram(stat = "count") + coord_flip()

#How to add frequency count labels to the bars in a bar graph using ggplot2?
plot1 <- ggplot(data=df4, aes(x=source, fill = source)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  coord_flip()

# Remove a column
df4 <- df4 %>% select(-day)

# Remove unwanted character strings
df3 <- lapply(df3, gsub, pattern= "02540000", replacement= "")
df3 <- as.data.frame(df3)

# Change col from char to integer values.
df4$coder <- as.integer(df4$coder)

# Mutate column according to data in another dataframe
df4 <- df2 %>% mutate(text = df3$text)

