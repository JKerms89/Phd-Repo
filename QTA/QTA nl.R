# Load packages 
library(readxl)
library(dplyr)
library(quanteda)
library(quanteda.corpora)
library(tm)

# Load dataframe
RCA <- read_excel("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/RCA dataset/RCA network lists/MainRCAdf/RCA19.xlsx")
RCA <- RCA %>% filter(source_country == 'NETHERLANDS')

# Carry out QTA of newspaper text.
#Quanteda analysis
data <- data.frame(text=RCA$text, stringsAsFactors=FALSE)
#assign unique id
data$doc_id <- paste(RCA$`statement ID`, RCA$source, RCA$time, RCA$source_country, RCA$period, sep =  "_")
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
wordcloud_dfm_trim <- dfm_trim(dfm, min_termfreq = 200)
textplot_wordcloud(wordcloud_dfm_trim)

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
lda10 <- LDA(dtm, k = 10, method = "Gibbs",
             control = list(burnin = burnin, iter = iter, thin = thin,
                            seed = seed, nstart = nstart, best = best))

terms(lda10, 10)
library(tidytext)
library(dplyr)
library(ggplot2)

lda10_topics <- tidy(lda10, matrix = "beta")
lda10_topterms <- lda10_topics %>% group_by(topic) %>%
  top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)

lda10_topterms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") + coord_flip()








