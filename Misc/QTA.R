#Quanteda analysis
library(quanteda)
library(quanteda.corpora)
library(tm)
data <- data.frame(text=df4$text, stringsAsFactors=FALSE)

#assign unique id
data$doc_id <- paste(dfnew6$id, dfnew6$source, dfnew6$Month_Yr, dfnew6$country, dfnew6$period, sep =  "_")

#CLean text
df4$text <- gsub("Classification  Language:.*","",df4$text)

#Join title headline with main text
data$text <- paste(dfnew6$title, dfnew6$text, sep = ".")

#Convert to corpus
corp2 <- corpus(data)

#Or load pre-prepared files
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET(main)/Quanteda analysis")
load(data, file = "pre_corpus_source_agg.rda")
load("pre_corpus_source_agg.rda")
load("agg_corpus.rda")


#Clean corpus and create dfm
dfm <- dfm(corpus, remove = stopwords("english"),
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
wordcloud_dfm_trim <- dfm_trim(dfm, min_termfreq = 1000)
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

#Basic barplot to visualise sentiment according to country (mean difference)
ggplot(dfnew6, aes(x=Month_Yr, y=sentiment_difference, color = country)) + 
  stat_summary(aes(y = sentiment_difference,group=1), fun=mean, colour="red", geom="line",group=1)
geom_point()

#Basic barplot to visualise sentiment according to country
ggplot(dfnew6, aes(x=Month_Yr, y=sentiment_difference, color = country)) + 
  geom_point()

# I need to do sentiment analysis for two corpora seperately initially, one on
# tabloids, and one on quality newspapers. Then compare the sentiment by 
#rbinding / row_bind the seperate dataframes, assigning tabloid and 
#quality labels then creating ggplot to explore any relations.

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


 


