library(tm)
library(readr)
library(tidytext)
library(ggplot2)
library(dplyr)
library(topicmodels)
library(SnowballC)
#Load Text
tweets <- read_csv("C:/R Files/data/trump_tweets3.csv")
tweets <- tweets[,2]

#Clean Text
docs <- Corpus(VectorSource(tweets))
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming (reduces words to their root form)
docs <- tm_map(docs, stemDocument)
# Remove additional stopwords
docs <- tm_map(docs, removeWords, c("@", "#", "!", "will"))

dtm <- DocumentTermMatrix(docs)

#Run Latent Dirichlet Allocation (LDA) using Gibbs Sampling
ldaOut <- LDA(dtm, method="gibbs", k = 5, control = list(seed = 1234))
terms(ldaOut,6)

#Extract the per-topic-per-word probabilities, called "beta", 
#from the model using tidytext package
tweet_topics <- tidy(ldaOut, matrix = "beta")

#Manipulate data to plot per topic per word probabilities
tweet_top_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

tweet_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic,scales = "free") +
  coord_flip()

#Another example using the associatedpress news articles dataset in topicmodels package
data("AssociatedPress")
AssociatedPress
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda
terms(ap_lda,6)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#per-document-per-topic probabilities, called "gamma"
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents