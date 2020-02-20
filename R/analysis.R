## ANALYSIS
library(cleanNLP)
library(dplyr)
library(plotly)
library(tidytext)
library(tidyr)
library(tm)
library(topicmodels)

source(file = 'R/functions.R')
dat <- readRDS(file = 'data/data.rds')

# some words will come up frequently that are not helpful
unhelpful_words <- c("defenders",
                     "deij",
                     "diversity",
                     "wildlife")

# TOKENIZE DATA
# single words
dat_tokenized <- dat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% unhelpful_words) %>%
  filter(nchar(word) > 3)

# lets see the most frequently used words
dat_filtered%>%
  group_by(Category)%>%
  count(word, sort = TRUE)%>%
  top_n(10)%>%
  mutate(word = reorder(word, n))%>%
  plot_ly(type = 'bar', x = ~word, color = ~Category, y = ~n)%>%
  layout(barmode = 'stack')

# Bigrams
# pull out 2-word phrases from all statements
bigrams <- dat %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# separate word pairs so we can filter out unwanted words
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filter stop words from each half and pairs with the same word
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  filter(!word1 %in% unhelpful_words, !word2 %in% unhelpful_words)%>%
  filter(word1 != word2)

# rejoin bigrams and tally by department
bigram_dept <- bigrams_filtered %>%
  # recombine the bigrams
  unite(bigram, word1, word2, sep = " ") %>%
  # filter out current efforts
  filter(Category != 'Efforts')%>%
  # group by bigram and department then add count and sort
  count(Department, bigram, sort = TRUE) %>%
  # count ungroups, so re-group by department (optional)
  #group_by(Department) %>%
  slice(seq_len(10)) %>%
  #ungroup() %>%
  arrange(n) %>%
  mutate(row = row_number())

# Bigram LDA?
bigram_dtm <- bigrams_filtered%>%
  unite(bigram, word1, word2, sep = " ")%>%
  mutate(Statement = as.character(Statement))%>%
  count(Statement, bigram, sort = TRUE)%>%
  ungroup()%>%
  cast_dtm(document = Statement, term = bigram, value = n)

inspect(bigram_dtm[1:4, 1:8])

# test a series of k's for likelihood minima
ks <- sapply(seq(2,10,1), function(k){LDA(bigram_dtm, k = k, method = "GIBBS", control = list(seed = seed))@loglikelihood})

plot(seq(2,10,1), ks)

k <- 10

lda <- LDA(dt_mat, k = k, method = "GIBBS", control = list(seed = seed))

bigram_betas <- tidy(lda, matrix = 'beta')

topic_bigrams <- top_terms_per_topic(lda, 10)

subplot(lapply(unique(topic_bigrams$topic), function(x){
  plot_ly(data = filter(topic_bigrams, topic == x), type = 'bar', y = ~term, x = ~beta, orientation = 'h')
}),
nrows = 4)
# LDA Analysis

# create document-term matrix
dt_mat <- dat_tokenized%>%
  filter(Category != 'Efforts')%>%
  mutate(Statement = as.character(Statement))%>%
  count(Statement, word, sort = TRUE)%>%
  ungroup()%>%
  cast_dtm(document = Statement, term = word, value = n)

inspect(dt_mat[1:4, 1:8])

seed <- 1024
# run the LDA analysis

# test a series of k's for likelihood minima
ks <- sapply(seq(2,10,1), function(k){LDA(dt_mat, k = k, method = "GIBBS", control = list(seed = seed))@loglikelihood})

plot(seq(2,10,1), ks)

k <- 9

lda <- LDA(dt_mat, k = k, method = "GIBBS", control = list(seed = seed))

# return the probability per term of inclusion in each topic
betas <- tidy(lda, matrix = 'beta')

# get the top terms per topic
topic_terms <- top_terms_per_topic(lda, 10)

subplot(lapply(unique(topic_terms$topic), function(x){
  plot_ly(data = filter(topic_terms, topic == x), type = 'bar', y = ~term, x = ~beta, orientation = 'h')
}),
nrows = 3)

# try using cleanNLP tools to analyse stems, etc.
cnlp_init_udpipe()
anno <- dat%>%
  filter(Category != 'Efforts')%>%
  cnlp_annotate(verbose=FALSE)

# create document term matrix on word stems
dt_mat <- anno$token%>%
  filter(nchar(token)>3, upos == 'VERB' | upos == 'NOUN')%>%
  count(doc_id, lemma, sort = TRUE)%>%
  ungroup()%>%
  cast_dtm(document = doc_id, term = lemma, value = n)

# run lda analysis
lda <- LDA(dt_mat, k = 9, method = "GIBBS", control = list(seed = seed))

# test a series of k's for likelihood minima
ks <- sapply(seq(2,10,1), function(k){LDA(dt_mat, k = k, method = "GIBBS", control = list(seed = seed))@loglikelihood})

# return the probability per term of inclusion in each topic
betas <- tidy(lda, matrix = 'beta')

# get the top terms per topic
topic_terms <- top_terms_per_topic(lda, 10)

subplot(lapply(unique(topic_terms$topic), function(x){
  plot_ly(data = filter(topic_terms, topic == x), type = 'bar', y = ~term, x = ~beta, orientation = 'h')
}),
nrows = 3)
