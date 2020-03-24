## ANALYSIS
library(cleanNLP)
library(cluster)
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
                     "wildlife",
                     "diverse")

# TOKENIZE DATA
# single words
dat_tokenized <- dat %>%
  tidytext::unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% unhelpful_words) %>%
  filter(nchar(word) > 3)

# lets see the most frequently used words
dat_tokenized%>%
  filter(Category != 'Efforts')%>%
  count(word, sort = TRUE)%>%
  top_n(10)%>%
  mutate(word = reorder(word, n))%>%
  plot_ly(type = 'bar', y = ~word, x = ~n,
          orientation = 'h')%>%
  layout(barmode = 'stack')

# create document-term matrix
dt_mat <- dat_tokenized%>%
  filter(Category != 'Efforts')%>%
  mutate(Statement = as.character(Statement))%>%
  count(Statement, word, sort = TRUE)%>%
  ungroup()%>%
  tidytext::cast_dtm(document = Statement, term = word, value = n)

inspect(dt_mat[1:4, 1:8])

seed <- 1024
# run the LDA analysis

# test a series of k's for likelihood minima
# ks <- sapply(seq(2,20,1), function(k){LDA(dt_mat, k = k, method = "GIBBS", control = list(seed = seed))@loglikelihood})
# 
# plot(seq(2,20,1), ks)
# 
# k <- 9

# calculate average silhouette value for a series of ks
avg_sil <- sapply(2:20, function(x){silhouette_test(x, dt_mat)})
plot_ly(type = 'scatter', mode = 'lines', x = 2:20, y = avg_sil)%>%
  layout(yaxis = list(title = 'Silhouette score', range = c(0, 0.5)))

k <- 5

lda <- topicmodels::LDA(dt_mat, k = k, method = "GIBBS", control = list(seed = seed))

# return the probability per term of inclusion in each topic
betas <- tidy(lda, matrix = 'beta')

# get the top terms per topic
topic_terms <- top_terms_per_topic(lda, 7)

# plot the top terms per topic
subplot(lapply(unique(topic_terms$topic), function(x){
  plot_ly(data = filter(topic_terms, topic == x)%>%
            mutate(term = reorder(term, beta)),
          type = 'bar', y = ~term, x = ~beta,
          orientation = 'h',
          name = x)%>%
    layout(xaxis = list(range = c(0, max(betas$beta))),
           yaxis = list(tickfont = list(size = 8),
                        showticklabels = FALSE))%>%
    add_annotations(
      text = ~term,
      x = ~beta, xanchor = 'right',
      font = list(color = 'white'),
      showarrow = FALSE
    )
  }),
  nrows = (k%/%2)+1)%>%
  layout(title = paste('Terms (k = ', k, ')', sep = ""))

# Calculate TF-IDF
dat_tokenized%>%
  filter(Category != 'Efforts')%>%
  count(Statement, word)%>%
  ungroup()%>%
  bind_tf_idf(term = word, document = Statement, n = n)%>%
  ungroup()%>%
  arrange(desc(tf_idf))%>%
  top_n(10, tf_idf)%>%
  mutate(word = reorder(word, tf_idf))%>%
  plot_ly(type = 'bar', orientation = 'h', x = ~tf_idf, y = ~word)

# BIGRAMS
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
  filter(word1 != word2, Category != 'Efforts')

# rejoin bigrams and tally by department
bigram_counts <- bigrams_filtered %>%
  # recombine the bigrams
  unite(bigram, word1, word2, sep = " ") %>%
  # filter out current efforts
  filter(Category != 'Efforts')%>%
  # group by bigram and department then add count and sort
  count(bigram, sort = TRUE) %>%
  # count ungroups, so re-group by department (optional)
  #group_by(Department) %>%
  slice(seq_len(10)) %>%
  #ungroup() %>%
  arrange(n) %>%
  mutate(row = row_number(), bigram = reorder(bigram, n))

# lets see the most frequently used bigrams
bigram_counts%>%
  plot_ly(type = 'bar', y = ~bigram, x = ~n,
          orientation = 'h')%>%
  layout(barmode = 'stack')

# Bigram LDA
bigram_dtm <- bigrams_filtered%>%
  unite(bigram, word1, word2, sep = " ")%>%
  mutate(Statement = as.character(Statement))%>%
  count(Statement, bigram, sort = TRUE)%>%
  ungroup()%>%
  cast_dtm(document = Statement, term = bigram, value = n)

inspect(bigram_dtm[1:4, 1:8])

# test a series of k's for likelihood minima
seed <- 1024

# ks <- sapply(seq(2,20,1), function(k){LDA(bigram_dtm, k = k, method = "GIBBS", control = list(seed = seed))@loglikelihood})
# 
# plot(seq(2,20,1), ks)
# 
# k <- 12

# calculate average silhouette value for a series of ks
avg_sil <- sapply(2:20, function(x){silhouette_test(x, bigram_dtm)})
plot_ly(type = 'scatter', mode = 'lines', x = 2:20, y = avg_sil)%>%
  layout(yaxis = list(title = 'Silhouette score', range = c(0, 0.5)))
#plot(2:20, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
k <- 5

bigram_lda <- LDA(bigram_dtm, k = k, method = "GIBBS", control = list(seed = seed))

bigram_betas <- tidy(bigram_lda, matrix = 'beta')

bigram_topic_terms <- top_terms_per_topic(bigram_lda, 7)%>%
  group_by(topic)%>%arrange(sort(beta))

subplot(lapply(unique(bigram_topic_terms$topic), function(x){
  plot_ly(data = filter(bigram_topic_terms, topic == x)%>%
            mutate(term = reorder(term, beta)),
          type = 'bar', y = ~term, x = ~beta,
          orientation = 'h',
          name = x)%>%
    layout(xaxis = list(range = c(0, max(bigram_betas$beta))),
           yaxis = list(tickfont = list(size = 10)))
  }),nrows = (k%/%2) + (k%%2))%>%
  layout(title = paste('Bigrams (k = ', k, ')', sep = ""))

# STEM ANALYSIS.
# use cleanNLP tools to create stems
cnlp_init_udpipe()

anno <- dat%>%
  filter(Category != 'Efforts')%>%
  cnlp_annotate(verbose=FALSE)

# create document term matrix on word stems
anno_mat <- anno$token%>%
  filter(nchar(token)>3, !token %in% unhelpful_words)%>%#, upos == 'VERB' | upos == 'NOUN')%>%
  anti_join(stop_words, by = c('token' = 'word'))%>%
  count(doc_id, lemma, sort = TRUE)%>%
  ungroup()%>%
  cast_dtm(document = doc_id, term = lemma, value = n)

# test a series of k's for likelihood minima
avg_sil <- sapply(2:20, function(x){silhouette_test(x, anno_mat)})
plot_ly(type = 'scatter', mode = 'lines', x = 2:20, y = avg_sil)%>%
  layout(yaxis = list(title = 'Silhouette score', range = c(0, 0.5)))
k <- 7

# ks <- sapply(seq(2,20,1), function(k){LDA(anno_mat, k = k, method = "GIBBS", control = list(seed = seed))@loglikelihood})
# 
# plot(seq(2,20,1),ks)
# k <- 8

# run lda analysis
anno_lda <- LDA(anno_mat, k = k, method = "GIBBS", control = list(seed = seed))

# return the probability per term of inclusion in each topic
anno_betas <- tidy(anno_lda, matrix = 'beta')

# get the top terms per topic
anno_topic_terms <- top_terms_per_topic(anno_lda, 7)

# plot the top terms per topic
subplot(lapply(unique(anno_topic_terms$topic), function(x){
  plot_ly(data = filter(anno_topic_terms, topic == x)%>%
            mutate(term = reorder(term, beta)),
          type = 'bar', y = ~term, x = ~beta,
          orientation = 'h',
          name = x)%>%
    layout(xaxis = list(range = c(0, max(anno_betas$beta))),
           yaxis = list(tickfont = list(size = 8)))
}),nrows = (k%/%2)+1)%>%
  layout(title = paste('Stem (k = ', k, ')', sep = ""))


# K-MEANS CLUSTERING
# One word tokens

# calculate average silhouette value for a series of ks
avg_sil <- sapply(2:20, function(x){silhouette_test(x, dt_mat)})
plot(2:20, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
k <- 3
# cluster terms according to k
km <- kmeans(dt_mat, k)
# grab the top 10 words most strongly associated with each topic
kmeans_topics <- as.data.frame(t(km$centers))
colnames(kmeans_topics) <- paste("Topic", 1:k)
kmeans_topics$term <- rownames(kmeans_topics)
kmeans_topics <- gather(kmeans_topics, topic, score, -term)%>%
  group_by(topic)%>%
  arrange(topic, desc(score))%>%
  slice(seq_len(10))%>%
  arrange(topic, score)

subplot(lapply(unique(kmeans_topics$topic), function(x){
  plot_ly(data = filter(kmeans_topics, topic == x),
          y = ~term, x = ~score,
          type = 'bar', orientation = 'h')
  }), nrows = 3)%>%
  layout(title = 'Annotation (k = 8)')
