#' remove special characters from a text string
#' 
#' @param x the input string
#' @return string
#'
remove_spChars <- function(x){
  gsub("[^a-zA-Z0-9 ]", " ", x)
} 

#' function to expand contractions in an English-language source
#' 
#' @param doc the input string
#' @return input string with contractions substituted
fix_contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

#' function that accepts the lda model and num word to display
#' 
#' @param lda_model output from topicmodels::LDA
#' @param num_words limit output to n most frequent words
#' 
#' @return tibble with columns 'topic', 'term', 'beta', and 'row'
top_terms_per_topic <- function(lda_model, num_words) {
  
  #tidy LDA object to get word, topic, and probability (beta)
  topics_tidy <- tidy(lda_model, matrix = "beta")
  
  
  top_terms <- topics_tidy %>%
    group_by(topic) %>%
    arrange(topic, desc(beta)) %>%
    #get the top num_words PER topic
    slice(seq_len(num_words)) %>%
    arrange(topic, beta) %>%
    #row is required for the word_chart() function
    mutate(row = row_number()) %>%
    ungroup() %>%
    #add the word Topic to the topic labels
    mutate(topic = paste("Topic", topic, sep = " "))
  #create a title to pass to word_chart
  title <- paste("LDA Top Terms for", k, "Topics")
  return(top_terms)
}

#' function to perform the silhouette test on kmeans output
#' @param k number of clusters to test
#' @param d document term matrix
#' @return mean silhouette score
silhouette_test <- function(k, d){
  km <- kmeans(d, k)
  ss <- silhouette(km$cluster, dist(d))
  return(mean(ss[,3]))
}

#' return the frequency and number of departments for a term
#' @param dt data frame containing textual data
#' @param term character string of term to quantify
#' @return vector with the number of responses containing the term (x) and
#' the number of different departments providing those responses (y)
dept_importance <- function(dt, term){
  filt <- filter(dt, Category != "Efforts", grepl(term, text))
  x <- nrow(filt)
  y <- length(unique(filt$Department))
  return(c(x,y))
}
