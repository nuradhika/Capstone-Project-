library(tidyverse)
library(magrittr)
library(stringr)
library(stringi)
library(tidytext)
library(doMC)
library(foreach)
library(tm)
library(RWeka)
library(ngram)

#read files 
news <-readLines("en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
blogs <-readLines("en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <-readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

#Get 1% of the data sample 
set.seed(400)

sample_data <- c(sample(news, length(news)*0.01),
                 sample(blogs, length(blogs)*0.01),
                 sample(twitter, length(twitter)*0.01))


#get 10% of the total sample. 
sample <- c(sample_data, length(sample_data)*0.1)

# make a corpus 
corpus <- VCorpus(VectorSource(sample))
#clean data
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, content_transformer(tolower))
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
#custom_stopwords <- c("is", "a", "for", "in", "the", "will", "can", "just", "like", "said")
#corpus <- tm_map(corpus, removeWords, custom_stopwords)


# Sort and get the frequency 
getFreq <- function(n) {
        freq <- sort(rowSums(as.matrix(n)), decreasing = TRUE)
        return(data.frame(word = names(freq), freq = freq))
}



unigram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
qgram <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
pgram <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))


#unigram
corpus_1gram <- TermDocumentMatrix(corpus, control = list(tokenize = unigram))
n_1gram <- getFreq(removeSparseTerms(corpus_1gram, 0.9999))
n_1gram <- n_1gram[, 1:2]
head(n_1gram,30)


#bigram
corpus_2gram <- TermDocumentMatrix(corpus, control = list(tokenize = bigram))
 n_2gram <- getFreq(removeSparseTerms(corpus_2gram, 0.9999))
 n_2gram <- n_2gram[, 1:2]

 #trigram 
corpus_3gram <- TermDocumentMatrix(corpus, control = list(tokenize = trigram))
 n_3gram <- getFreq(removeSparseTerms(corpus_3gram, 0.9999))
n_3gram <- n_3gram[, 1:2]
 
#qgram
 corpus_4gram <- TermDocumentMatrix(corpus, control = list(tokenize = qgram))
n_4gram <- getFreq(removeSparseTerms(corpus_4gram, 0.9999))
 n_4gram <- n_4gram[, 1:2]

 #pgram
corpus_5gram <- TermDocumentMatrix(corpus, control = list(tokenize = pgram))
n_5gram <- getFreq(removeSparseTerms(corpus_5gram, 0.9999))
 n_5gram <- n_5gram[, 1:2]

 #save ngram files as .rds files 
saveRDS(n_1gram, file = "./n_1gram.rds")
saveRDS(n_2gram, file = "./n_2gram.rds")
saveRDS(n_3gram, file = "./n_3gram.rds")
saveRDS(n_4gram, file = "./n_4gram.rds")
saveRDS(n_5gram, file = "./n_5gram.rds")

