############################ 
# Upload and clean data

library(kernlab)
library(readr)

# Upload data as object
gross_data <- read_delim("~/Documents/Training-Set-MDM/noticias_2010.csv", 
                          ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

#dim(gross_data) 17131

# Prepare and limit the data to the candidate searched
getData <- function(data) {
  # Drop columns
  data <- data[,c("X2", "X3", "X4", "X5", "X6", "X7", "X9", "X10")]
  
  # Rename columns
  data <- setNames(data, c("date", "title", "vehicle", "uf", "nacional", "category", "qtd_client", "news"))
  
  # Verify rows with na value
  row_has_na <- apply(data, 1, function(x){any(is.na(x))})
  
  # Get data frame without na rows
  data <- data[!row_has_na,]
  row_has_na <- NULL
  
  # Verify NULL elements
  null_vector <- !grepl("NULL", data$category, ignore.case = TRUE)
  
  # Select data frame without null element in category row
  data<- data[null_vector,]
  null_vector <- NULL
  
  # Count and remove duplicated rows
  data <- data[!duplicated(data),]
  
  # Remove quotes
  data$title <- gsub("\"", "", data$title)
  data$news <- gsub("\"", "", data$news)
  
  # Remove html tags
  data$title <- gsub("<.*?>", "", data$title)
  data$news <- gsub("<.*?>", "", data$news)
  
  # Remove UTC from date column
  data$date <- gsub("UTC", "", data$date)
  
  return(data)
}

# Separe news that appears the candidate
filter_by_candidate <- function(data, candidate) {
  
  candidate_news <- grepl(candidate, data$news, ignore.case = TRUE)  
  data <- data[candidate_news,]
  
  return(data)
}

# Filter data by the category wanted
filter_by_category <- function(data, category) {
  
  category_rows <- grepl(category, data$category, ignore.case = TRUE)  
  data <- data[category_rows,]
  
  return(data)
}

# Prepare data
data <- getData(gross_data)
# Filter
data <- filter_by_candidate(data, 'lula')

# Preparing dictonary as data frame
# Load SentiLex-PT base

############################

sentiWords <- readLines("~/Documents/Training-Set-MDM/SentiLex-lem-PT02.txt")
sentiWords <- strsplit(sentiWords, "\\s*\\;")

get_data_senti_df <- function(document) {
  colA <- c()
  colB <- c()
  colC <- c()
  colD <- c()
  for(i in 1:length(sentiWords)) {
    colA <- c(colA, sentiWords[[i]][1])
    colB <- c(colB, sentiWords[[i]][2])
    colC <- c(colC, sentiWords[[i]][3])
    colD <- c(colD, sentiWords[[i]][4])
  }
  colA.1 <- colA
  colA <- gsub("\\.\\S+", "", colA) # names
  colA.1 <- gsub("\\S+\\=", "", colA.1) # type
  colC <- gsub("\\S+\\=", "", colC) # polarity
  
  data_senti <- data.frame(name=colA, type=colA.1, pol=colC)
  
  # Remove NA row
  row_has_na <- apply(data_senti, 1, function(x){any(is.na(x))})
  data_senti <- data_senti[!row_has_na,]

  return(data_senti)
}

# Get only positive words from dictionary
get_pos_words_data_senti <- function(document) {
  colA <- c()
  colC <- c()
  for(i in 1:length(sentiWords)) {
    colA <- c(colA, sentiWords[[i]][1])
    colC <- c(colC, sentiWords[[i]][3])
  }
  colA <- gsub("\\.\\S+", "", colA) # names
  colC <- gsub("\\S+\\=", "", colC) # polarity
  
  n <- colA[!is.na(colA)]
  p <- colC[!is.na(colC)]
  
  pos.words <- c()
  for(i in 1:length(p)) { 
    if(p[i] == "1") {
      pos.words <- c(pos.words, n[i])
    }
  }
  
  return(pos.words)
}

# Get only negative words from dictionary
get_neg_words_data_senti <- function(document) {
  colA <- c()
  colC <- c()
  for(i in 1:length(sentiWords)) {
    colA <- c(colA, sentiWords[[i]][1])
    colC <- c(colC, sentiWords[[i]][3])
  }
  colA <- gsub("\\.\\S+", "", colA) # names
  colC <- gsub("\\S+\\=", "", colC) # polarity
  
  n <- colA[!is.na(colA)]
  p <- colC[!is.na(colC)]
  
  neg.words <- c()
  for(i in 1:length(p)) { 
    if(p[i] == "-1") {
      neg.words <- c(neg.words, n[i])
    }
  }
  
  return(neg.words)
}

# Get data frame with words avaluated as positvie/neutral/negative
data_senti <- get_data_senti_df(sentiWords)

# Get only positive words
pos.words <- get_pos_words_data_senti(sentiWords)

# Get only negative words
neg.words <- get_neg_words_data_senti(sentiWords)

############################ 
# LEXICAL ANALYZER
# http://analyzecore.com/2014/05/11/twitter-sentiment-analysis-based-on-affective-lexicons-in-r/
# https://stackoverflow.com/questions/32395098/r-sentiment-analysis-with-phrases-in-dictionaries

# If it has any problem:
# apt-get install r-cran-rjava
# install.packages("RWeka")

library("RWeka")

# Get sentiment score of sentences making ngrams with min and max interval
lex_sentiment <- function(sentences, pos_words, neg_words, .progress='none')
{
  require(plyr)
  require(stringr)
  require(tm)
  require(RWeka)
  
  sentences <- prepare_doc(sentences)
  
  scores <- laply(sentences, function(sentence, pos_words, neg_words){
    # Create bigram structure by using NGramTokenizer
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1,max=1))
    
    # word.list <- str_split(sentence, '\\s+')
    ngram_list = BigramTokenizer(sentence)
    words <- unlist(ngram_list)
    
    pos_matches <- match(words, pos_words)
    neg_matches <- match(words, neg_words)
    pos_matches <- !is.na(pos_matches)
    neg_matches <- !is.na(neg_matches)
    
    score <- sum(pos_matches) - sum(neg_matches)
    return(score)
    
  }, pos_words, neg_words, .progress=.progress)
  
  scores_df <- data.frame(text=sentences, score=scores)
  return(scores_df)
}

# News evaluation
#data_$news <- as.factor(data_$news)
#scores <- score.sentiment(data_$news, pos.words, neg.words, .progress='text')

# save evaluation
#write.csv(scores, file=paste('~/Documents/Training-Set-MDM/_scores.csv'), row.names=TRUE)

# Calculate opinion
#stat <- scores
#stat$created <- data_$date
#stat$created <- as.Date(stat$created)
#stat <- mutate(stat, news=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))

library('dplyr')

#by.news <- group_by(stat, news, created)
#by.news <- summarise(by.news, number=n())
#write.csv(by.news, file=paste('~/Documents/Training-Set-MDM/_opin.csv'), row.names=TRUE)

library('ggplot2')

# Plot graph
#ggplot(by.news, aes(created, number)) + geom_line(aes(group=news, color=news), size=2) +
#  geom_point(aes(group=news, color=news), size=4) +
#  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
  #stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +
#  ggtitle("Avaliacao de noticias em 2008")

############################ SENTIMENT ANALYSIS WITH DOC2VEC
# http://analyzecore.com/2017/02/08/twitter-sentiment-analysis-doc2vec/

library(tidyverse)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)

##### Vectorization #####
# ...
# Temp data set
#data_ <- data_to_test

# Remove netural value from data_senti
neutral_senti <- grepl("0", data_senti$pol)
data_senti<- data_senti[!neutral_senti,]
neutral_senti <- NULL

data_senti_01 <- data_senti %>%
  # replacing class values
  # mutate(pol = ifelse(pol == 0, 1/2, ifelse(pol == -1, 0, 1)))
  mutate(pol = ifelse(pol == -1, 0, 1))

# data splitting on train and test
# set.seed(1234)

prep_fun <- tolower
tok_fun <- word_tokenizer
# Train
# preprocessing and tokenization
data_senti_01$name <- as.vector(data_senti_01$name)
it_data_senti <- itoken(data_senti_01$name,
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    #ids = df_tweets$id,
                    progressbar = TRUE)
# Test
data_$news <- as.vector(data_$news)
it_data_test <- itoken(data_$news, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  #ids = tweets_test$id,
                  progressbar = TRUE)

# creating vocabulary and document-term matrix
vocab <- create_vocabulary(it_data_senti)
vectorizer <- vocab_vectorizer(vocab)

# train with senti words and test with data news
dtm_train <- create_dtm(it_data_senti, vectorizer)
dtm_test <- create_dtm(it_data_test, vectorizer)

# define tf-idf model
tfidf <- TfIdf$new()

# fit the model to the train data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
dtm_test_tfidf <- fit_transform(dtm_test, tfidf)

# train the model
t1 <- Sys.time()
glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf,
                               y = data_senti_01[['pol']], 
                               family = 'binomial', 
                               # L1 penalty
                               alpha = 1,
                               # interested in the area under ROC curve
                               type.measure = "auc",
                               # 5-fold cross-validation
                               nfolds = 5,
                               # high value is less accurate, but has faster training
                               thresh = 1e-3,
                               # again lower number of iterations for faster training
                               maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'mins'))

plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

################################### 
# SENTIMENT ANALYSIS WITH OTHER MOLDELS
# http://rpubs.com/lgendrot/sentiment

library("dplyr")
library("tm")

# Prepare document as Tokenization, but returns data frame
prepare_doc <- function(documents){
  # Lowercase all words for convenience
  doc <- tolower(documents)
  
  # Remove words with more than 3 numbers in them (they overwhelm the corpus, and are uninformative)
  doc <- gsub("[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", "", doc)
  
  # Remove all punctuation
  doc <- gsub("[[:punct:]]", "", doc)
  
  # Remove all newline characters
  doc <- gsub("[\r\n]", "", doc)
  
  # Regex pattern for removing stop words
  stop_pattern <- paste0("\\b(", paste0(stopwords("pt"), collapse="|"), ")\\b")
  doc <- gsub(stop_pattern, "", doc)
  
  # Replace whitespace longer than 1 space with a single space
  doc <- gsub(" {2,}", " ", doc)
  
  return(doc)
}

# Tokenization
tokenize <- function(documents){
  
  # Prepare document
  doc <- prepare_doc(documents)
  
  # Split on spaces and return list of character vectors
  doc_words <- strsplit(doc, " ")
  return(doc_words)
}

# Build the corpus
corpus_freq <- function(tokens, corpus_size=NULL, word_list = NULL){
  # Concatenate all tokenized words into a single character list
  all_words <- do.call(c, tokens)
  
  #If corpus size is not blank, and word list is, create a word frequency frame
  #take the top occuring words up to the length of corpus_size
  #and reorder alphabetically
  
  #This gives us an data frame of the most frequent words in our corpus, ordered alphabetically
  #sized by the corpus_size parameter
  if(is.null(word_list) & !is.null(corpus_size)){
    corpusfreq <- data.frame(table(all_words))
    names(corpusfreq) <- c("Word", "Freq")
    corpusfreq$Word <- as.character(corpusfreq$Word)
    corpusfreq$Freq <- as.numeric(corpusfreq$Freq)
    corpusfreq <- corpusfreq[order(-corpusfreq$Freq), ]
    corpusfreq <- corpusfreq[1:corpus_size, ]
    corpusfreq <- corpusfreq[order(corpusfreq$Word), ]
  } else {
    #Else it is assumed a pre-compiled word list has been passed into the function
    corpusfreq <- data.frame(word_list)
    names(corpusfreq) <- c("Word")  
  }
  
  # N docs is where we will store the document frequency (I.E how many documents a word appears in)
  # We'll need this to calculate TF-IDF
  corpusfreq$n_docs <- 0
  
  # For every vector of words in our tokenized list, count how many times each word in our corpus
  # occurs
  for(token_list in tokens){
    t <- data.frame(table(token_list))
    names(t) <- c("Word", "n_docs")
    t$n_docs <- 1
    t_freq <- merge(x=corpusfreq, y=t, by="Word", all.x=TRUE)
    t_freq$n_docs.y[is.na(t_freq$n_docs.y)] <- 0
    corpusfreq$n_docs <- corpusfreq$n_docs + t_freq$n_docs.y
  }
  return(corpusfreq)
}

# Term frequency - inverse term frequency
tfidf <- function(document, corpus){
  #Create a data frame out of a single document and its word frequency
  doc_f <- data.frame(unlist(table(document)))
  names(doc_f) <- c("Word", "Freq")
  
  #Get a data frame of the words in the corpus found in the current document
  in_doc <- intersect(doc_f$Word, corpus$Word)
  doc_f <- doc_f[doc_f$Word %in% in_doc, ]
  
  #Get a data frame of the words in the corpus not found in the current document
  #Set their frequency to 0
  not_in_doc <- data.frame(Word=setdiff(corpus$Word, document))
  not_in_doc$Freq <-0
  
  #Bind our two data frames, we now have frequencies for the words that are in our corpus, and 0s
  #everywhere else
  tf <- rbind(doc_f, not_in_doc)
  tf$Word <- as.character(tf$Word)
  tf$Freq <- as.numeric(tf$Freq)
  
  #Order alphabetically again so it remains compatible with our corpus data frame
  tf <- tf[order(tf$Word), ]
  
  #Calculate the tfidf
  #log1p is the same as log(1+___)
  log_freq <- log1p(tf$Freq)
  log_doc_freq <- log1p(nrow(corpus)/corpus$n_docs)
  tf$tfidf <- log_freq * log_doc_freq
  
  #Divide by zero errors get NA values, but should be 0s
  tf$tfidf[is.na(tf$tfidf)] <- 0
  return(tf)
}

#tmp_tfidf <- tfidf(tmp_news[[1]], corpus_tmp)

# This function takes a token_list (the output of tokenize) and either a corpus size to create a new
# corpus, or a pre-compiled corpus
get_feature_vectors <- function(tokens_list, corpus_size=1500, corpus=NULL){
  if(is.null(corpus)){
    corpus <- corpus_freq(tokens_list, corpus_size=corpus_size)
  }
  
  #Our feature matrix starts out as an all 0 matrix with N by C dimensions
  feature_matrix <- matrix(0, length(tokens_list), nrow(corpus))
  
  #For every document in our tokenized list, calculate the tfidf feature vector, and put it into our
  # feature matrix row-wise
  for(i in 1:length(tokens_list)){
    feature_vector <- tfidf(tokens_list[[i]], corpus)$tfidf
    feature_matrix[i, 1:nrow(corpus)] <- feature_vector
  }
  
  #The column names are the same as the alphabetical list of words in our corpus
  #Unnecessary step, but useful for examining the resulting feature matrix
  colnames(feature_matrix) <- corpus$Word
  return(data.frame(feature_matrix))
}

##### Helper functions

#Calculates accuracy, true negative, true positive, and positive predictive value of a confusion matrix.
sensitivity <- function(confusion_matrix){
  acc <- (confusion_matrix[1]+confusion_matrix[4])/sum(confusion_matrix)
  tn <- (confusion_matrix[1]) / (confusion_matrix[3]+confusion_matrix[1])
  ppv <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[3])
  tp <- (confusion_matrix[4]) / (confusion_matrix[4]+confusion_matrix[2])
  return(list(accuracy=acc, specificity=tn, precision=ppv, sensitivity=tp))
}

#add_targets takes our feature matrix, and the original data frame (with the documents in the
#same order) and adds the dependent variable for model training. In this case it's our pre-labeled
#sentiment.
add_targets <- function(feature_matrix, df){
  feature_matrix$sentiment <- df$sentiment
  return(feature_matrix)
}

#The ensemble function takes a list of prediction vectors, each with a length equal to the
# number of documents, and takes a majority vote.
ensemble <- function(predictions){
  votes <- matrix(0, length(predictions), length(predictions[[1]]))
  for(i in 1:length(predictions)){
    votes[i,] <- ifelse(predictions[[i]] == "positive",1,0)
  }
  vote_decision <- colSums(votes)/nrow(votes)
  vote_decision <- ifelse(vote_decision >= .5,"P", "N")
  
  return(vote_decision)
}

##### Aplicate model

# Remove neutral sentiment from data_senti
#neutral_senti <- grepl("0", data_senti$pol)
#data_senti<- data_senti[!neutral_senti,]
#neutral_senti <- NULL



#h2o.shutdown()

#library("dplyr")

# Divide data
#train <- sample_frac(data, .70)
#test <- setdiff(data, train)
#test <- sample_frac(test, 1)

#dim(train) 4696
#dim(valid) 2012
#dim(test) 2012

# Make sentiment analysis by a lexical approach with ngrams
# Return data frame with score and sentiment polarity
# Make lexical analysis with data to train and then use it to train the model
#df_sentiment <- lex_sentiment(train$news, pos.words, neg.words, .progress='text')
#df_sentiment$sentiment <- 0
#df_sentiment <- df_sentiment %>% mutate(sentiment = ifelse(
#  df_sentiment$score > 0, 1, ifelse(df_sentiment$score < 0, -1, 0)))

# Include sentiment and scores columns into data frame
#train$score <- df_sentiment$score
#train$sentiment <- df_sentiment$sentiment

## The data$sentiment with score 0 means that its polarity it is unknown

# Include setiment and score column in test and valid data
#valid$score <- 0
#test$score <- 0

#valid$sentiment <- 0
#test$sentiment <- 0

# Remove neutral sentiment from data_senti
#neutral_senti <- grepl(0, data$sentiment)
#data <- data[!neutral_senti,]
#neutral_senti <- NULL

#Tokenize
#train_tokens <- tokenize(train$news)
#valid_tokens <- tokenize(valid$news)
#test_tokens <- tokenize(test$news)

#Get corpus, and calculate feature vectors
#train_features <- get_feature_vectors(train_tokens, corpus_size=2700)
#valid_features <- get_feature_vectors(valid_tokens, corpus_size=1100)
#test_features <- get_feature_vectors(test_tokens, corpus_size=1100)

#Add the dependent variable for model fitting, I.E. the pre-labeled sentiment
#my_features <- add_targets(my_features, data)

#train_features$sentiment <- as.factor(train$sentiment)
#valid_features$sentiment <- as.factor(valid$sentiment)
#test_features$sentiment <- as.factor(test$sentiment)

# Divide data
#train <- sample_frac(my_features, .70)
#valid <- setdiff(my_features, train)
#test <- sample_frac(valid, 1)

##### FORMULAS

#Formula for each model
#form <- as.formula(paste("sentiment~", paste(setdiff(names(test), c("sentiment")), collapse="+")))

#library("nnet")
#library("neuralnet")

# Single hidden-layer neural network of size 10
#m_nnet <- nnet(form, data=train, size=10, MaxNWts=100000)
#Naive Bayes algorithm with laplace smoothing
#m_nbayes <- naiveBayes(form, data=train, laplace=1000, threshold=.5)
#Random forest
#m_randomforest <- ranger(dependent.variable.name="sentiment", data=train, write.forest=TRUE)
#logistic regressions
#m_logit <- glm(form, data=train, family=binomial(link='logit'))
#Support vector machine
#m_svm <- svm(form, data=train, type="C")

##### Aplicate

#pred_nnet <- predict(m_nnet, test, type="class")

#ens <- ensemble(list(pred_nnet))
#table(test$sentiment, ens)
#sensitivity(table(test$sentiment, ens))

#library(ggplot2)
#ggplot(data=filter(test,sentiment), aes(x=date, y=sentiment)) +
#  geom_line() + 
#  geom_point() +
#  xlab("Date") +
#  ylab("Sentiment (afinn)") +
#  ggtitle("Teste")
