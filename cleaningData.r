install.packages("kernlab")
install.packages('readr')

#### Upload and clean data
library(kernlab)
library(readr)

# Upload data as object
data <- read_delim("~/Documents/Training-Set-MDM/noticias_2008.csv", 
                          ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

# Data to test
data_to_test <- data[1:1000,]

# Object dimension
#dim(dadosLinear)
# Verify names
#names(dadosLinear)
# Verify data types by collumn
#sapply(dadosLinear[1,], class)
# Last elements of the table
#tail(dadosLinear)

# Drop columns
data_to_test <- data_to_test[,c("X2", "X3", "X4", "X5", "X6", "X7", "X9", "X10")]
# Rename columns
data_to_test <- setNames(data_to_test, c("date", "title", "vehicle", "uf", "nacional", "category", "qtd_client", "news"))
# Verify rows with na value
row_has_na <- apply(data_to_test, 1, function(x){any(is.na(x))})
# Number of rows with na
#sum(row_has_na)
# Get data frame without na rows
data_to_test <- data_to_test[!row_has_na,]
row_has_na <- NULL
# Verify NULL elements
null_vector <- !grepl("NULL", data_to_test$category, ignore.case = TRUE)
#length(null_vector)
# Select data frame without null element in category row
data_to_test<- data_to_test[null_vector,]
null_vector <- NULL
# Count and remove duplicated rows
#sum(duplicated(dadosLinear))
data_to_test <- data_to_test[!duplicated(data_to_test),]
# Remove quotes
data_to_test$title <- gsub("\"", "", data_to_test$title)
data_to_test$news <- gsub("\"", "", data_to_test$news)

# Remove html tags
data_to_test$title <- gsub("<.*?>", "", data_to_test$title)
data_to_test$news <- gsub("<.*?>", "", data_to_test$news)

# Remove UTC from date column
data_to_test$date <- gsub("UTC", "", data_to_test$date)

# Separe news that appears Lula
data_to_test.news <- grepl("Lula", data_to_test$news, ignore.case = TRUE)

# Verify whether exists \n caracter in data
#install.packages("stringr")
#library(stringr)
#sum(grepl("\n", dadosLinear$title))
#sum(grepl("\n", dadosLinear$body))
#dadosLinear[grepl("[()]", dadosLinear$title),]
# Remove words between parentheses in title
data_to_test$title <- gsub("\\s*\\([^\\]+\\)", "", data_to_test$title)
# Verify words between parenthes in body
#grep("[()]", dadosLinear$body)
# Remove parentheses from body
data_to_test$news <- gsub("[(|)]", "", data_to_test$news)
#dadosS <- dadosLinear

# Get last word
# dadosS$vehicle <- str_extract(dadosS$vehicle, '\\w+$')
# rg_dadosT <- tail(strsplit(paste(dadosT$vehicle, ""), '-')[[1]],1)

####
# Update R version
# insert link in sources.list at etc/apt
# deb https://cran.rstudio.com/bin/linux/ubuntu xenial/
# sudo apt-get remove r-base-core
# sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
# sudo add-apt-repository ppa:marutter/rdev
# sudo apt-apt update
# sudo apt-get install r-base
# sudo apt-get install r-base-dev


######## Test word cloud
install.packages("wordcloud")
library(tm)
library(wordcloud)
library(RColorBrewer)

# Create corpus
dadosCorpus <- Corpus(VectorSource(dadosLinear$body))
# Remove whitespace
dadosCorpus <- tm_map(dadosCorpus, stripWhitespace)
# Converto letter for lowercase
dadosCorpus <- tm_map(dadosCorpus, tolower)
# Remove punctuation
dadosCorpus <- tm_map(dadosCorpus, removePunctuation)
# Remove stopwords
dadosCorpus <- tm_map(dadosCorpus, removeWords, stopwords("portuguese"))
# Remove numbers
dadosCorpus <- tm_map(dadosCorpus, removeNumbers)
# stem Document
dadosCorpus <- tm_map(dadosCorpus, stemDocument)
  
### Do stem?
  
# Term document matrix
dadosTM <- DocumentTermMatrix(dadosCorpus)
# Sparse terms
dadosSparse <- removeSparseTerms(dadosTM, 0.99)

# Convert to data frame
dadosDF <- as.data.frame(as.matrix(dadosSparse))
colnames(dadosDF) <- make.names(colnames(dadosDF))

# Split into train and test
dadosDFTrain <- head(dadosDF, nrow(dadosLinear[700,])) # 70%
dadosDFTest <- head(dadosDF, nrow(dadosLinear[300,])) # 30%


# Add to 

# Frequency
#dadosWordFreq <- sort(rowSums(dadosMatrix), decreasing = TRUE)
# Define limit by frequency
#dadosLimit <- quantile(dadosWordFreq, probs = 0.25)
# Limit words
#dadosTopFreq <- dadosWordFreq[dadosWordFreq > dadosLimit]
# Create Data Frame with words and its frequency
#dadosDataFrame <- data.frame(word = names(dadosTopFreq), freq = dadosTopFreq)
# plot wordcloud
#wordcloud(dadosDataFrame$word, dadosDataFrame$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))


### Sentiment analysis

# Install dependencies
install.packages("tm")
install.packages("RTextTools")
library("RTextTools")
#install.packages("caret")
#library("caret")
#install.packages("tidytext")
#library(tidytext)

# Using Bag of words, from:
#(not) https://www.codementor.io/jadianes/data-science-python-r-sentiment-classification-machine-learning-du107otfg
# https://stackoverflow.com/questions/32395098/r-sentiment-analysis-with-phrases-in-dictionaries?rq=1

# Preparing dictonary as data frame
# Load SentiLex-PT base

sentiWords <- readLines("~/Documents/Training-Set-MDM/SentiLex-lem-PT02.txt")
sentiWords <- strsplit(sentiWords, "\\s*\\;")
# Create columns from substrings
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

n <- colA[!is.na(colA)]
p <- colC[!is.na(colC)]

pos.words <- c()
for(i in 1:length(p)) { 
  if(p[i] == "1") {
    pos.words <- c(pos.words, n[i])
  }
}

neg.words <- c()
for(i in 1:length(p)) { 
  if(p[i] == "-1") {
    neg.words <- c(neg.words, n[i])
  }
}
n <- NULL
p <- NULL
i <- NULL

# Create data frame
data_senti <- data.frame(name=colA, type=colA.1, pol=colC)
colA.1 <- NULL; colA <- NULL; colB <- NULL; colC <- NULL; colD <- NULL
# Remove NA row
row_has_na <- apply(data_senti, 1, function(x){any(is.na(x))})
data_senti <- data_senti[!row_has_na,]
row_has_na <- NULL

# Make score function to compare matches from database and SentiLex-PT

# test
vDadosLinearTit <- as.vector(dadosLinear$title)

#score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
#{
#  require(plyr)
#  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
#  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
#    sentence = gsub('[[:punct:]]', '', sentence)
#    sentence = gsub('[[:cntrl:]]', '', sentence)
#    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
#    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
#    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
#    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
#    pos.matches = match(words, pos.words)
#    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
#    pos.matches = !is.na(pos.matches)
#    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
#    score = sum(pos.matches) - sum(neg.matches)
    
#    return(score)
#  }, pos.words, neg.words, .progress=.progress )
  
#  scores.df = data.frame(score=scores, text=sentences)
#  return(scores.df)
#}

#analysis <- score.sentiment(vDadosLinearTit, pos.words, neg.words)
#table(analysis$text)

#set.seed(1234)

### Shuffle data
#dadosLinear_matrix <- create_matrix(dadosLinear, language="portuguese", removeNumbers=TRUE,
#                                    stemWords=TRUE, removeSparseTerms=.998, toLower = TRUE, removeStopwords = TRUE)


#palavras_freq = sort(rowSums(dadosLinear_matrix), decreasing = TRUE)

####################################

install.packages("tidyverse")
install.packages("tidytext")

library("tidyverse")
library("tidytext")

####################################
# Temp data set
data_ <- data_to_test

# Cleanig the courpus
cleanCorpus <- function(corpus){
  
  corpus.tmp <- tm_map(corpus, content_transformer(tolower))
  corpus.tmp <- tm_map(corpus.tmp, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  corpus.tmp <- tm_map(corpus.tmp, removeWords,stopwords("portuguese"))
  corpus.tmp <- tm_map(corpus.tmp, stemDocument)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  
  return(corpus.tmp)
}

corpus_ <- Corpus(VectorSource(data_$news), readerControl =  list(reader=readPlain))
cln.corpus_ <- cleanCorpus(corpus_)

############################ LEXICAL ANALYZER
# http://analyzecore.com/2014/05/11/twitter-sentiment-analysis-based-on-affective-lexicons-in-r/




score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}
# News evaluation
data_$news <- as.factor(data_$news)
scores <- score.sentiment(data_$news, pos.words, neg.words, .progress='text')

# save evaluation
write.csv(scores, file=paste('~/Documents/Training-Set-MDM/_scores.csv'), row.names=TRUE)

# Calculate opinion
stat <- scores
stat$created <- data_$date
stat$created <- as.Date(stat$created)
stat <- mutate(stat, news=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))

library('dplyr')

by.news <- group_by(stat, news, created)
by.news <- summarise(by.news, number=n())
write.csv(by.news, file=paste('~/Documents/Training-Set-MDM/_opin.csv'), row.names=TRUE)

library('ggplot2')

# Plot graph
ggplot(by.news, aes(created, number)) + geom_line(aes(group=news, color=news), size=2) +
  geom_point(aes(group=news, color=news), size=4) +
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
  #stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +
  ggtitle("Avaliacao de noticias em 2008")

############################ SENTIMENT ANALYSIS WITH DOC2VEC
# http://analyzecore.com/2017/02/08/twitter-sentiment-analysis-doc2vec/

library(tidyverse)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)

##### Vectorization #####
# ...

data_senti_01 <- data_senti  %>%
  # replacing class values
  # mutate(pol = ifelse(pol == 0, 1/2, ifelse(pol == -1, 0, 1)))
  mutate(pol = ifelse(pol == 0, 0, 1))

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
