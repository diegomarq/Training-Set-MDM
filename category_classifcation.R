############################ 

# Upload and clean data
get_data <- function() {
  require(kernlab)
  require(readr)
  data <- read_delim("~/Documents/Training-Set-MDM/noticias_2010.csv", 
                         ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
  return(data)
}

gross_data <- get_data()
#dim(gross_data) 17131

# Prepare and limit the data to the candidate searched
clean_data <- function(data) {
  # Drop columns
  data <- data[,c("X2", "X3", "X4", "X5", "X6", "X7", "X9", "X10")]
  
  # Rename columns
  data <- setNames(data, c("date", "title", "vehicle", "uf", "nacional", "category", "qtd_client", "news"))
  
  # Verify rows with na value
  row_has_na <- apply(data, 1, function(x){any(is.na(x))})
  
  # Get data frame without na rows
  data <- data[!row_has_na,]
  
  # Verify NULL elements
  null_vector <- !grepl("NULL", data$category, ignore.case = TRUE)
  
  # Remove data frame with null element in category row
  data<- data[null_vector,]
  
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

# Clean $title, $category and $news
data <- clean_data(gross_data)

# Prepare document as Tokenization, but returns data frame
prepare_doc <- function(documents){
  
  require("dplyr")
  require("tm")
  
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

# Prepara data$news
data$news <- prepare_doc(data$news)
data$title <- prepare_doc(data$title)

# Filter data by the category wanted
filter_by_category <- function(data, category) {
  
  category_rows <- grepl(category, data$category, ignore.case = TRUE)  
  data <- data[category_rows,]
  
  return(data)
}

# POLITICA
data_op <- filter_by_category(data, 'OPINIÃO')
pol_rows <- !grepl("ELEIÇÕES E POLÍTICA", data_politic$category, ignore.case = TRUE)

# Remove data frame with null element in category row
data_politic<- data_politic[pol_rows,]
pol_rows <- NULL

data_op_pol <- rbind(data_politic, data_op)

write.csv(data_op_pol, file=paste('~/Documents/Training-Set-MDM/noticias_op_pol.csv'),
          row.names=TRUE)
#dim(data_politic) 4114

##########################

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
  
  require("foreach")
  #require("doParallel")
  
  if(is.null(corpus)){
    corpus <- corpus_freq(tokens_list, corpus_size=corpus_size)
  }
  
  #Our feature matrix starts out as an all 0 matrix with N by C dimensions
  feature_matrix <- matrix(0, length(tokens_list), nrow(corpus))
  
  #For every document in our tokenized list, calculate the tfidf feature vector, and put it into our
  # feature matrix row-wise
  foreach(i = 1:length(tokens_list), .combine = rbind) %dopar% {
    feature_vector <- tfidf(tokens_list[[i]], corpus)$tfidf
    feature_matrix[i, 1:nrow(corpus)] <- feature_vector
  }
  #for(i in 1:length(tokens_list)){
  #  feature_vector <- tfidf(tokens_list[[i]], corpus)$tfidf
  #  feature_matrix[i, 1:nrow(corpus)] <- feature_vector
  #}
  
  #The column names are the same as the alphabetical list of words in our corpus
  #Unnecessary step, but useful for examining the resulting feature matrix
  colnames(feature_matrix) <- corpus$Word
  return(data.frame(feature_matrix))
}

# Tokenize data_politic$news
#data_politic_tmp <- data_politic
data_pol_tokens <- tokenize(data_politic$news)

# Get vector of sizes from tokens_list
vector_of_size <- function(tokens_list) {
  size <- c()
  for(i in 1:length(tokens_list)) {
    size <- c(size, length(tokens_list[[i]]))
  }
  return(size)
}

# Get median
tokens_size <- vector_of_size(data_pol_tokens)
size <- median(tokens_size) # 356


#Get corpus, and calculate feature vectors from data_politic$news as data_pol_tokens
data_pol_features <- get_feature_vectors(data_pol_tokens, corpus_size=356)

##### Helper functions

#Calculates accuracy, true negative, true positive, and positive predictive value of a confusion matrix.
sensitivity <- function(confusion_matrix){
  acc <- (confusion_matrix[1]+confusion_matrix[4])/sum(confusion_matrix)
  tn <- (confusion_matrix[1]) / (confusion_matrix[3]+confusion_matrix[1])
  ppv <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[3])
  tp <- (confusion_matrix[4]) / (confusion_matrix[4]+confusion_matrix[2])
  return(list(accuracy=acc, specificity=tn, precision=ppv, sensitivity=tp))
}

ensemble <- function(predictions){
  votes <- matrix(0, length(predictions), length(predictions[[1]]))
  for(i in 1:length(predictions)){
    votes[i,] <- ifelse(predictions[[i]] == "POLÍTICA",1,0)
  }
  vote_decision <- colSums(votes)/nrow(votes)
  vote_decision <- ifelse(vote_decision >= .5,1, 0)
  
  return(votes)
}

numeric_category <- function(document) {
  votes <- matrix(0, length(document), length(document[[1]]))
  for(i in 1:length(document)){
    votes[i,] <- ifelse(document[[i]] == "POLÍTICA",1,0)
  }
  vote_decision <- colSums(votes)/nrow(votes)
  vote_decision <- ifelse(vote_decision >= .5,1, 0)
  return(votes)
}

##### Aplicate model

data_pol_features$category <- data_politic_tmp$category
data_pol_features$category <- as.factor(data_politic_tmp$category)
write.csv(data_pol_features, file=paste('~/Documents/Training-Set-MDM/data_pol_features.csv'), row.names=TRUE)
#test_features$sentiment <- as.factor(test$sentiment)

# Divide data
train <- sample_frac(data_pol_features, .70)
valid <- setdiff(data_pol_features, train)
test <- sample_frac(valid, 1)

##### FORMULAS

#Formula for each model
form <- as.formula(paste("category~", paste(setdiff(names(test), c("category")), collapse="+")))

library("nnet")
library("neuralnet")

library("class")
library("e1071")

# Single hidden-layer neural network of size 10
m_nnet <- nnet(form, data=train, size=10, MaxNWts=100000)
#Naive Bayes algorithm with laplace smoothing
m_nbayes <- naiveBayes(form, data=train, laplace=1000, threshold=.5)
#Random forest
m_randomforest <- ranger(dependent.variable.name="sentiment", data=train, write.forest=TRUE)
#logistic regressions
m_logit <- glm(form, data=train, family=binomial(link='logit'))
#Support vector machine
m_svm <- svm(form, data=train, type="C", probability = TRUE)

##### Aplicate

#pred_nnet <- predict(m_nnet, test, type="class")
pred_svm <- predict(m_svm, test, probability = TRUE)

#library(pROC)
ens <- ensemble(list(pred_svm))
ens_test_category <- numeric_category(list(test$category))
sensitivity(table(as.vector(ens_test_category), as.vector(ens)))
library(caret)
confusionMatrix(test$category, pred_svm)

roc_obj <- roc(as.vector(ens_test_category), as.vector(ens))
auc(roc_obj)

library(ggplot2)
ggplot(data=filter(test,sentiment), aes(x=date, y=sentiment)) +
  geom_line() + 
  geom_point() +
  xlab("Date") +
  ylab("Sentiment (afinn)") +
  ggtitle("Teste")

############ h2o
# https://github.com/h2oai/h2o-tutorials/blob/master/tutorials/glm/glm_h2oworld_demo.Rmd
library("h2o")
h2o.init(nthreads = -1, #Number of threads -1 means use all cores on your machine
         max_mem_size = "8G")  #max mem size is the maximum memory to allocate to H2O

loan_csv <- "/home/diego/Documents/Training-Set-MDM/noticias_op_pol.csv"
data_h2o <- h2o.importFile(loan_csv)  # 163,987 rows x 15 columns
dim(data)

data_h2o$category <- as.factor(data_h2o$category)

h2o.levels(data_h2o$category)
#"ELEIÇÕES E POLÍTICA" "POLÍTICA"

splits <- h2o.splitFrame(data = data_h2o, 
                         ratios = c(0.7, 0.15),  #partition data into 70%, 15%, 15% chunks
                         seed = 1)  #setting a seed will guarantee reproducibility
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

nrow(train)  # 700
nrow(valid) # 150
nrow(test)  # 151

y <- "category"
x <- setdiff(names(data), c(y, "int_rate"))  #remove the interest rate column because it's correlated with the outcome
print(x)

glm_fit1 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "dl_fit1",
                            seed = 1)

glm_perf1 <- h2o.performance(model = glm_fit1,
                             newdata = test)

h2o.auc(glm_perf1)
