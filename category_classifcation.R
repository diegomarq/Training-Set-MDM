############################ 

# Upload and clean data
get_data <- function() {
  require(kernlab)
  require(readr)
  data <- read_delim("~/Documents/Training-Set-MDM/Noticias-2016.csv", 
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
data_politic <- filter_by_category(data, 'POLÍTICA')
pol_rows <- !grepl("OBSERVADOR DA POLÍTICA", data_politic$category, ignore.case = TRUE)
# Remove data frame with null element in category row
data_politic<- data_politic[pol_rows,]
pol_rows <- NULL

#ECONOMIA
data_economy <- filter_by_category(data, 'ECONOMÍA')
ec_rows <- !grepl("ECONOMÍA MERCADOS", data_economy$category, ignore.case = TRUE)
# Remove data frame with null element in category row
data_economy <- data_economy[ec_rows,]
ec_rows <- NULL


#CULTURA
data_sport <- filter_by_category(data, 'ESPORTES')
sp_rows <- !grepl("* ESPORTES", data_sport$category, ignore.case = TRUE)
# Remove data frame with null element in category row
data_sport <- data_sport[sp_rows,]
sp_rows <- NULL

data_pol_ec_esp <- rbind(data_politic, data_economy)
data_pol_ec_esp <- rbind(data_pol_ec_esp, data_sport)

write.csv(data_pol_ec_esp, file=paste('~/Documents/Training-Set-MDM/Noticias-2016-pol-ec-esp.csv'),
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
#h2o.init(nthreads = -1, #Number of threads -1 means use all cores on your machine
#         max_mem_size = "8G")  #max mem size is the maximum memory to allocate to H2O

h2o.init(nthreads=-1, max_mem_size="8G")
h2o.removeAll() ## clean slate - just in case the cluster was already running

loan_csv <- "/home/diego/Documents/Training-Set-MDM/Noticias-2016-pol-ec-esp.csv"
df <- h2o.importFile(path = normalizePath(loan_csv))
#data_h2o <- h2o.importFile(loan_csv)  # 163,987 rows x 15 columns
dim(data)

df$category <- as.factor(df$category)
df$news <- as.factor(df$news)

df_model <- df[,c(7,9)]

splits <- h2o.splitFrame(
  df_model,           ##  splitting the H2O frame we read above
  c(0.6,0.2),   ##  create splits of 60% and 20%; 
  ##  H2O will create one more split of 1-(sum of these parameters)
  ##  so we will get 0.6 / 0.2 / 1 - (0.6+0.2) = 0.6/0.2/0.2
  seed=1234) ##  setting a seed will ensure reproducible results (not R's seed)

train <- h2o.assign(splits[[1]], "train.hex")   
## assign the first result the R variable train
## and the H2O name train.hex
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex") ## R test, H2O test.hex

y <- "category"
x <- "news"

## run our first predictive model
rf1 <- h2o.randomForest(
  training_frame = train,
  validation_frame = valid,
  x="category",
  y="news",
  model_id = "rf_covType_v1",
  ntrees = 1,
  stopping_rounds = 2,
  score_each_iteration = T,
  seed = 10000)

glm_fit1 = h2o.glm(training_frame = train,
             validation_frame = valid,
             x = x,
             y = y,
             family='multinomial',
             solver='L_BFGS')

dl_fit3 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "dl_fit3",
                            validation_frame = valid,  #in DL, early stopping is on by default
                            epochs = 20,
                            hidden = c(10,10),
                            score_interval = 1,           #used for early stopping
                            stopping_rounds = 3,          #used for early stopping
                            stopping_metric = "AUC",      #used for early stopping
                            stopping_tolerance = 0.0005,  #used for early stopping
                            seed = 1,
                            distribution = "multinomial")

dl_perf3 <- h2o.performance(model = dl_fit3,
                            newdata = test)

h2o.auc(dl_perf3)

plot(dl_fit3, 
     timestep = "epochs", 
     metric = "AUC")