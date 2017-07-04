install.packages("kernlab")
install.packages('readr')

#### Upload and clean data
library(kernlab)
library(readr)

# Upload data as object
dadosLinear <- read_delim("~/Documents/Training-Set-MDM/dadosLinear.csv", 
                          ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

# Data to test
dadosLinear <- dadosLinear[1:1000,]

# Object dimension
#dim(dadosLinear)
# Verify names
#names(dadosLinear)
# Verify data types by collumn
#sapply(dadosLinear[1,], class)
# Last elements of the table
#tail(dadosLinear)

# Drop columns
dadosLinear <- dadosLinear[,c("X3", "X5", "X8", "X9", "X11")]
# Rename columns
dadosLinear <- setNames(dadosLinear, c("date", "title", "vehicle", "category", "body"))
# Verify rows with na value
row_has_na <- apply(dadosLinear, 1, function(x){any(is.na(x))})
# Number of rows with na
#sum(row_has_na)
# Get data frame without na rows
dadosLinear <- dadosLinear[!row_has_na,]
row_has_na <- NULL
# Verify NULL elements
null_vector <- !grepl("NULL", dadosLinear$category, ignore.case = TRUE)
#length(null_vector)
# Select data frame without null element in category row
dadosLinear<- dadosLinear[null_vector,]
null_vector <- NULL
# Count and remove duplicated rows
#sum(duplicated(dadosLinear))
dadosLinear <- dadosLinear[!duplicated(dadosLinear),]
# Remove quotes
dadosLinear$title <- gsub("\"", "", dadosLinear$title)
dadosLinear$body <- gsub("\"", "", dadosLinear$body)

# Verify whether exists \n caracter in data
#install.packages("stringr")
#library(stringr)
#sum(grepl("\n", dadosLinear$title))
#sum(grepl("\n", dadosLinear$body))
#dadosLinear[grepl("[()]", dadosLinear$title),]
# Remove words between parentheses in title
dadosLinear$title <- gsub("\\s*\\([^\\]+\\)", "", dadosLinear$title)
# Verify words between parenthes in body
#grep("[()]", dadosLinear$body)
# Remove parentheses from body
dadosLinear$body <- gsub("[(|)]", "", dadosLinear$body)
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
# https://www.codementor.io/jadianes/data-science-python-r-sentiment-classification-machine-learning-du107otfg

# Load SentiLex-PT base as data frame
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
# names
colA.1 <- colA
colA <- gsub("\\.\\S+", "", colA)
# type
colA.1 <- gsub("\\S+\\=", "", colA.1)
# polarity
colC <- gsub("\\S+\\=", "", colC)
# Create data frame
dadosSentiWords <- data.frame(name=colA, type=colA.1, pol=colC)
colA.1 <- NULL; colA <- NULL; colB <- NULL; colC <- NULL; colD <- NULL
# Remove NA row
row_has_na <- apply(dadosSentiWords, 1, function(x){any(is.na(x))})
dadosSentiWords <- dadosSentiWords[!row_has_na,]
row_has_na <- NULL

set.seed(1234)

### Shuffle data
dadosLinear_matrix <- create_matrix(dadosLinear, language="portuguese", removeNumbers=TRUE,
                                    stemWords=TRUE, removeSparseTerms=.998, toLower = TRUE, removeStopwords = TRUE)


palavras_freq = sort(rowSums(dadosLinear_matrix), decreasing = TRUE)