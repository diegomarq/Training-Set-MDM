install.packages("kernlab")
library(kernlab)
install.packages('readr')
library(readr)

# Upload data as object
dadosLinear <- read_delim("~/Documents/Training-Set-MDM/dadosLinear.csv", 
                          ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

# Data to test
dadosLinear <- dadosLinear[1:1000,]

# Object dimension
dim(dadosLinear)

# Verify names
names(dadosLinear)

# Verify data types by collumn
sapply(dadosLinear[1,], class)

# Last elements of the table
tail(dadosLinear)

# Drop columns
dadosLinear <- dadosLinear[,c("X3", "X5", "X8", "X9", "X11")]

# Rename columns
dadosLinear <- setNames(dadosLinear, c("date", "title", "vehicle", "category", "body"))

# Verify rows with na value
row_has_na <- apply(dadosLinear, 1, function(x){any(is.na(x))})

# Number of rows with na
sum(row_has_na)

# Get data frame without na rows
dadosLinear <- dadosLinear[!row_has_na,]

# Verify NULL elements
null_vector <- !grepl("NULL", dadosLinear$category, ignore.case = TRUE)
length(null_vector)

# Select data frame without null element in category row
dadosLinear<- dadosLinear[null_vector,]

# Count and remove duplicated rows
sum(duplicated(dadosLinear))
dadosLinear <- dadosLinear[!duplicated(dadosLinear),]

# Remove quotes
dadosLinear$title <- gsub("\"", "", dadosLinear$title)
dadosLinear$body <- gsub("\"", "", dadosLinear$body)

# Verify whether exists \n caracter in data
install.packages("stringr")
library(stringr)

sum(grepl("\n", dadosLinear$title))
sum(grepl("\n", dadosLinear$body))

dadosLinear[grepl("[()]", dadosLinear$title),]
# Remove words between parentheses in title
dadosLinear$title <- gsub("\\s*\\([^\\]+\\)", "", dadosLinear$title)

# Verify words between parenthes in body
grep("[()]", dadosLinear$body)

# Remove parentheses from body
dadosLinear$body <- gsub("[(|)]", "", dadosLinear$body)

dadosS <- dadosLinear

# Get last word
# dadosS$vehicle <- str_extract(dadosS$vehicle, '\\w+$')
# rg_dadosT <- tail(strsplit(paste(dadosT$vehicle, ""), '-')[[1]],1)

####
# Update R version
# insert link in sources.list at etc/apt
# deb https://cran.rstudio.com/bin/linux/ubuntu xenial/
# sudo apt-get remove r-base-core
# sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB
# sudo add-apt-repository ppa:marutter/rdev
# sudo add-apt update
# sudo apt-get install r-base
# sudo apt-get install r-base-dev

### Start test of sentiment analysis

# Install dependencies
install.packages("tm")
install.packages("RTextTools")
library("RTextTools")
install.packages("caret")
library("caret")

set.seed(101)

### Shuffle data

dadosLinear_matrix <- create_matrix(dadosLinear, language="portuguese", removeNumbers=TRUE,
                             stemWords=TRUE, removeSparseTerms=.998, toLower = TRUE, removeStopwords = TRUE)

### Test word cloud
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

palavras_freq = sort(rowSums(dadosLinear_matrix), decreasing = TRUE)
