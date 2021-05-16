################################################################################
# Text Analysis and Wordcloud Creation 
# By Anahita Sanandaji
# Copyright 2021 may not be shared or posted without permission. 
################################################################################

# Resource to Check: http://rstudio-pubs-static.s3.amazonaws.com/256376_0bc78a7b3f444eeea19d13cfa14a3942.html
#===============================================================================
# Install, Load Packages and Set working Directory is important
#===============================================================================
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")

library(tm)
library(SnowballC)
library(wordcloud)
# Please set your working directory now: you can also go to session -> Set working directory
getwd()
# or use setwd("?")

#===============================================================================
# Step 0: Read for .txt File from web or txt file
#===============================================================================

dataFromWeb <- readLines("http://cob.ohio.edu/anahitas/MIS4580/dataAnalysis.txt", warn= FALSE)
dataFromFile <- readLines("HarryPotter.txt", warn= FALSE)

#all.equal is just a function to test if the data in different locations is the same
all.equal(dataFromWeb, dataFromFile)

myData <- dataFromWeb
myData <- dataFromFile
myData[1:3]

#===============================================================================
# Step 1: Do some text pre-processing and data cleaning:
#===============================================================================

myCleanedData <- myData
myCleanedData[1]

#Remove HTML codes
myCleanedData <- gsub("<.*?>", "", myCleanedData)
myData[5]
myCleanedData[5]

#Remove all punctuation marks
myCleanedData <- gsub("[[:punct:]]","", myCleanedData)
myData[1:5]
myCleanedData[1:5]

#Remove some chars
myCleanedData <- gsub("â???","", myCleanedData)
myCleanedData <- gsub("T","", myCleanedData)

#Remove all extra white space characters
myCleanedData <- gsub("\\s+", " ", myCleanedData)
# NOTE: or use myCleanedData <- gsub("[[:space:]]"," ", myCleanedData)
myData[1]
myCleanedData[1]

#Convert all data to lowercase
myCleanedData <- tolower(myCleanedData)
myData[1]
myCleanedData[1]


#===============================================================================
# Step 2: Use a VectorSource function to prepare your data to be converted into a corpus:
#===============================================================================

#install.packages("tm") #may needed
#library(tm)

### Load the data as a corpus
### First VectorSource to prepare our data to be converted into a corpus, 
### then Vcorpus funtion will take the data and turn it into the required corpus
### data that is required to create TermDocumentMatrix
myCorpusData <- VCorpus(VectorSource((myCleanedData)))
myCorpusData[[1]]$content

class(myCleanedData)
class(myCorpusData)

#===============================================================================
#Step 3: Use tm_map function to clean and prepare the text:
#===============================================================================
### Use the tm_map function to prepare and clean the text up: 
#   - Here we will remove "stopwords" and "stem" the text:
#       - stopwords are typically words that have little meaning, beware that there can be different types words considered stopwards depending on how u want to clean your data
#       - stemming a word means reducing it to its roots ( fishes, fishing, fisher, all become fish)


## Remove stop words:
myMainCleanData <- myCorpusData
myMainCleanData <- tm_map(myMainCleanData,removeWords, stopwords("english"))

myCorpusData[[1]]$content
myMainCleanData[[1]]$content

### stemming
##install.packages("SnowballC") #may needed
##library(SnowballC)

# example:
stemDocument(c("computational", "computer", "computers", "computation"))

stemDocument(c("logic", "logical", "logics"))
#stem the data then check it out
myMainCleanData <- tm_map(myMainCleanData,stemDocument)
myCorpusData[[1]]$content
myMainCleanData[[1]]$content


#===============================================================================
#Step 1 and 2 and 3 combined :Note Alternate to Step 1: 
# You can also run the below tm commands directly instead of Step1 commands but 
# if you have already run the step 1 this is not necessary
#===============================================================================
##myCleanedData <- dataFromFile
##Remove HTML codes
##myCleanedData <- gsub("<.*?>", "", myCleanedData)

##myCorpusData <- VCorpus(VectorSource((myCleanedData))) #Load the data as a corpus
##myMainCleanData <- myCorpusData
##myMainCleanData <- tm_map(myMainCleanData,content_transformer(tolower))
##myMainCleanData <- tm_map(myMainCleanData,removePunctuation)
##myMainCleanData <- tm_map(myMainCleanData,stripWhitespace)
##myMainCleanData <- tm_map(myMainCleanData,removeWords, stopwords("English"))
##myMainCleanData <- tm_map(myMainCleanData,stemDocument)

## end of *** Step 1,2 and 3 Combined
#===============================================================================


#===============================================================================
#Step 4: Create "term document matrix", use TermDocumentMatrix:
#===============================================================================
## next, we need to create "term document matrix",
## we will use TermDocumentMatrix, a function that will take the Corpus
## and create a matrix that will contain frequency counts for all the words in the corpus).
## Note, the doc will refer to the line number that you see when you inspect(corpusData)
## this is what will be used to let us begin to use basically data mining techniques on unstructured data 

myTDMData <- TermDocumentMatrix(myMainCleanData)

# check it out
myTDMData
myTDMData[1:5]$content
inspect(myTDMData)
inspect(myTDMData[,1:5])
inspect(t(myTDMData[,1:10]))
#===============================================================================
#Step 5: Do Actual Text Analysis:
#===============================================================================

## To find terms that occur at least 4 times ( the lower frequency bound),
# then we can use the findFreqTerms().
findFreqTerms(myTDMData,lowfreq = 40, highfreq = Inf)
findFreqTerms(myTDMData,lowfreq = 40)
findFreqTerms(myTDMData, 4, Inf) #same as above
findFreqTerms(myTDMData, 5, Inf)
findFreqTerms(myTDMData, 50)
findFreqTerms(myTDMData, 100, Inf)
findFreqTerms(myTDMData, 1000, Inf)


## we can use the FindAssocs function to find words which associate together
# EX: if we want to fin words associated with "wizrd" based on a given 
# correlation of 50%
findAssocs(x = myTDMData, term = "wizard",corlimit = 0.5)
 
#another way to say the same thing
findAssocs(myTDMData, term = "wizard",corlimit = 0.5)

# another example
findAssocs(x = myTDMData, corlimit = 0.4, term ="harri")


##--- Visualization------------------------------------------------------------
# now we can try to do some basic visualization of the data, 

# You may need to install.packages("wordcloud")
# library(wordcloud) 

### calculate the frequency of words:
rowSums(as.matrix(myTDMData))

#### calculate the frequency of words and sort
freqOfTerms <- sort(rowSums(as.matrix(myTDMData)),decreasing = TRUE)
head(freqOfTerms)

###1) a bar plot of words
freqOfTerms[1:10]
barplot(freqOfTerms[1:10],col ="lightblue",las = 2)


# df<-data.frame(namesFromData[1:10], freqOfTerms[1:10])
# ggplot(data = df, aes(x = namesFromData.1.10., y= freqOfTerms.1.10.)) +
#   geom_bar(stat="identity") +
#   ggtitle("c): Anahita Sanandaji Bar Plot")

### 2) Create Wordcloud!

# First: get list of words from heading 
namesFromData <- names(freqOfTerms) 

#show the importance of words based on frequency with a word cloud
wordcloud(namesFromData, freqOfTerms, min.freq = 10)
wordcloud(namesFromData, freqOfTerms, min.freq = 20)
wordcloud(namesFromData, freqOfTerms, min.freq = 2, max.words = 6)

wordcloud(namesFromData, freqOfTerms, min.freq = 5, max.words = 200,
          colors = brewer.pal(8, "Dark2")) #by A.Sanandaji




