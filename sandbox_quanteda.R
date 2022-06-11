require(quanteda)
require(quanteda.textmodels)
require(quanteda.textstats)
require(quanteda.textplots)
require(readtext)
require(devtools)
require(quanteda.corpora)
require(newsmap)
require(seededlda)


# Setup parallel processing
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

# The default threads used by quanteda is 2. Increase this to use all but one thread on the local processor to improve performance but leave some space for other processes.
quanteda_options("threads"=7)
library(tm)
library(SnowballC)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(stringi)
library(RWeka)

set.seed(123)

profane <- readLines("https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en")

# The data file to analyze
devTrainFile<-"Data\\final\\en_US\\prod-training.txt"
conFile <- file(devTrainFile, "r") 


# Create where we are going to store the exploratory data analysis info
# ogram <- data.frame(word = NULL,freq=NULL)
# bgram <- data.frame(word = NULL,freq=NULL)
# tgram <- data.frame(word = NULL,freq=NULL)
countsLine <- data.frame(WordChars = NULL, Words  = NULL, WordLen = NULL)
# Loop through the data file and process in chunks. Necessary
# to manage memory use for very large data sets
# Number of lines to process in each chunk
dataChunk <- 4000
DFM_onegram <- NULL
DFM_2gram <- NULL
DFM_3gram <- NULL
loop = TRUE
while (loop) {
    # read lines from the file until there is nothing left and break
    text = readLines(conFile, n = dataChunk)
    if ( length(text) == 0 ) {
        break
    }
    # convert the chunk of lines into a corpus
    docs <- quanteda::corpus(text)
    # #print(head(docs))
    # print(head(text))
    # #print(str(text))
    # print(summary(docs))

    
    # Convert the document into a list of tokens removing the stuff we don't
    # want that can be done within the function
    toks <- quanteda::tokens(docs, remove_punct=TRUE, remove_symbols = TRUE, 
                   remove_numbers = TRUE, remove_url = TRUE, 
                   remove_separators = TRUE, split_hyphens = TRUE, 
                   tolower=TRUE)
    # print(toks)
    toksClean <- quanteda::tokens_remove(toks, profane)
    
    # Create the 1, 2, 3-grams
    toks_2grams <- tokens_ngrams(toksClean, n=2)
    toks_3grams <- tokens_ngrams(toksClean, n=3)
    # print(head(toks_2grams))
    # print(head(toks_3grams))
    # Create or add to the DFM for the onegrams
    if( is.null(DFM_onegram)) {
        DFM_onegram <- dfm(toksClean)
        # print(DFM_onegram)
        # print(ndoc(DFM_onegram))
        # print(nfeat(DFM_onegram))
        # print(topfeatures(DFM_onegram, 10))        
    } else {
        print("Append the new DFM to the old one")
        DFM_onegram <- rbind(DFM_onegram, dfm(toksClean))
        # print(DFM_onegram)
        # print(ndoc(DFM_onegram))
        # print(nfeat(DFM_onegram))
        # print(topfeatures(DFM_onegram, 10))  
    }
    # Create or add to the DFM for the two-grams
    if( is.null(DFM_2gram)) {
        DFM_2gram <- dfm(toks_2grams)
        # print(DFM_2gram)
        # print(ndoc(DFM_2gram))
        # print(nfeat(DFM_2gram))
        # print(topfeatures(DFM_2gram, 10))        
    } else {
        print("Append the new DFM to the old one")
        DFM_2gram <- rbind(DFM_2gram, dfm(toks_2grams))
        # print(DFM_2gram)
        # print(ndoc(DFM_2gram))
        # print(nfeat(DFM_2gram))
        # print(topfeatures(DFM_2gram, 10))  
    }
    # Create or add to the DFM for the three-grams
    if( is.null(DFM_3gram)) {
        DFM_3gram <- dfm(toks_3grams)
        # print(DFM_3gram)
        # print(ndoc(DFM_3gram))
        # print(nfeat(DFM_3gram))
        # print(topfeatures(DFM_3gram, 10))        
    } else {
        print("Append the new DFM to the old one")
        DFM_3gram <- rbind(DFM_3gram, dfm(toks_3grams))
        # print(DFM_3gram)
        # print(ndoc(DFM_3gram))
        # print(nfeat(DFM_3gram))
        # print(topfeatures(DFM_3gram, 10))  
    }

    #loop=FALSE  
    

    
}
print("Onegram Totals")
totals_onegram <- colSums(DFM_onegram)
print(head(totals_onegram, 25))
print("Onegram Totals")
totals_2gram <- colSums(DFM_2gram)
print(head(totals_2gram, 25))
print("Onegram Totals")
totals_3gram <- colSums(DFM_3gram)
print(head(totals_3gram, 25))


close(conFile)
## Close parallel processing
stopCluster(cluster)
registerDoSEQ()