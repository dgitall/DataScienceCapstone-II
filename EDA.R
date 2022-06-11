## Perform exploratory data analysis on the data sets
## Common NLP exploratory data analysis will use include
##      * Word count -- which words appear most frequently and what is the distribution
##      * Word length -- Distribution of word lengths
##      * Sentence Length -- Distribution of sentence lengths
##      * Ngram exploration -- Which 2- and 3-grams appear most frequently
##      * Foreign words -- Which words from another language appear and which languages

library(tm)
library(SnowballC)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(stringi)
library(RWeka)

# Setup parallel processing
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

set.seed(123)

# Create a function for cleaning up the text
# This and word counts from 
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
TextClean <- function(corpus) {
    # Create a special function for changing a character to a space
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    # Remove special characters
    corpus <- tm_map(corpus, toSpace, "/")
    corpus <- tm_map(corpus, toSpace, "@")
    corpus <- tm_map(corpus, toSpace, "\\|")    
    corpus <- tm_map(corpus, toSpace, '(\r\n|\r|\n)')
    corpus <- tm_map(corpus, toSpace, '\\n')
    corpus <- tm_map(corpus, toSpace, '\\r') 
    corpus <- tm_map(corpus, toSpace, "â") 
    corpus <- tm_map(corpus, toSpace, "@")
    corpus <- tm_map(corpus, toSpace, "<")
    corpus <- tm_map(corpus, toSpace, "~")
    corpus <- tm_map(corpus, toSpace, "#")
    corpus <- tm_map(corpus, toSpace, "Ÿ")
    corpus <- tm_map(corpus, toSpace, "ð")
    corpus <- tm_map(corpus, toSpace, "®")
    # corpus <- tm_map(corpus, toSpace, "\\\\")
    corpus <- tm_map(corpus, toSpace, "€")
    corpus <- tm_map(corpus, toSpace, "™")    
    # Convert the text to lower case
    corpus <- tm_map(corpus, content_transformer(tolower))
    # Remove numbers
    corpus <- tm_map(corpus, removeNumbers)
    # Remove english common stopwords
    # docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    # docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
    # Remove punctuations
    rPunct <- content_transformer(function (x ) removePunctuation(x, preserve_intra_word_contractions = TRUE, 
                                                                    preserve_intra_word_dashes = TRUE))
    corpus <- tm_map(corpus, rPunct)
    # Eliminate extra white spaces
    corpus <- tm_map(corpus, stripWhitespace)
    # Text stemming
    # docs <- tm_map(docs, stemDocument)
    corpus
}
# The data file to analyze
devTrainFile<-"Data\\final\\en_US\\dev-training.txt"
conFile <- file(devTrainFile, "r") 

# Loop through the data file and process in chunks. Necessary
# to manage memory use for very large data sets
ogram <- data.frame(word = NULL,freq=NULL)
bgram <- data.frame(word = NULL,freq=NULL)
tgram <- data.frame(word = NULL,freq=NULL)
countsLine <- data.frame(WordChars = NULL, Words  = NULL, WordLen = NULL)
while (TRUE) {
    text = readLines(conFile, n = 1000)
    if ( length(text) == 0 ) {
        break
    }

    docs <- Corpus(VectorSource(text))
    
    # Clean up the text
    docs <- TextClean(docs)
    
    # Word Count Exploration
    OnegramTokenizer <- function(x) NGramTokenizer(x, 
                                                   Weka_control(min=1, max=1))    
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- rowSums(m)
    tmp <- data.frame(word = names(v),freq=v)
    # Merge the count from the current block into the larger storage matrix
    # where there are duplicates, update the 'freq' count 
    if(length(ogram) == 0) {
        ogram <- tmp
    } else {
        ogram <- ogram %>%
            full_join(tmp, by = "word") %>%
            rowwise() %>%
            transmute(word, freq = sum(freq.x, freq.y, na.rm=TRUE))
    }
    
    # Gather the number of words in each line and other per line metrics
    counts <- data.frame(WordChars=stri_stats_latex(docs$content[1])[["CharsWord"]],
                             Words = stri_stats_latex(docs$content[1])[["Words"]],
                             WordLen = stri_stats_latex(docs$content[1])[["CharsWord"]]/
                                 stri_stats_latex(docs$content[1])[["Words"]])
    for(i in 2:length(docs)) {
        # nl<-stri_detect_regex('(\r\n|\r|\n)', docs$content[i])
        # if(nl) print(paste("newline found in", i, " at ", nl ))
        counts <- rbind(counts, data.frame(WordChars=stri_stats_latex(docs$content[i])[["CharsWord"]],
                                           Words = stri_stats_latex(docs$content[i])[["Words"]],
                                           WordLen = stri_stats_latex(docs$content[i])[["CharsWord"]]/
                                               stri_stats_latex(docs$content[i])[["Words"]] ))
    }    
    if(length(countsLine) == 0) {
        countsLine <- counts
    } else {
        countsLine <- rbind(countsLine, counts)
    }
    
    docs2 <- VCorpus(VectorSource(text))
    
    # Clean up the text
    docs2 <- TextClean(docs2)
    
    BigramTokenizer <- function(x) NGramTokenizer(x, 
                                                  Weka_control(min=2, max=2))
    TrigramTokenizer <- function(x) NGramTokenizer(x, 
                                                  Weka_control(min=3, max=3))
    # Using Tyler's method of making the 'Text' object here
    bgramTDM <- TermDocumentMatrix(docs2, 
                                   control = list(tokenize = BigramTokenizer))
    tgramTDM <- TermDocumentMatrix(docs2, 
                                   control = list(tokenize = TrigramTokenizer))
    m <- as.matrix(bgramTDM)
    v <- rowSums(m)
    tmp <- data.frame(word = names(v),freq=v)
    if(length(bgram) == 0) {
        bgram <- tmp
    } else {
        bgram <- bgram %>%
            full_join(tmp, by = "word") %>%
            rowwise() %>%
            transmute(word, freq = sum(freq.x, freq.y, na.rm=TRUE))
    }
    m <- as.matrix(tgramTDM)
    v <- rowSums(m)
    tmp <- data.frame(word = names(v),freq=v)
    if(length(tgram) == 0) {
        tgram <- tmp
    } else {
        tgram <- tgram %>%
            full_join(tmp, by = "word") %>%
            rowwise() %>%
            transmute(word, freq = sum(freq.x, freq.y, na.rm=TRUE))
    }
    
}
close(conFile)

wordsTotal <- sum(ogram$freq)
ogram <- ogram %>% arrange(desc(freq)) 
ogram <- ogram %>% mutate(fract = freq/wordsTotal)
cumfract = cumsum(ogram$fract)
ogram <- cbind(ogram, as.data.frame(cumfract))

# Get the list of words that make up half of the total occurrence of words
dhalf <- filter(ogram, cumfract <= 0.50)
# Plot a distribution of all words and of the most common
smD <- ogram[which(ogram$freq>500),]
smD$word <- factor(smD$word, levels = smD$word)
smDDist <- ggplot(smD, aes(y=freq, x=factor(word)))
smDDist <- smDDist + geom_point(stat = "identity")
MeanX <- mean(ogram$freq,na.rm=TRUE)
MedX <- median(ogram$freq, na.rm=TRUE)
Max <- max(ogram$freq,na.rm=TRUE)
MaxY <- round(which.max(density(ogram$fract,na.rm=TRUE)$y), 0)
MaxX <- round(density(ogram$fract,na.rm=TRUE)$x[MaxY], 0)
smDDist <- smDDist + geom_hline(yintercept=MeanX, color="red")
smDDist <- smDDist + geom_hline(yintercept=MedX, color="darkred")
# smDDist <- smDDist + geom_hline(yintercept=Max, color="blue")
smDDist <- smDDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=1,
                                label=paste("Avg. = ", signif(MeanX,4)),
                                color="red")
smDDist <- smDDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=3,
                                label=paste("Median = ", MedX),
                                color="darkred")
smDDist <- smDDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=5,
                            label=paste("Total Words ", wordsTotal),
                            color="blue")
smDDist <- smDDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=7,
                              label=paste("Maximum Occurances =", Max),
                              color="purple")
smDDist <- smDDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=9,
                              label=paste("The ", nrow(dhalf), " most frequent words are 50% of all words"),
                              color="black")
smDDist <- smDDist + xlab("Most Frequent Words") + ylab("Number of Occurances")
print(smDDist)

# halfDist <- ggplot(dhalf, aes(x=fract))
# halfDist <- halfDist + geom_histogram(binwidth = .001)
# halfDist <- halfDist + geom_vline(xintercept=MeanX, color="red")
# halfDist <- halfDist + geom_vline(xintercept=MedX, color="darkred")
# halfDist <- halfDist + geom_vline(xintercept=Max, color="blue")
# halfDist <- halfDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=1,
#                                 label=paste("Avg. = ", signif(MeanX,3)),
#                                 color="red")
# halfDist <- halfDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=3,
#                                 label=paste("Median = ", signif(MedX,3)),
#                                 halfDistcolor="darkred")
# halfDist <- halfDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=5,
#                                 label=paste("Most frequent ", MaxX),
#                                 color="blue")
# halfDist <- halfDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=7,
#                                 label=paste("Maximum ", round(Max, 4)),
#                                 color="purple")
# halfDist <- halfDist + xlab("Frequency of Word Appearance") + ylab("Count of Frequency")
# 
# q <- ggarrange(fullDist, halfDist, ncol=2)
# print(q)

#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(dhalf$word,dhalf$freq)

# Check the bigrams
bigramTotal <- sum(bgram$freq)
bgram <- bgram %>% arrange(desc(freq)) 
bgram<- bgram %>% mutate(fract = freq/wordsTotal)
cumfract = cumsum(bgram$fract)
bgram <- cbind(bgram, as.data.frame(cumfract))
# Plot a distribution of all bigrams
smBGram <- bgram[which(bgram$freq>200),]
smBGram$word <- factor(smBGram$word, levels = smBGram$word)
smBGramDist <- ggplot(smBGram, aes(y=freq, x=word))
smBGramDist <- smBGramDist + geom_point(stat = "identity")
# bgramDist <- ggplot(bgram, aes(x=fract))
# bgramDist <- bgramDist + geom_histogram(binwidth = .00001)
MeanX <- mean(bgram$freq,na.rm=TRUE)
MedX <- median(bgram$freq, na.rm=TRUE)
Max <- max(bgram$freq,na.rm=TRUE)
MaxY <- round(which.max(density(bgram$freq,na.rm=TRUE)$y), 0)
MaxX <- round(density(bgram$freq,na.rm=TRUE)$x[MaxY], 0)
smBGramDist <- smBGramDist + geom_hline(yintercept=MeanX, color="red")
smBGramDist <- smBGramDist + geom_hline(yintercept=MedX, color="darkred")
# smBGramDist <- smBGramDist + geom_hline(yintercept=Max, color="blue")
smBGramDist <- smBGramDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=1,
                                label=paste("Avg. = ", signif(MeanX,4)),
                                color="red")
smBGramDist <- smBGramDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=3,
                                label=paste("Median = ", signif(MedX,3)),
                                color="darkred")
smBGramDist <- smBGramDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=5,
                              label=paste("Total 2-Grams ", bigramTotal),
                              color="blue")
# smBGramDist <- smBGramDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=7,
#                               label=paste("Maximum Occurrences =", Max),
#                               color="purple")
smBGramDist <- smBGramDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=7,
                                label=paste("Maximum Occurrences ", round(Max, 4)),
                                color="purple")

# bgramDist <- bgramDist + xlim(0,0.0005)
smBGramDist <- smBGramDist + xlab("Frequency of 2-Gram Appearance") + ylab("Count of Frequency")
# print(smBGramDist)
#wordcloud
#limit words by specifying min frequency
cloudWord <- wordcloud(smBGram$word,smBGram$freq, min.freq=200)
# q <- ggarrange(smBGramDist, cloudWord, ncol=2)
print(q)

# Check the 3-grams
trigramTotal <- sum(tgram$freq)
tgram <- tgram %>% arrange(desc(freq)) %>% mutate(fract = freq/wordsTotal)
cumfract = cumsum(tgram$fract)
tgram <- cbind(tgram, as.data.frame(cumfract))
# Plot a distribution of all bigrams
smTGram <- tgram[which(tgram$freq>35),]
smTGram$word <- factor(smTGram$word, levels = smTGram$word)
smTGramDist <- ggplot(smTGram, aes(y=freq, x=word))
smTGramDist <- smTGramDist + geom_point(stat = "identity")
# bgramDist <- ggplot(bgram, aes(x=fract))
# bgramDist <- bgramDist + geom_histogram(binwidth = .00001)
MeanX <- mean(tgram$freq,na.rm=TRUE)
MedX <- median(tgram$freq, na.rm=TRUE)
Max <- max(tgram$freq,na.rm=TRUE)
MaxY <- round(which.max(density(tgram$freq,na.rm=TRUE)$y), 0)
MaxX <- round(density(tgram$freq,na.rm=TRUE)$x[MaxY], 0)
smTGramDist <- smTGramDist + geom_hline(yintercept=MeanX, color="red")
smTGramDist <- smTGramDist + geom_hline(yintercept=MedX, color="darkred")
# smBGramDist <- smBGramDist + geom_hline(yintercept=Max, color="blue")
smTGramDist <- smTGramDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=1,
                                      label=paste("Avg. = ", signif(MeanX,4)),
                                      color="red")
smTGramDist <- smTGramDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=3,
                                      label=paste("Median = ", signif(MedX,3)),
                                      color="darkred")
smTGramDist <- smTGramDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=5,
                              label=paste("Total 3-Grams ", bigramTotal),
                              color="blue")
# smBGramDist <- smBGramDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=7,
#                               label=paste("Maximum Occurances =", Max),
#                               color="purple")
smTGramDist <- smTGramDist + annotate("text", x=Inf, y=Inf, hjust=1, vjust=7,
                                      label=paste("Maximum Occurances ", round(Max, 4)),
                                      color="purple")

# bgramDist <- bgramDist + xlim(0,0.0005)
smTGramDist <- smTGramDist + theme(axis.ticks=element_blank(), axis.text.x = element_blank())
smTGramDist <- smTGramDist + xlab("Frequency of 3-Gram Appearance") + ylab("Count of Frequency")
print(smTGramDist)
#wordcloud
#limit words by specifying min frequency
wordcloud(smTGram$word,smTGram$freq, min.freq=45)


gChars <- ggplot(countsLine, aes(x=WordChars))
gChars <- gChars + geom_histogram(na.rm = TRUE, binwidth = 10)
charsMeanX <- mean(countsLine$WordChars,na.rm=TRUE)
charsMax <- max(countsLine$WordChars,na.rm=TRUE)
charsMaxY <- round(which.max(density(countsLine$WordChars,na.rm=TRUE)$y), 0)
charsMaxX <- round(density(countsLine$WordChars,na.rm=TRUE)$x[charsMaxY], 0)
gChars <- gChars + geom_vline(xintercept=charsMeanX, color="red")
gChars <- gChars + geom_vline(xintercept=charsMaxX, color="blue")
gChars <- gChars + annotate("text", x=Inf, y=Inf, hjust=1, vjust=1,
                            label=paste("Avg. = ", round(charsMeanX, 2)),
                            color="red")
gChars <- gChars + annotate("text", x=Inf, y=Inf, hjust=1, vjust=3,
                            label=paste("Most frequent ", round(charsMaxX, 2)),
                            color="blue")
gChars <- gChars + annotate("text", x=Inf, y=Inf, hjust=1, vjust=5,
                            label=paste("Maximum ", charsMax),
                            color="purple")
gChars <- gChars + xlim(0,600)
gChars <- gChars + xlab("# Word Characters per Line") + ylab("Count of Occurance")

gWords <- ggplot(countsLine, aes(x=Words))
gWords <- gWords + geom_histogram(na.rm = TRUE, binwidth = 3)
wordsMeanX <- mean(countsLine$Words)
wordsMax <- max(countsLine$Words)
wordsMaxY <- round(which.max(density(countsLine$Words)$y), 0)
wordsMaxX <- round(density(countsLine$Words)$x[wordsMaxY], 0)
gWords <- gWords + geom_vline(xintercept=wordsMeanX, color="red")
gWords <- gWords + geom_vline(xintercept=wordsMaxX, color="blue")
gWords <- gWords + annotate("text", x=Inf, y=Inf, hjust=1, vjust=1,
                            label=paste("Avg. = ", round(wordsMeanX, 2)),
                            color="red")
gWords <- gWords + annotate("text", x=Inf, y=Inf, hjust=1, vjust=3,
                            label=paste("Most frequent ", round(wordsMaxX, 2)),
                            color="blue")
gWords <- gWords + annotate("text", x=Inf, y=Inf, hjust=1, vjust=5,
                            label=paste("Maximum ", wordsMax),
                            color="purple")
gWords <- gWords + xlim(0,150)
gWords <- gWords + xlab("# Words per Line") + ylab("Count of Occurance")

gWordLen <- ggplot(countsLine, aes(x=WordLen))
gWordLen <- gWordLen + geom_histogram(na.rm = TRUE, binwidth = .2)
wordLenMeanX <- mean(countsLine$WordLen,na.rm=TRUE)
wordLenMax <- max(countsLine$WordLen,na.rm=TRUE)
wordLenMaxY <- round(which.max(density(countsLine$WordLen,na.rm=TRUE)$y), 0)
wordLenMaxX <- round(density(countsLine$WordLen,na.rm=TRUE)$x[wordLenMaxY], 0)
gWordLen <- gWordLen + geom_vline(xintercept=wordLenMeanX, color="red")
gWordLen <- gWordLen + geom_vline(xintercept=wordLenMaxX, color="blue")
gWordLen <- gWordLen + annotate("text", x=Inf, y=Inf, hjust=1, vjust=1,
                            label=paste("Avg. = ", round(wordLenMeanX, 2)),
                            color="red")
gWordLen <- gWordLen + annotate("text", x=Inf, y=Inf, hjust=1, vjust=3,
                            label=paste("Most frequent ", round(wordLenMaxX, 2)),
                            color="blue")
gWordLen <- gWordLen + annotate("text", x=Inf, y=Inf, hjust=1, vjust=5,
                            label=paste("Maximum ", wordLenMax),
                            color="purple")
gWordLen <- gWordLen + xlim(0,10)
gWordLen <- gWordLen + xlab("Average Word Length per Line") + ylab("Count of Occurance")

q <- ggarrange(gChars, gWords, gWordLen, ncol=3)
print(q)

stopCluster(cluster)
registerDoSEQ()
