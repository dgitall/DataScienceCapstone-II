mydf1 <- data.frame(ID = c(1,2,3,4,5), 
                    sum = c(5, 5, 5, 5, 5), 
                    name = c("tom", "dick", "harry", "steve", "mike"))
mydf2 <- data.frame(ID = c(1,2,99), 
                    sum = c(7, 7, 7), 
                    name = c("tom", "dick", "Aaron"))


library(dplyr)
mydf <- mydf1 %>%
    full_join(mydf2, by = c("ID", "name")) %>%     
    rowwise() %>%
    transmute(ID, name, sum = sum(sum.x, sum.y, na.rm=TRUE))

Total <- sum(mydf$sum)
mydf <- mydf %>% arrange(desc(sum)) 
mydf<- mydf %>% mutate(fract = sum/Total)
cumfract <- cumsum(mydf$fract)
mydf <- cbind(mydf, as.data.frame(cumfract))

print(mydf)
      print(mydf1)
            print(mydf2)
            
            fullDist <- ggplot(mydf, aes(x=fract))
            fullDist <- fullDist + geom_histogram(binwidth = .01)
            print(fullDist)
            
            library("RWeka")
            library("tm")
            library(ngram)
            texts <- c("This is the first document.", "This is the second file.", "This is the third text.")
            docs <- VCorpus(VectorSource(texts))
            TrigramTokenizer <- function(x) NGramTokenizer(x, 
                                                           Weka_control(min=2, max=2))
# Using Tyler's method of making the 'Text' object here
tdm <- TermDocumentMatrix(docs, 
                          control = list(tokenize = TrigramTokenizer))
m <- as.matrix(tdm)
v <- rowSums(m)
tmp <- data.frame(word = names(v),freq=v)

library(RTextTools)
texts <- c("This is the first document.", "This is the second file.", "This is the third text.")
matrix <- create_matrix(texts,ngramLength=3)
m <- as.matrix(matrix)
v <- rowSums(m)
tmp <- data.frame(word = names(v),freq=v)


sample <- "The text didn't work like we thought-even though it should. But--why'd."
resam <- removePunctuation(sample, preserve_intra_word_contractions = TRUE, 
                           preserve_intra_word_dashes = TRUE)
resam2 <- removePunctuation(sample, preserve_intra_word_contractions = FALSE, 
                           preserve_intra_word_dashes = FALSE)
