### Read in the data and break it down into tokens and save as n_grams

require(quanteda)
require(quanteda.textmodels)
require(quanteda.textstats)
require(quanteda.textplots)
require(readtext)
require(devtools)
require(quanteda.corpora)
require(tidyr)
require(methods)
require(dplyr)
require(data.table)


prepareFile <- function(conFile) {
    # Load in the shutterstock list of profane words
    profane <-
        readLines(
            "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
        )
    
    # Create where we are going to store the exploratory data analysis info
    countsLine <-
        data.frame(WordChars = NULL,
                   Words  = NULL,
                   WordLen = NULL)
    # Loop through the data file and process in chunks. Necessary
    # to manage memory use for very large data sets
    # Number of lines to process in each chunk
    dataChunk <- 8000
    DFM_onegram <- NULL
    DFM_2gram <- NULL
    DFM_3gram <- NULL
    DFM_4gram <- NULL
    DFM_5gram <- NULL
    loop = TRUE
    print("   ...read and tokenize file")
    while (loop) {
        # read lines from the file until there is nothing left and break
        text = readLines(conFile, n = dataChunk)
        if (length(text) == 0) {
            break
        }
        # convert the chunk of lines into a corpus
        docs <- quanteda::corpus(text)
        
        # Convert the document into a list of tokens removing the stuff we don't
        # want that can be done within the function
        toks <-
            quanteda::tokens(
                docs,
                remove_punct = TRUE,
                remove_symbols = TRUE,
                remove_numbers = TRUE,
                remove_url = TRUE,
                remove_separators = TRUE,
                split_hyphens = TRUE,
                tolower = TRUE
            )
        toksClean <- quanteda::tokens_remove(toks, profane)
        
        # Create the 1, 2, 3-grams
        toks_2grams <- tokens_ngrams(toksClean, n=2)
        toks_3grams <- tokens_ngrams(toksClean, n = 3)
        toks_4grams <- tokens_ngrams(toksClean, n = 4)
        toks_5grams <- tokens_ngrams(toksClean, n = 5)
        
        # Create or add to the DFM for the onegrams
        if( is.null(DFM_onegram)) {
            DFM_onegram <- dfm(toksClean)
        } else {
            # Use an optimized overidden version of rbind for quanteda
            # for DFMs. This will increment counts for if they both 
            # have the same tokens            DFM_onegram <- rbind(DFM_onegram, dfm(toksClean))
        }
        # Create or add to the DFM for the two-grams
        if( is.null(DFM_2gram)) {
            DFM_2gram <- dfm(toks_2grams)
        } else {
            # Use an optimized overidden version of rbind for quanteda
            # for DFMs. This will increment counts for if they both 
            # have the same tokens            DFM_2gram <- rbind(DFM_2gram, dfm(toks_2grams))
        }
        # Create or add to the DFM for the three-grams
        if (is.null(DFM_3gram)) {
            DFM_3gram <- dfm(toks_3grams)
        } else {
            # Use an optimized overidden version of rbind for quanteda
            # for DFMs. This will increment counts for if they both 
            # have the same tokens
            DFM_3gram <- rbind(DFM_3gram, dfm(toks_3grams))
        }
        # Create or add to the DFM for the four-grams
        if (is.null(DFM_4gram)) {
            DFM_4gram <- dfm(toks_4grams)
        } else {
            # Use an optimized overidden version of rbind for quanteda
            # for DFMs. This will increment counts for if they both 
            # have the same tokens
            DFM_4gram <- rbind(DFM_4gram, dfm(toks_4grams))
        }
        # Create or add to the DFM for the five-grams
        if (is.null(DFM_5gram)) {
            DFM_5gram <- dfm(toks_5grams)
        } else {
            # Use an optimized overidden version of rbind for quanteda
            # for DFMs. This will increment counts for if they both 
            # have the same tokens
            DFM_5gram <- rbind(DFM_5gram, dfm(toks_5grams))
        }
    }
    # print("Onegram Totals")
    totals_onegram <- colSums(DFM_onegram)
    # print(head(totals_onegram, 25))
    # print("Two-gram Totals")
    totals_2gram <- colSums(DFM_2gram)
    # print(head(totals_2gram, 25))
    # print("Three-gram Totals")
    totals_3gram <- colSums(DFM_3gram)
    # print(head(totals_3gram, 5))
    totals_4gram <- colSums(DFM_4gram)
    totals_5gram <- colSums(DFM_5gram)
    
    # Write the data to a file to use later
    save(DFM_onegram, file="Data\\final\\en_US\\DFM_onegram.RData")
    save(DFM_2gram, file="Data\\final\\en_US\\DFM_2gram.RData")
    save(DFM_3gram, file="Data\\final\\en_US\\DFM_3gram.RData")
    save(DFM_4gram, file="Data\\final\\en_US\\DFM_4gram.RData")
    save(DFM_5gram, file="Data\\final\\en_US\\DFM_5gram.RData")
    save(totals_onegram, file="Data\\final\\en_US\\totals_onegram.RData")
    save(totals_2gram, file="Data\\final\\en_US\\totals_2gram.RData")
    save(totals_3gram, file="Data\\final\\en_US\\totals_3gram.RData")
    save(totals_4gram, file="Data\\final\\en_US\\totals_4gram.RData")
    save(totals_5gram, file="Data\\final\\en_US\\totals_5gram.RData")
    
    close(conFile)
    # print("   ...breaking up columns")
    # 
    # three_counts <- data.frame(totals_3gram)
    # colnames(three_counts)[1] <-  "count"
    # 
    # three_counts$three_gram <- row.names(three_counts)
    # three_counts <-
    #     separate(three_counts,
    #              "three_gram",
    #              c('first_word', 'second_word', 'third_word'),
    #              sep = '_')
    # # print(paste("three_gram column names: ", colnames(three_counts)))
    # print("three_gram head: ")
    # print(head(three_counts))
    
    return()
}

prepareData <- function() {
    
    # Load our test set
    prodTestingFile <- "Data\\final\\en_US\\prod-training.txt"
    print(paste("   training data file: ", prodTestingFile))
    conFile <- file(prodTestingFile, "r")
    prepareFile(conFile)

    return ()
}
