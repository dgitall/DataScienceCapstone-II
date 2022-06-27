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
require(futile.logger)


prepareFile <- function(conFile, saveData = TRUE, only5Gram = TRUE) {
    # Load in the shutterstock list of profane words
    profane <-
        readLines(
            "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
        )
    flog.info("   ...Profanity list loaded")
    # Create where we are going to store the exploratory data analysis info
    # countsLine <-
    #     data.frame(WordChars = NULL,
    #                Words  = NULL,
    #                WordLen = NULL)
    # Loop through the data file and process in chunks. Necessary
    # to manage memory use for very large data sets
    # Number of lines to process in each chunk
    dataChunk <- 2000
    DFM_onegram <- NULL
    DFM_2gram <- NULL
    DFM_3gram <- NULL
    DFM_4gram <- NULL
    DFM_5gram <- NULL
    loop = TRUE
    count = 0
    flog.info("   ...read and tokenize file")
    while (loop) {
        tryCatch({
            count <- count + dataChunk
            # if (count %% dataChunk)
                flog.debug("      ...Reading lines: %d", count)
            # read lines from the file until there is nothing left and break
            text = readLines(conFile, n = dataChunk)
            flog.trace(text)
            if (length(text) == 0) {
                flog.debug("End Of File")
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
                    split_hyphens = TRUE
                )
            toks <- quanteda::tokens_tolower(toks)
            toks <- quanteda::tokens_remove(toks, profane)
            
            # Create the 1, 2, 3-grams
            toks_2grams <- tokens_ngrams(toks, n = 2)
            toks_3grams <- tokens_ngrams(toks, n = 3)
            toks_4grams <- tokens_ngrams(toks, n = 4)
            toks_5grams <- tokens_ngrams(toks, n = 5)
            
            if(!only5Gram) {
                # Create or add to the DFM for the onegrams
                if (is.null(DFM_onegram)) {
                    DFM_onegram <- dfm(toks)
                } else {
                    # Use an optimized overidden version of rbind for quanteda
                    # for DFMs. This will increment counts for if they both
                    # have the same tokens
                    DFM_onegram <- rbind(DFM_onegram, dfm(toks))
                }
                # Create or add to the DFM for the two-grams
                if (is.null(DFM_2gram)) {
                    DFM_2gram <- dfm(toks_2grams)
                } else {
                    # Use an optimized overidden version of rbind for quanteda
                    # for DFMs. This will increment counts for if they both
                    # have the same tokens
                    DFM_2gram <- rbind(DFM_2gram, dfm(toks_2grams))
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
        },
        warning = function(warn)
        {
            flog.warn("MY WARNING: %s", warn)
            
        },
        error = function(err) {
            flog.error("MY ERROR:  %s", err)
            flog.error("Line Count: %s", count)
            flog.error("Text: %s", text)
            flog.error("Tokens: %s", head(toks))
            flog.error("Tokens Clean: %s", head(toks))
            flog.error("DFM_onegram: %s", head(DFM_onegram))
            flog.error("2_grams: %s", head(toks_2grams))
            flog.error("DFM_2gram: %s", head(DFM_2gram))
            flog.error("3_grams: %s", head(toks_3grams))
            flog.error("DFM_3gram: %s", head(DFM_3gram))
            flog.error("4_grams: %s", head(toks_4grams))
            flog.error("DFM_4gram: %s", head(DFM_4gram))
            flog.error("5_grams: %s", head(toks_5grams))
            flog.error("DFM_5gram: %s", head(DFM_5gram))
        },
        finally = function(f) {
            # print(paste("f: ", f))
        })
    }
    
    tryCatch({
        flog.trace("Onegram Totals")
        totals_onegram <- colSums(DFM_onegram)
        flog.trace(head(totals_onegram, 25))
        flog.trace("Two-gram Totals")
        totals_2gram <- colSums(DFM_2gram)
        flog.trace(head(totals_2gram, 25))
        flog.trace("Three-gram Totals")
        totals_3gram <- colSums(DFM_3gram)
        flog.trace(head(totals_3gram, 5))
        totals_4gram <- colSums(DFM_4gram)
        totals_5gram <- colSums(DFM_5gram)
    },
    error = function(err) {
        flog.error("MY ERROR:  %s", err)
        flog.error("Column Sums: %s", count)
        flog.error("totals_onegram: %s", head(totals_onegram))
        flog.error("DFM_onegram: %s", head(DFM_onegram))
        flog.error("totals_2gram: %s", head(totals_2gram))
        flog.error("DFM_2gram: %s", head(DFM_2gram))
        flog.error("totals_3gram: %s", head(totals_3gram))
        flog.error("3_grams: %s", head(toks_3grams))
        flog.error("DFM_3gram: %s", head(DFM_3gram))
        flog.error("totals_4gram: %s", head(totals_4gram))
        flog.error("DFM_4gram: %s", head(DFM_4gram))
        flog.error("totals_5gram: %s", head(totals_5gram))
        flog.error("DFM_5gram: %s", head(DFM_5gram))
    })
    
    # Write the data to a file to use later
    if (saveData == TRUE)
    {
        save(DFM_onegram, file = "Data\\final\\en_US\\DFM_onegram.RData")
        save(DFM_2gram, file = "Data\\final\\en_US\\DFM_2gram.RData")
        save(DFM_3gram, file = "Data\\final\\en_US\\DFM_3gram.RData")
        save(DFM_4gram, file = "Data\\final\\en_US\\DFM_4gram.RData")
        save(DFM_5gram, file = "Data\\final\\en_US\\DFM_5gram.RData")
        save(totals_onegram, file = "Data\\final\\en_US\\totals_onegram.RData")
        save(totals_2gram, file = "Data\\final\\en_US\\totals_2gram.RData")
        save(totals_3gram, file = "Data\\final\\en_US\\totals_3gram.RData")
        save(totals_4gram, file = "Data\\final\\en_US\\totals_4gram.RData")
        save(totals_5gram, file = "Data\\final\\en_US\\totals_5gram.RData")
    }
    
    
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
    
    if(only5Gram) result = totals_5gram
    else result = NULL
    
    return(result)
}

prepareData <- function(loggingTreshold = INFO,
                        logFile = NULL) {
    flog.threshold(loggingTreshold)
    if (is.null(logFile)) {
        flog.appender(appender.console())
    }
    else {
        flog.appender(appender.file(logFile))
    }
    # Load our test set
    devTestingFile <-
        "D:\\Documents\\Work\\Data Analytics\\Data Science Specialization\\Capstone Project\\ProjectII\\DataScienceCapstone-II\\Data\\final\\en_US\\dev-training.txt"
    prodTestingFile <-
        "D:\\Documents\\Work\\Data Analytics\\Data Science Specialization\\Capstone Project\\ProjectII\\DataScienceCapstone-II\\Data\\final\\en_US\\prod-training.txt"
    flog.info("   training data file: %s", prodTestingFile)
    conFile <- file(prodTestingFile, "r")
    flog.info(conFile)
    prepareFile(conFile)
    close(conFile)
    
    return ()
}
