require(quanteda)
require(quanteda.textmodels)
require(quanteda.textstats)
require(quanteda.textplots)
require(readtext)
require(devtools)
# require(quanteda.corpora)
require(tidyr)
require(methods)
require(dplyr)
require(data.table)

source("Tokenize_Files.R")


prepareFileLocal <- function(conFile) {
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
        # toks_2grams <- tokens_ngrams(toksClean, n=2)
        toks_5grams <- tokens_ngrams(toksClean, n = 5)
        
        # Create or add to the DFM for the onegrams
        # if( is.null(DFM_onegram)) {
        #     DFM_onegram <- dfm(toksClean)
        # } else {
        #     DFM_onegram <- rbind(DFM_onegram, dfm(toksClean))
        # }
        # # Create or add to the DFM for the two-grams
        # if( is.null(DFM_2gram)) {
        #     DFM_2gram <- dfm(toks_2grams)
        # } else {
        #     DFM_2gram <- rbind(DFM_2gram, dfm(toks_2grams))
        # }
        # Create or add to the DFM for the three-grams
        if (is.null(DFM_3gram)) {
            DFM_5gram <- dfm(toks_5grams)
        } else {
            # Use an optimized overidden version of rbind for quanteda
            # for DFMs. This will increment counts for if they both
            # have the same tokens
            DFM_5gram <- rbind(DFM_5gram, dfm(toks_5grams))
        }
    }
    # print("Onegram Totals")
    # totals_onegram <- colSums(DFM_onegram)
    # print(head(totals_onegram, 25))
    # print("Two-gram Totals")
    # totals_2gram <- colSums(DFM_2gram)
    # print(head(totals_2gram, 25))
    # print("Three-gram Totals")
    totals_5gram <- colSums(DFM_5gram)
    # print(head(totals_3gram, 5))
    
    # Write the data to a file to use later
    # save(DFM_onegram, file="Data\\final\\en_US\\DFM_onegram.RData")
    # save(DFM_2gram, file="Data\\final\\en_US\\DFM_2gram.RData")
    # save(DFM_3gram, file="Data\\final\\en_US\\DFM_3gram.RData")
    # save(totals_onegram, file="Data\\final\\en_US\\totals_onegram.RData")
    # save(totals_2gram, file="Data\\final\\en_US\\totals_2gram.RData")
    # save(totals_3gram, file="Data\\final\\en_US\\totals_3gram.RData")
    
    close(conFile)
    print("   ...breaking up columns")
    
    five_counts <- data.frame(totals_5gram)
    colnames(five_counts)[1] <-  "count"
    
    five_counts$five_gram <- row.names(five_counts)
    five_counts <-
        separate(five_counts,
                 "five_gram",
                 c('first_word', 'second_word', 'third_word', 'fourth_word', 'fifth_word'),
                 sep = "[_]+",
                 extra = "merge",
                 fill = "right")
    # print(paste("three_gram column names: ", colnames(three_counts)))
    print("three_gram head: ")
    print(head(five_counts))
    
    return(five_counts)
}

# Look for the word anywhere in the provided list
findWord <- function(list, word) {
    max(list$word %in% list(word)) >= 1
}

prepareData3 <- function() {
    result <- list()
    
    # Load our test set
    prodTestingFile <- "Data\\final\\en_US\\prod-testing.txt"
    print(paste("   ...testing data file: ", prodTestingFile))
    conFile <- file(prodTestingFile, "r")
    three_counts <- prepareFile(conFile)
    three_sum <-
        sum(three_counts$count)
    print(three_sum)
    
    result$sum = three_sum
    result$counts <- three_counts
    
    print(result$sum)
    print(head(result$counts))
    
    return (result)
}

prepareData5 <- function(logName = NULL) {
    result <- list()
    
    # Load our test set
    prodTestingFile <- "Data\\final\\en_US\\prod-testing.txt"
    flog.info(paste("   ...testing data file: ", prodTestingFile), name = logName)
    conFile <- file(prodTestingFile, "r")
    five_counts <-
        prepareFile(conFile, saveData = FALSE, saveDataSuffix = "", only5Gram = TRUE, logName = logName)
    flog.info("   ...Finished preparing file", name = logName)
    flog.info("   ...breaking up columns", name = logName)
    
    five_counts <- data.frame(five_counts)
    colnames(five_counts)[1] <-  "count"
    
    five_counts$five_gram <- row.names(five_counts)
    five_counts <-
        separate(five_counts,
                 "five_gram",
                 c('first_word', 'second_word', 'third_word', 'fourth_word', 'fifth_word'),
                 sep = "[_]+",
                 extra = "merge",
                 fill = "right")
    # print(paste("three_gram column names: ", colnames(three_counts)))
    flog.debug("five_gram head: ", name = logName)
    flog.debug(head(five_counts), name = logName)
    flog.debug(colnames(five_counts), name = logName)

    five_sum <-
        sum(five_counts$count)
    flog.debug(five_sum, name = logName)
    
    result$sum = five_sum
    result$counts <- five_counts
    
    flog.debug(result$sum, name = logName)
    flog.debug(head(result$counts), name = logName)
    
    return (result)
}


validateSimple <-
    function(doLoadData = FALSE,
             doPrepareData = FALSE,
             numberTests = -1,
             modelFile = NULL,
             testingFile = NULL, loggingTreshold = INFO, logFile = NULL) {
        
        flog.threshold(loggingTreshold, name = 'validate.log')
        if (is.null(logFile)) {
            flog.appender(appender.console(), name = 'validate.log')
        }
        else {
            flog.appender(appender.file(logFile), name = 'validate.log')
        }

        
        flog.info("START VALIDATING THE SIMPLE MODELS", name = 'validate.log')
        flog.info("----------------------------------", name = 'validate.log')
        
        if ((!doLoadData && exists("model_list")) || is.null(modelFile)) {
            flog.info("NO NEED TO LOAD DATA", name = 'validate.log')
        }
        else {
            flog.info("LOADING SIMPLE MODELS", name = 'validate.log')
            # Load the list of simple models
            load(file = modelFile )
        }

        
        if (!doPrepareData && exists("testingData")) {
            flog.info("NO NEED TO PREPARE DATA", name = 'validate.log')
        }
        else if(!doPrepareData && !exists("testingData") && !is.null(testingFile)) {
            flog.info("LOAD TESTING DATA", name = 'validate.log')
            load(testingFile)
        }
        else {
            flog.info("PREPARE DATA", name = 'validate.log')
            testingData <- prepareData5(logName = 'validate.log')
            if(!is.null(testingFile)) {
                save(testingData, file = testingFile)
            }
        }
        flog.debug(testingData$sum, name = 'validate.log')                                               
        flog.debug(head(testingData$counts), name = 'validate.log')
        
        cum_count = 0
        word1_error = 0
        word2_error = 0
        word3_error = 0
        word5_error = 0
        word1_error_weighted = 0
        word2_error_weighted = 0
        word3_error_weighted = 0
        word5_error_weighted = 0
        word1_error_any = 0
        word2_error_any = 0
        word3_error_any = 0
        word5_error_any = 0
        word1_error_any_weighted = 0
        word2_error_any_weighted = 0
        word3_error_any_weighted = 0
        word5_error_any_weighted = 0
        flog.info("START VALIDATING", name = 'validate.log')
        flog.info(paste("Number of rows: ", nrow(testingData$counts)), name = 'validate.log')
        flog.info(paste("Number of 5-grams: ", testingData$sum), name = 'validate.log')
 
        
        if (numberTests == -1) {
            num_tests = nrow(testingData$counts)
        }
        else {
            num_tests = min(numberTests, nrow(testingData$counts))
        }
        
        for (i in 1:num_tests) {
            cum_count <- cum_count + testingData$counts$count[i]
            flog.debug(testingData$counts[i], name = 'validate.log')
            flog.debug(testingData$counts[i,])
            if (is.null(model_list$model_one)) {
                estimated_word1 <- NULL
                word1_error        <- NULL    
                word1_error_weighted <- NULL
                word1_error_any <- NULL
                word1_error_any_weighted <- NULL
            }

            else {
                estimated_word1 <- model_list$model_one$predict()
                # Calculate errors based on the top guess
                if (estimated_word1$word[1] != testingData$counts$fifth_word[i]) {
                    word1_error = word1_error + 1
                    word1_error_weighted = word1_error_weighted + testingData$counts$count[i]
                }
                # Calculate errors based any guess in the list
                if (!findWord(estimated_word1, testingData$counts$fifth_word[i])) {
                    word1_error_any = word1_error_any + 1
                    word1_error_any_weighted = word1_error_any_weighted + testingData$counts$count[i]
                }
            }
            # print(estimated_word1)
            if (is.null(model_list$model_two)) {
                estimated_word2 <- NULL
                word2_error        <- NULL    
                word2_error_weighted <- NULL
                word2_error_any <- NULL
                word2_error_any_weighted <- NULL
                }
            else {
                estimated_word2 <-
                    model_list$model_two$predict(testingData$counts$fourth_word[i])
                if (estimated_word2$word[1] != testingData$counts$fifth_word[i]) {
                    word2_error = word2_error + 1
                    word2_error_weighted = word2_error_weighted + testingData$counts$count[i]
                }
                if (!findWord(estimated_word2, testingData$counts$fifth_word[i])) {
                    word2_error_any = word2_error_any + 1
                    word2_error_any_weighted = word2_error_any_weighted + testingData$counts$count[i]
                }
            }
            
            if (is.null(model_list$model_three)){
                estimated_word3 <- NULL
                word3_error        <- NULL    
                word3_error_weighted <- NULL
                word3_error_any <- NULL
                word3_error_any_weighted <- NULL
            }
            else {
                estimated_word3 <-
                    model_list$model_three$predict(testingData$counts$third_word[i],
                                                     testingData$counts$fourth_word[i])
                if (estimated_word3$word[1] != testingData$counts$fifth_word[i]) {
                    word3_error = word3_error + 1
                    word3_error_weighted = word3_error_weighted + testingData$counts$count[i]
                }
                
                
                if (!findWord(estimated_word3, testingData$counts$fifth_word[i])) {
                    # print(paste("testingData$counts$third_word[i]: ",testingData$counts$third_word[i]))
                    # print(estimated_word3)
                    word3_error_any = word3_error_any + 1
                    word3_error_any_weighted = word3_error_any_weighted + testingData$counts$count[i]
                }
            }
            if (is.null(model_list$model_five)){
                estimated_word5 <- NULL
                word5_error        <- NULL    
                word5_error_weighted <- NULL
                word5_error_any <- NULL
                word5_error_any_weighted <- NULL
            }
            else {
                estimated_word5 <-
                    model_list$model_five$predict(testingData$counts$first_word[i],
                                                     testingData$counts$second_word[i],
                                                  testingData$counts$third_word[i],
                                                  testingData$counts$fourth_word[i])
                if (estimated_word5$word[1] != testingData$counts$fifth_word[i]) {
                    word5_error = word5_error + 1
                    word5_error_weighted = word5_error_weighted + testingData$counts$count[i]
                }
                if (!findWord(estimated_word5, testingData$counts$fifth_word[i])) {
                    flog.debug(paste("testingData$counts$third_word[i]: ",testingData$counts$third_word[i]))
                    flog.debug(estimated_word3)
                    word5_error_any = word5_error_any + 1
                    word5_error_any_weighted = word5_error_any_weighted + testingData$counts$count[i]
                }
            }
            
            
            if (i %% 1000 == 0)
            {
                flog.info(paste("Interation: ", i), name = 'validate.log')
                flog.info(paste("           cum_count = ", cum_count), name = 'validate.log')
                flog.info(paste("           word1_error = ", word1_error), name = 'validate.log')
                flog.info(paste("           word2_error = ", word2_error), name = 'validate.log')
                flog.info(paste("           word3_error = ", word3_error), name = 'validate.log')
                flog.info(paste("           word5_error = ", word5_error), name = 'validate.log')
                flog.info(paste("           word1_error_weighted = ", word1_error_weighted), name = 'validate.log')
                flog.info(paste("           word2_error_weighted = ", word2_error_weighted), name = 'validate.log')
                flog.info(paste("           word3_error_weighted = ", word3_error_weighted), name = 'validate.log')
                flog.info(paste("           word5_error_weighted = ", word5_error_weighted), name = 'validate.log')
                flog.info(paste("           word1_error_any = ", word1_error_any), name = 'validate.log')
                flog.info(paste("           word2_error_any = ", word2_error_any), name = 'validate.log')
                flog.info(paste("           word3_error_any = ", word3_error_any), name = 'validate.log')
                flog.info(paste("           word5_error_any = ", word5_error_any), name = 'validate.log')
                flog.info(paste(
                    "           word1_error_any_weighted = ",
                    word1_error_any_weighted
                ), name = 'validate.log')
                flog.info(paste(
                    "           word2_error_any_weighted = ",
                    word2_error_any_weighted
                ), name = 'validate.log')
                flog.info(paste(
                    "           word3_error_any_weighted = ",
                    word3_error_any_weighted
                ), name = 'validate.log')
                flog.info(paste(
                    "           word5_error_any_weighted = ",
                    word5_error_any_weighted
                ), name = 'validate.log')
            }
        }
        numberCounted <- i
        flog.info("VALIDATION RESULTS:", name = 'validate.log')
        flog.info(paste(
            "Number of n-grams tested: ",
            numberCounted,
            " out of ",
            nrow(testingData$counts)
        ), name = 'validate.log')
        flog.info(
            paste(
                "Cumulative count of n-grams tested: ",
                cum_count,
                " out of ",
                testingData$sum
            )
            , name = 'validate.log')
        flog.info(
            paste(
                "Word 1 Model errors for first guess: ",
                word1_error,
                " (",
                round(word1_error / numberCounted, digits = 2) * 100,
                "%) and weighted ",
                word1_error_weighted,
                " (",
                round(word1_error_weighted / cum_count, digits = 2) * 100,
                "%)"
            )
            , name = 'validate.log')
        flog.info(
            paste(
                "Word 2 Model errors for first guess: ",
                word2_error,
                " (",
                round(word2_error / numberCounted, digits = 2) * 100,
                "%) and weighted ",
                word2_error_weighted,
                " (",
                round(word2_error_weighted / cum_count, digits = 2) * 100,
                "%)"
            )
            , name = 'validate.log')
        flog.info(
            paste(
                "Word 3 Model errors for first guess: ",
                word3_error,
                " (",
                round(word3_error / numberCounted, digits = 2) * 100,
                "%) and weighted ",
                word3_error_weighted,
                " (",
                round(word3_error_weighted / cum_count, digits = 2) * 100,
                "%)"
            )
            , name = 'validate.log')
        flog.info(
            paste(
                "Word 5 Model errors for first guess: ",
                word5_error,
                " (",
                round(word5_error / numberCounted, digits = 2) * 100,
                "%) and weighted ",
                word5_error_weighted,
                " (",
                round(word5_error_weighted / cum_count, digits = 2) * 100,
                "%)"
            )
            , name = 'validate.log')
        
        flog.info(
            paste(
                "Word 1 Model errors for any guess: ",
                word1_error_any,
                " (",
                round(word1_error_any / numberCounted, digits = 2) * 100,
                "%) and weighted ",
                word1_error_any_weighted,
                " (",
                round(word1_error_any_weighted / cum_count, digits = 2) * 100,
                "%)"
            )
            , name = 'validate.log')
        flog.info(
            paste(
                "Word 2 Model errors for any guess: ",
                word2_error_any,
                " (",
                round(word2_error_any / numberCounted, digits = 2) * 100,
                "%) and weighted ",
                word2_error_any_weighted,
                " (",
                round(word2_error_any_weighted / cum_count, digits = 2) * 100,
                "%)"
            )
            , name = 'validate.log')
        flog.info(
            paste(
                "Word 3 Model errors for any guess: ",
                word3_error_any,
                " (",
                round(word3_error_any / numberCounted, digits = 2) * 100,
                "%) and weighted ",
                word3_error_any_weighted,
                " (",
                round(word3_error_any_weighted / cum_count, digits = 2) * 100,
                "%)"
            )
            , name = 'validate.log')
        flog.info(
            paste(
                "Word 5 Model errors for any guess: ",
                word5_error_any,
                " (",
                round(word5_error_any / numberCounted, digits = 2) * 100,
                "%) and weighted ",
                word5_error_any_weighted,
                " (",
                round(word5_error_any_weighted / cum_count, digits = 2) * 100,
                "%)"
            )
            , name = 'validate.log')
    }

