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

source("Tokenize_Files.R")


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
        toks_3grams <- tokens_ngrams(toksClean, n = 3)
        
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
            DFM_3gram <- dfm(toks_3grams)
        } else {
            # Use an optimized overidden version of rbind for quanteda
            # for DFMs. This will increment counts for if they both 
            # have the same tokens
            DFM_3gram <- rbind(DFM_3gram, dfm(toks_3grams))
        }
    }
    # print("Onegram Totals")
    # totals_onegram <- colSums(DFM_onegram)
    # print(head(totals_onegram, 25))
    # print("Two-gram Totals")
    # totals_2gram <- colSums(DFM_2gram)
    # print(head(totals_2gram, 25))
    # print("Three-gram Totals")
    totals_3gram <- colSums(DFM_3gram)
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
    
    three_counts <- data.frame(totals_3gram)
    colnames(three_counts)[1] <-  "count"
    
    three_counts$three_gram <- row.names(three_counts)
    three_counts <-
        separate(three_counts,
                 "three_gram",
                 c('first_word', 'second_word', 'third_word'),
                 sep = '_')
    # print(paste("three_gram column names: ", colnames(three_counts)))
    print("three_gram head: ")
    print(head(three_counts))
    
    return(three_counts)
}

findWord <- function(list, word) {
    max(list$word %in% list(word))==1
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

prepareData5 <- function() {
    
    result <- list()
    
    # Load our test set
    prodTestingFile <- "Data\\final\\en_US\\prod-testing.txt"
    print(paste("   ...testing data file: ", prodTestingFile))
    conFile <- file(prodTestingFile, "r")
    five_counts <- Tokenize_Files::prepareFile(conFile, saveFile=FALSE, only5Gram = TRUE)
    five_sum <-
        sum(five_counts$count)
    print(five_sum)
    
    result$sum = five_sum
    result$counts <- five_counts
    
    print(result$sum)
    print(head(result$counts))
    
    return (result)
}
    
    
validateSimple <- function(doLoadData = FALSE, doPrepareData = FALSE, numberTests = -1) {    
    print("START VALIDATING THE SIMPLE MODELS")
    print("----------------------------------")
    
    if(!doLoadData && exists("model_list2")) {
        print("NO NEED TO LOAD DATA")
    }
    else {
        print("LOADING SIMPLE MODELS")
        # Load the list of simple models
        load(file = "Data\\final\\en_US\\model_list2.RData")       
    }

    if(!doPrepareData && exists("testingData")) {
        print("NO NEED TO PREPARE DATA")
    }
    else {
        print("PREPARE DATA")
        testingData <- prepareData() 
    }
    print(testingData$sum)
    print(head(testingData$counts))
    
    cum_3gram_count = 0
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
    print("START VALIDATING")
    print(paste("Number of rows: ", nrow(testingData$counts)))
    print(paste("Number of 5-grams: ", testingData$sum))
 #   for (i in 1:nrow(testingData$counts)) {
    if(numberTests == -1) {
        num_tests = nrow(testingData$counts)
    }
    else {
        num_tests = min(numberTests, nrow(testingData$counts))
    }
    for (i in 1:num_tests) {
        cum_3gram_count <- cum_3gram_count + testingData$counts$count[i]
        # print(testingData$counts[i,])
        estimated_word1 <- model_list$model_one$predict()
        # print(estimated_word1)
        estimated_word2 <-
            model_list$model_two$predict(testingData$counts$second_word[i])
        estimated_word3 <-
            model_list$model_three$predict_3(testingData$counts$first_word[i], testingData$counts$second_word[i])
        # Calculate errors based on the top guess
        if (estimated_word1$word[1] != testingData$counts$third_word[i]) {
            word1_error = word1_error + 1
            word1_error_weighted = word1_error_weighted + testingData$counts$count[i]
        }
        if (estimated_word2$word[1] != testingData$counts$third_word[i]) {
            word2_error = word2_error + 1
            word2_error_weighted = word2_error_weighted + testingData$counts$count[i]
        }
        if (estimated_word3$word[1] != testingData$counts$third_word[i]) {
            word3_error = word3_error + 1
            word3_error_weighted = word3_error_weighted + testingData$counts$count[i]
        }
        # Calculate errors based any guess in the list
        if (!findWord(estimated_word1, testingData$counts$third_word[i])) {
            word1_error_any = word1_error_any + 1
            word1_error_any_weighted = word1_error_any_weighted + testingData$counts$count[i]
        }
        if (!findWord(estimated_word2, testingData$counts$third_word[i])) {
            word2_error_any = word2_error_any + 1
            word2_error_any_weighted = word2_error_any_weighted + testingData$counts$count[i]
        }
        if (!findWord(estimated_word3, testingData$counts$third_word[i])) {
            # print(paste("testingData$counts$third_word[i]: ",testingData$counts$third_word[i]))
            # print(estimated_word3)
            word3_error_any = word3_error_any + 1
            word3_error_any_weighted = word3_error_any_weighted + testingData$counts$count[i]
        }
        if(i%%1000==0)
        {
            print(paste("Interation: ",i))
            print(paste("           cum_3gram_count = ", cum_3gram_count))
            print(paste("           word1_error = ", word1_error))
            print(paste("           word2_error = ", word2_error))
            print(paste("           word3_error = ", word3_error))
            print(paste("           word1_error_weighted = ", word1_error_weighted))
            print(paste("           word2_error_weighted = ", word2_error_weighted))
            print(paste("           word3_error_weighted = ", word3_error_weighted))
            print(paste("           word1_error_any = ", word1_error_any))
            print(paste("           word2_error_any = ", word2_error_any))
            print(paste("           word3_error_any = ", word3_error_any))
            print(paste("           word1_error_any_weighted = ", word1_error_any_weighted))
            print(paste("           word2_error_any_weighted = ", word2_error_any_weighted))
            print(paste("           word3_error_any_weighted = ", word3_error_any_weighted))


        }
    }
    numberCounted <- i
    print("VALIDATION RESULTS:")
    print(paste("Number of tri-grams tested: ", numberCounted, " out of ", nrow(testingData$counts)))
    print(paste("Cumulative count of tri-grams tested: ", cum_3gram_count, " out of ", testingData$sum))
    print(
        paste(
            "Word 1 Model errors for first guess: ",
            word1_error,
            " (",
            round(word1_error / numberCounted, digits = 2) * 100,
            "%) and weighted ",
            word1_error_weighted,
            " (",
            round(word1_error_weighted / cum_3gram_count, digits = 2) * 100,
            "%)"
        )
    )
    print(
        paste(
            "Word 2 Model errors for first guess: ",
            word2_error,
            " (",
            round(word2_error / numberCounted, digits = 2) * 100,
            "%) and weighted ",
            word2_error_weighted,
            " (",
            round(word2_error_weighted / cum_3gram_count, digits = 2) * 100,
            "%)"
        )
    )
    print(
        paste(
            "Word 3 Model errors for first guess: ",
            word3_error,
            " (",
            round(word3_error / numberCounted, digits = 2) * 100,
            "%) and weighted ",
            word3_error_weighted,
            " (",
            round(word3_error_weighted / cum_3gram_count, digits = 2) * 100,
            "%)"
        )
    )
    
    print(
        paste(
            "Word 1 Model errors for any guess: ",
            word1_error_any,
            " (",
            round(word1_error_any / numberCounted, digits = 2) * 100,
            "%) and weighted ",
            word1_error_any_weighted,
            " (",
            round(word1_error_any_weighted / cum_3gram_count, digits = 2) * 100,
            "%)"
        )
    )
    print(
        paste(
            "Word 2 Model errors for any guess: ",
            word2_error_any,
            " (",
            round(word2_error_any / numberCounted, digits = 2) * 100,
            "%) and weighted ",
            word2_error_any_weighted,
            " (",
            round(word2_error_any_weighted / cum_3gram_count, digits = 2) * 100,
            "%)"
        )
    )
    print(
        paste(
            "Word 3 Model errors for any guess: ",
            word3_error_any,
            " (",
            round(word3_error_any / numberCounted, digits = 2) * 100,
            "%) and weighted ",
            word3_error_any_weighted,
            " (",
            round(word3_error_any_weighted / cum_3gram_count, digits = 2) * 100,
            "%)"
        )
    )
}