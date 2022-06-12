require(quanteda)
require(quanteda.textmodels)
require(quanteda.textstats)
require(quanteda.textplots)
require(readtext)
require(devtools)
require(quanteda.corpora)

prepareFile <- function(conFile) {
    # Load in the shutterstock list of profane words
    profane <- readLines("https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en")
    
    # Create where we are going to store the exploratory data analysis info
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
        
        # Convert the document into a list of tokens removing the stuff we don't
        # want that can be done within the function
        toks <- quanteda::tokens(docs, remove_punct=TRUE, remove_symbols = TRUE, 
                                 remove_numbers = TRUE, remove_url = TRUE, 
                                 remove_separators = TRUE, split_hyphens = TRUE, 
                                 tolower=TRUE)
        toksClean <- quanteda::tokens_remove(toks, profane)
        
        # Create the 1, 2, 3-grams
        # toks_2grams <- tokens_ngrams(toksClean, n=2)
        toks_3grams <- tokens_ngrams(toksClean, n=3)
        
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
        if( is.null(DFM_3gram)) {
            DFM_3gram <- dfm(toks_3grams)
        } else {
            DFM_3gram <- rbind(DFM_3gram, dfm(toks_3grams))
        }
    }
    # print("Onegram Totals")
    # totals_onegram <- colSums(DFM_onegram)
    # print(head(totals_onegram, 25))
    # print("Two-gram Totals")
    # totals_2gram <- colSums(DFM_2gram)
    # print(head(totals_2gram, 25))
    print("Three-gram Totals")
    totals_3gram <- colSums(DFM_3gram)
    print(head(totals_3gram, 5))
    
    # Write the data to a file to use later
    # save(DFM_onegram, file="Data\\final\\en_US\\DFM_onegram.RData")
    # save(DFM_2gram, file="Data\\final\\en_US\\DFM_2gram.RData")
    # save(DFM_3gram, file="Data\\final\\en_US\\DFM_3gram.RData")
    # save(totals_onegram, file="Data\\final\\en_US\\totals_onegram.RData")
    # save(totals_2gram, file="Data\\final\\en_US\\totals_2gram.RData")
    # save(totals_3gram, file="Data\\final\\en_US\\totals_3gram.RData")
    
    close(conFile)
    
    three_counts <- data.frame(totals_3gram)
    colnames(three_counts)[1] <-  "count"

    three_counts$three_gram <- row.names(three_counts)
    three_counts <-  separate(three_counts, "three_gram", c('first_word', 'second_word', 'third_word'), sep='_')
    print(paste("three_gram column names: ", colnames(three_counts)))
    print("three_gram head: ")
    print(head(three_counts))
    
    return(three_counts)
}

findWord <- function(list, word) {
    result = FALSE
    
    print(list)
    
    for (i in 1:nrow(list)) {
        if (list[i,word] == word) {
            result = TRUE
        }
    }
    
    return(result)
}

validateSimple <- function(){
    # Load the list of simple models
    load(file="Data\\final\\en_US\\model_list.RData") 
    
    # Load our test set
    prodTestingFile <- "Data\\final\\en_US\\prod-testing.txt"
    conFile <- file(prodTestingFile, "r") 
    three_counts <- prepareFile(conFile)
    three_sum <- 
        sum(three_counts$count)
    print(three_sum)
    
    word1_error = 0
    word2_error = 0
    word3_error = 0
    word1_error_weighted = 0
    word2_error_weighted = 0
    word3_error_weighted = 0
    word1_error_any = 0
    word2_error_any = 0
    word3_error_any = 0
    word1_error_any_weighted = 0
    word2_error_any_weighted = 0
    word3_error_any_weighted = 0
    
    for (i in 1:nrow(three_counts)) {
        print(three_counts[i, ])
        estimated_word1 <- model_list$model_one$predict()
        print(estimated_word1)
        estimated_word2 <- model_list$model_two$predict(three_counts[i, second_word])
        estimated_word3 <- model_list$model_three$predict_3(three_counts[i, first_word], three_counts[i, second_word])
        # Calculate errors based on the top guess
        if(estimated_word1[1,word] != three_counts[i, third_word]) {
            word1_error = word1_error + 1
            word1_error_weighted = word1_error_weighted + three_counts[i, count]
        }
        if(estimated_word2[1,word] != three_counts[i, third_word]) {
            word2_error = word2_error + 1
            word2_error_weighted = word2_error_weighted + three_counts[i, count]
        }
        if(estimated_word3[1,word] != three_counts[i, third_word]) {
            word3_error = word3_error + 1
            word3_error_weighted = word3_error_weighted + three_counts[i, count]
        }
        # Calculate errors based any guess in the list
        if(!findWord(estimated_word1, three_counts[i, third_word])) {
            word1_error_any = word1_error_any + 1
            word1_error_any_weighted = word1_any_error_weighted + three_counts[i, count]
        }
        if(!findWord(estimated_word2, three_counts[i, third_word])) {
            word2_error_any = word2_error_any + 1
            word2_error_any_weighted = word2_any_error_weighted + three_counts[i, count]
        }
        if(!findWord(estimated_word3, three_counts[i, third_word])) {
            word3_error_any = word3_error_any + 1
            word3_error_any_weighted = word3_any_error_weighted + three_counts[i, count]
        }
    }
    print(paste("Word 1 Model errors for first guess: ", word1_error, " (", word1_error/three_counts.len(),") and ", word1_error_weighted, " (",word1_error_weighted/three_sum,")"))
    print(paste("Word 2 Model errors for first guess: ", word2_error, " (", word2_error/three_counts.len(),") and ", word2_error_weighted, " (",word2_error_weighted/three_sum,")"))
    print(paste("Word 3 Model errors for first guess: ", word3_error, " (", word3_error/three_counts.len(),") and ", word3_error_weighted, " (",word3_error_weighted/three_sum,")"))

    print(paste("Word 1 Model errors for any guess: ", word1_error_any, " (", word1_error_any/three_counts.len(),") and ", word1_error_any_weighted, " (",word1_error_any_weighted/three_sum,")"))
    print(paste("Word 2 Model errors for any guess: ", word2_any_error, " (", word2_any_error/three_counts.len(),") and ", word2_any_error_weighted, " (",word2_any_error_weighted/three_sum,")"))
    print(paste("Word 3 Model errors for any guess: ", word3_any_error, " (", word3_any_error/three_counts.len(),") and ", word3_any_error_weighted, " (",word3_any_error_weighted/three_sum,")"))
}