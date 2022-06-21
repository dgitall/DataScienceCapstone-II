# Create the markov chain models to use for predictive text. 
# A generic markov chain model class is created to use for each of
# the following models.

# Model 1: Given two words, what is the likelihood of the possible next words. This will
# use the 3-gram distribution matrix as an input and store only the top n
# predictions

# Model 2: Given one word, what is the likelihood of the possible next words. 
# This uses the 2-gram distribution matrix as an input and stores only the top n
# predictions.

# Model 3: What is the most likely word to show up next with no knowledge of
# the prior words. This uses the 1-gram distribution matrix as an input and
# stores only the top n predictions.

library(methods)
library(dplyr)
library(tidyr)
library(data.table)

markovmodel <- setRefClass("markovmodel", 
                           fields = list(f_numwords = "numeric",
                                         f_keep = "numeric",
                                         f_count_one = "data.frame",
                                         f_total_one = "numeric",
                                         f_count_two = "data.frame",
                                         f_total_two = "numeric",
                                         f_count_three = "data.frame",
                                         f_total_three = "numeric"),
                           methods = list(
                               init = function(one_gram,
                                               two_gram = NULL,
                                               three_gram = NULL, 
                                               threshold=2, 
                                               keep=4)
                               {
                                   f_keep <<- keep
                                   # If the model is setup to use only one-grams
                                   if((is.null(three_gram)) && is.null(two_gram)) {
                                       print("MODEL 1")
                                       f_numwords <<- 0
                                       # Store the one-gram in the field and
                                       # change the column name, sort with
                                       # highest first, and calculate the total
                                       # number of words.
                                       f_count_one <<- data.frame(one_gram)
                                       colnames(f_count_one)[1] <<-
                                           "count"
                                       f_count_one <<-
                                           f_count_one %>% arrange(desc(count))
                                       f_total_one <<-
                                           sum(f_count_one$count)
                                       f_count_one$first_word <<- row.names(f_count_one)
                                       # For this one we want to calculate the probabilities and
                                       # trim down to only the number specified
                                       f_count_one <<- calc_dist(f_count_one, f_total_one)
                                       f_count_one <<- head(f_count_one, f_keep*2)
                                       print(paste("1-gram column names: ",colnames(f_count_one)))
                                       print("1-gram head: ")
                                       print(head(f_count_one))
                                       
                                       # Store empty dataframes for the other n-grams
                                       f_count_two <<- data.frame()
                                       f_total_two <<- 0
                                       f_count_three <<- data.frame()
                                       f_total_three <<- 0
                                       
                                       # Throw out those with fewer occurrences than the threshold
                                       f_count_one <<- data.frame(f_count_one[f_count_one$"count" >= threshold,])
                                   }
                                   # If the model is setup to use only the 1- and 2-grams
                                   else if((is.null(three_gram)) && !is.null(two_gram)) {
                                       print("MODEL 2")
                                       f_numwords <<- 1
                                       # Store the one-gram in the field and
                                       # change the column name, sort with
                                       # highest first, and calculate the total
                                       # number of words.
                                       tryCatch({
                                           f_count_one <<- data.frame(one_gram)
                                           colnames(f_count_one)[1] <<-
                                               "count"
                                           f_count_one <<-
                                               f_count_one %>% arrange(desc(count))
                                           f_total_one <<-
                                               sum(f_count_one$count)
                                           f_count_one$first_word <<- row.names(f_count_one)
                                           # For this one we want to calculate the probabilities and
                                           # trim down to only the number specified plus some extra if needed
                                           f_count_one <<- calc_dist(f_count_one, f_total_one)
                                           f_count_one <<- head(f_count_one, f_keep*2)
                                           print(paste("1-gram column names: ",colnames(f_count_one)))
                                           print("1-gram head: ")
                                           print(head(f_count_one))
                                       },
                                       # warning = function(warn) {
                                       #     print(paste("MY WARNING: ", warn))
                                       # },
                                       error = function(err) {
                                           print(paste("MY ERROR: ", err))
                                       },
                                       finally = function(f) {
                                           print(paste("f: ", f))
                                       })

                                       # Store the 2-gram in the field and
                                       # change the column name, and calculate
                                       # the total number of words.
                                       tryCatch({
                                           
                                           f_count_two <<- 
                                               data.frame(two_gram)
                                           colnames(f_count_two)[1] <<-
                                               "count"
                                           f_total_two <<- 
                                               sum(f_count_two$count)
                                           f_count_two$two_gram <<- row.names(f_count_two)
                                           f_count_two <<-  separate(f_count_two, "two_gram", c('first_word', 'second_word'), sep='_')
                                           print(paste("2-gram column names: ", colnames(f_count_two)))
                                           print("2-gram head: ")
                                           print(head(f_count_two))
                                       },
                                       # warning = function(warn) {
                                       #     print(paste("MY WARNING: ", warn))
                                       # },
                                       error = function(err) {
                                           print(paste("MY ERROR: ", err))
                                       },
                                       finally = function(f) {
                                           print(paste("f: ", f))
                                       })

                                       # Store empty dataframes for the other n-grams
                                       f_count_three <<- data.frame()
                                       f_total_three <<- 0

                                       # keep only the most frequent words
                                       f_count_one <<- data.frame(f_count_one[f_count_one$"count" >= threshold,])
                                       f_count_two <<- data.frame(f_count_two[f_count_two$"count" >= threshold,])
                                   }
                                   # WHen the model is setup to use 1-, 2-, and 3-grams
                                   else if((!is.null(three_gram)) && !is.null(two_gram)) {
                                       print("MODEL 3")
                                       # Store the one-gram in the field and
                                       # change the column name, sort with
                                       # highest first, and calculate the total
                                       # number of words.
                                       tryCatch({
                                           f_count_one <<- data.frame(one_gram)
                                           colnames(f_count_one)[1] <<-
                                               "count"
                                           f_count_one <<-
                                               f_count_one %>% arrange(desc(count))
                                           f_total_one <<-
                                               sum(f_count_one$count)
                                           f_count_one$first_word <<- row.names(f_count_one)
                                           # For this one we want to calculate the probabilities and
                                           # trim down to only the number specified plus some extra if needed
                                           f_count_one <<- calc_dist(f_count_one, f_total_one)
                                           f_count_one <<- head(f_count_one, f_keep*2)
                                           print(paste("1-gram column names: ",colnames(f_count_one)))
                                           print("1-gram head: ")
                                           print(head(f_count_one))
                                       },
                                       # warning = function(warn) {
                                       #     print(paste("MY WARNING: ", warn))
                                       # },
                                       error = function(err) {
                                           print(paste("MY ERROR: ", err))
                                       },
                                       finally = function(f) {
                                           print(paste("f: ", f))
                                       })
                                       
                                       # Store the 2-gram in the field and
                                       # change the column name, and calculate
                                       # the total number of words.
                                       tryCatch({
                                           f_count_two <<- 
                                               data.frame(two_gram)
                                           colnames(f_count_two)[1] <<-
                                               "count"
                                           f_total_two <<- 
                                               sum(f_count_two$count)
                                           f_count_two$two_gram <<- row.names(f_count_two)
                                           f_count_two <<-  separate(f_count_two, "two_gram", c('first_word', 'second_word'), sep='_')
                                           print(paste("2-gram column names: ", colnames(f_count_two)))
                                           print("2-gram head: ")
                                           print(head(f_count_two))
                                           print(sum(is.na(f_count_two$second_word)))
                                       },
                                       # warning = function(warn) {
                                       #     print(paste("MY WARNING: ", warn))
                                       # },
                                       error = function(err) {
                                           print(paste("MY ERROR: ", err))
                                       },
                                       finally = function(f) {
                                           print(paste("f: ", f))
                                       })

                                       # Store the 3-gram in the field and
                                       # change the column name, and calculate
                                       # the total number of words.
                                       tryCatch({
                                           f_count_three <<- 
                                               data.frame(three_gram)
                                           colnames(f_count_three)[1] <<-
                                               "count"
                                           f_total_three <<- 
                                               sum(f_count_three$count)
                                           f_count_three$three_gram <<- row.names(f_count_three)
                                           f_count_three <<-  separate(f_count_three, "three_gram", c('first_word', 'second_word', 'third_word'), sep='_')
                                           print(paste("three_gram column names: ", colnames(f_count_three)))
                                           print("three_gram head: ")
                                           print(head(f_count_three))
                                       },
                                       # warning = function(warn) {
                                       #     print(paste("MY WARNING: ", warn))
                                       # },
                                       error = function(err) {
                                           print(paste("MY ERROR: ", err))
                                       },
                                       finally = function(f) {
                                           print(paste("f: ", f))
                                       })

                                       # keep only the most frequent words
                                       tryCatch({
                                           f_count_one <<-
                                               data.frame(f_count_one[f_count_one$"count" >= threshold, ])
                                           f_count_two <<-
                                               data.frame(f_count_two[f_count_two$"count" >= threshold, ])
                                           f_count_three <<-
                                               data.frame(f_count_three[f_count_three$"count" >= threshold, ])
                                       },
                                       warning = function(warn) {
                                           print(paste("MY WARNING: ", warn))
                                       },
                                       error = function(err) {
                                           print(paste("MY ERROR: ", err))
                                       },
                                       finally = function(f) {
                                           print(paste("f: ", f))
                                       })

                                   }           
                                   else
                                       errorCondition("Invalid combination of parameters for one_gram, two_gram, and three_gram:", one_gram, two_gram, three_gram)
                               },
                               ## When using zero words, we return the probability 
                               ## matrix from the 1-gram word count matrix
                               calc_dist = function(word_count, words_total)
                               {
                                   dist <- word_count
                                   # print(summary(dist))
                                   colnames(dist)[1] <-
                                       "count"
                                   # print(summary(dist))
                                   # print(paste("Dist colnames: ", colnames(dist)))
                                   # print(head(f_count_one))
                                   # print(paste("f_count_one colnames: ", colnames(f_count_one)))
                                   # print(paste("Number of words: ", words_total))
                                   # print(paste("Number of NAs: ", sum(is.na(dist$count))))
                                   tryCatch({
                                       dist <-
                                           dist %>% arrange(desc(dist$count))

                                       # Add a probability column and return
                                       dist <-
                                           dist %>% mutate(prob = dist$count / words_total)

                                   },  
                                   warning = function(warn)
                                   {
                                       print(paste("MY WARNING: ", warn))
                                   },
                                   error = function(err) {
                                       print(paste("MY ERROR: ", err))
                                   },
                                   finally = function(f) {
                                       print(paste("f: ", f))
                                   })
                                   return(dist)

                               },
                               predict = function(word1 = NULL, word2 = NULL)
                               {
                                   predicted_list <- data.frame()
                                   # print(paste("PREDICTION FOR MODEL", f_numwords))
                                   
                                   if (f_numwords == 0)
                                   {
                                       predicted_list <- predict_1(word1)
                                   }
                                   else if (f_numwords == 1)
                                   {
                                       predicted_list <- predict_2(word1)
                                   }
                                   else if (f_numwords == 2)
                                   {
                                       predicted_list <- predict_3(word1, word2)
                                   }
                                   else
                                   {
                                       errorCondition("Predict: No valid parameters passed")
                                   }
                                   
                                   return(predicted_list)
                                   
                               },
                               
                               ## Prediction using only the one-gram list of
                               ## words. Regardless of the word passed in, the
                               ## prediction is based on the overall probability
                               ## of words appearing
                               predict_1 = function(word)
                               {
                                   # print("PREDICT ONE")
                                   # print(summary(f_count_one))
                                   # print(paste("f_count_one colnames: ", colnames(f_count_one)))
                                   # print(paste("Number of words: ", f_total_one))
                                   # dist <- calc_dist(f_count_one, f_total_one)
                                   dist <- rename(f_count_one, word = first_word)
                                   dist <- subset(dist, select = c(word, prob))
                                   # keep only the most frequent words
                                   dist <- head(dist, n=f_keep)
                                   return(dist)
                               },
                               ## Prediction using only the 1-gram and 2-gram
                               ## list of words. This uses a two step process.
                               ## First, use Bayes to calculate the probability
                               ## of different words showing up after the one
                               ## passed in This is count_of_word_combination /
                               ## count_of_word_on_own. Use just the top f_keep.
                               ## Second get the top f_keep words on their own
                               ## and add to the list. Sort by the probabilities
                               ## and return the top f_keep.
                               predict_2 = function(word)
                               {
                                   # print("PREDICT TWO")
                                   # print(paste("Word to find: ", word))
                                   tryCatch({
                                       # Get the list of 2-grams that start with the word passed in
                                       count_2 <- filter(f_count_two, first_word == word)
                                       # all_filters <- list(first_word = c(word))
                                       # cj_filter <- do.call(CJ, all_filters)
                                       
                                       # note you could avoid this `do.call` line by
                                       # cj_filter <- CJ(first_word = c(word))
                                       # count_2 <- data.table(f_count_two)
                                       # setkeyv(count_2, names(cj_filter))
                                       # 
                                       # count_2 <- count_2[cj_filter]
                                       # dt <- as.data.table(f_count_two)
                                       # count_2 <- dt[first_word == word]
                                       # print(paste("count_2 colnames: ", colnames(count_2)))
                                       # print("count_2 Head: ")
                                       # print(head(count_2))
                                       count_word_first <- sum(count_2$count)
                                       # print(paste("Count of word pairs: ", count_word_first))
                                       count_22 <- calc_dist(count_2, count_word_first)
                                       # print("count_22 Head: ")
                                       # print(head(count_22))
                                       
                                       # Merge this with f_counts_one to get the counts for the second word
                                       # count_1 <- rename(f_count_one, second_word = first_word)
                                       # df = merge(x = count_22, y = count_1, by = "second_word")
                                       # print("df Head: ")
                                       # print(head(df))
                                       # word_prob <- f_count_one[f_count_one$first_word==word,]$count / f_total_one
                                       # print(paste("Prob of ",word," occuring is ", word_prob))
                                       # # Calculate the Bayesian probabilities
                                       # df <- df %>% mutate(prob_two = prob*word_prob)
                                       # df <-
                                       #     df %>% arrange(desc(df$prob_two))
                                       
                                       # create distribution list with just the most likely second_word selections
                                       dist <- count_22 %>% select(c(second_word, prob))
                                       dist <- rename(dist, word = second_word)
                                       dist <- head(dist, n=f_keep*2)
      
                                       # Get the most likely words based on the single word choice
                                       # count_11 <- calc_dist(f_count_one, f_total_one)
                                       # count_11 <- count_11 %>% select(c(first_word, prob))
                                       count_11 <- rename(f_count_one, word = first_word)
                                       count_11 <- subset(count_11, select = c(word, prob))
                                       # count_11 <- head(count_11, n=f_keep)
                                       # print("count_11 Head: ")
                                       # print(count_11)
                                       
                                       dist <- rbind(dist,count_11)
                                       dist <-
                                           dist %>% arrange(desc(dist$prob)) %>% distinct(word, .keep_all = TRUE)
                                       # print("dist: ")
                                       # print(dist)
                                       dist <- head(dist, n=f_keep)
                                       
                                       return(dist)
                                       
                                   },
                                   warning = function(warn)
                                   {
                                       print(paste("MY WARNING: ", warn))
                                   },
                                   error = function(err) {
                                       print(paste("MY ERROR: ", err))
                                   },
                                   finally = function(f) {
                                       print(paste("f: ", f))
                                   })
                                   return(dist)
                               },
                               predict_3 = function(word1, word2)
                               {
                                   # print("PREDICT THREE")
                                   # print(paste("Words to find: ", word1, " and ", word2))
                                   tryCatch({
                                       # Get the list of 3-grams that start with the two words passed in
                                       count_3 <- filter(f_count_three, first_word == word1, second_word == word2)
                                       # all_filters <- list(first_word = word, second_word = word2)
                                       # cj_filter <- do.call(CJ, all_filters)
                                       # DT <- data.table(f_count_two)
                                       # setkeyv(DT, names(cj_filter))
                                       # cj_filter <- CJ(first_word = word1, second_word = word2)
                                       # count_3 <- data.table(f_count_three)
                                       # setkeyv(count_3, names(cj_filter))
                                       # count_3 <- count_3[cj_filter]
                                       # dt <- as.data.table(f_count_three)
                                       # count_3 <- dt[(first_word == word2) && (second_word == word1)]
                                       # print(paste("count_3 colnames: ", colnames(count_3)))
                                       # print("count_3 Head: ")
                                       # print(head(count_3))
                                       count_word_first <- sum(count_3$count)
                                       # print(paste("Count of word pairs: ", count_word_first))
                                       count_33 <- calc_dist(count_3, count_word_first)
                                       # print("count_33 Head: ")
                                       # print(head(count_33))
                                       
                                       # create distribution list with just the most likely second_word selections
                                       dist <- count_33 %>% select(c(third_word, prob))
                                       dist <- rename(dist, word = third_word)
                                       dist <- head(dist, n=f_keep*2)
                                       
                                       # Get the list of 2-grams that start with the second word passed in
                                       count_2 <- filter(f_count_two, first_word == word2)
                                       # cj_filter <- CJ(first_word = c(word2))
                                       # count_2 <- data.table(f_count_two)
                                       # setkeyv(count_2, names(cj_filter))
                                       # 
                                       # count_2 <- count_2[cj_filter]
                                       # dt <- as.data.table(f_count_two)
                                       # count_2 <- dt[first_word == word2]
                                       # print(paste("count_2 colnames: ", colnames(count_2)))
                                       # print("count_2 Head: ")
                                       # print(head(count_2))
                                       count_word_first <- sum(count_2$count)
                                       # print(paste("Count of word pairs: ", count_word_first))
                                       count_22 <- calc_dist(count_2, count_word_first)
                                       # print("count_22 Head: ")
                                       # print(head(count_22))
                                       
                                       # create distribution list with just the most likely second_word selections
                                       count_22 <- count_22 %>% select(c(second_word, prob))
                                       count_22 <- rename(count_22, word = second_word)
                                       count_22 <- head(count_22, n=f_keep*2)
                                       dist <- rbind(dist,count_22)
                                       
                                       # Get the most likely words based on the single word choice
                                       # count_11 <- calc_dist(f_count_one, f_total_one)
                                       # count_11 <- count_11 %>% select(c(first_word, prob))
                                       count_11 <- rename(f_count_one, word = first_word)
                                       count_11 <- subset(count_11, select = c(word, prob))
                                       # count_11 <- head(count_11, n=f_keep)
                                       # print("count_11 Head: ")
                                       # print(count_11)
                                       
                                       dist <- rbind(dist,count_11)
                                       dist <-
                                           dist %>% arrange(desc(dist$prob)) %>% distinct(word, .keep_all = TRUE)
                                       # print("dist: ")
                                       # print(dist)
                                       dist <- head(dist, n=f_keep)
                                       
                                       return(dist)
                                       
                                   },
                                   warning = function(warn)
                                   {
                                       print(paste("MY WARNING: ", warn))
                                   },
                                   error = function(err) {
                                       print(paste("MY ERROR: ", err))
                                   },
                                   finally = function(f) {
                                       print(paste("f: ", f))
                                   })
                                   return(dist)
                               },
                               
                               
                               get_numwords = function()
                               {
                                   f_numwords
                               },
                               set_numwords = function()
                               {
    
                               }
                               
                           ))

init_models <- function() {
    
    load(file="Data\\final\\en_US\\totals_onegram.RData") 
    model1 <- markovmodel()
    model1$init(one_gram = totals_onegram, threshold = 0, keep = 5)
    # print(model1)
    
    load(file="Data\\final\\en_US\\totals_2gram.RData") 

    model2 <- markovmodel()
    model2$init(one_gram = totals_onegram, 
                two_gram = totals_2gram, 
                threshold = 0, keep = 5)
    # print(model2)
    
    load(file="Data\\final\\en_US\\totals_3gram.RData") 
    model3 <- markovmodel()
    model3$init(one_gram = totals_onegram, 
                two_gram = totals_2gram, 
                three_gram = totals_3gram,
                threshold = 0, keep = 5)
    # print(model3)
    
    model_list <- list("model_one" = model1, "model_two" = model2, "model_three" = model3)
    return(model_list)
}

# model_except <- markovmodel()
# model_except$init(one_gram = totals_onegram, 
#                   three_gram = totals_3gram, 
#                   threshold = 2, keep = 4)

main <- function(){
    library(microbenchmark)
    library(ggplot2)
    
    model_list <- init_models()
   # print(model_list)
    
    results <- microbenchmark(
        model_list$model_one$predict(),
        model_list$model_two$predict('where'),
        model_list$model_three$predict_3('where', 'i'),
        times = 50)
    
    print(results)
    print(autoplot(results))

    # predicted_list <- model_list$model_one$predict()
    # print("Predicted List for Model 1:")
    # print(predicted_list)
    # 
    # predicted_list <- model_list$model_two$predict('where')
    # print("Predicted List for Model 2:")
    # print(predicted_list)
    # 
    # predicted_list <- model_list$model_three$predict_3('where', 'i')
    # print("Predicted List for Model 3:")
    # print(predicted_list)
    
    save(model_list, file="Data\\final\\en_US\\model_list.RData")
}
