# Create the markov chain models to use for predictive text.
# A generic markov chain model class is created to use for each of
# the following models.

# Model 1: What is the most likely word to show up next with no knowledge of
# the prior words. This uses the 1-gram distribution matrix as an input and
# stores only the top n predictions.

# Model 2: Given one word, what is the likelihood of the possible next words.
# This uses the 1- and 2-gram distribution matrices as an input and stores only the top n
# predictions.

# Model 3: Given two words, what is the likelihood of the possible next words. This will
# use the 1-, 2-, and 3-gram distribution matrices as an input and store only the top n
# predictions

# Model 5: Given four words, what is the likelihood of the possible next words. This will
# use the 1-, 2-, 3-, 4-, and 5-gram distribution matrices as an input and store only the top n
# predictions

require(methods)
require(dplyr)
require(tidyr)
require(data.table)
require(futile.logger)
require(rlang)


markovmodel <- setRefClass(
    "markovmodel",
    fields = list(
        f_Numwords = "numeric",
        f_keep = "numeric",
        f_count_one = "data.frame",
        f_total_one = "numeric",
        f_count_two = "data.frame",
        f_total_two = "numeric",
        f_count_three = "data.frame",
        f_total_three = "numeric",
        f_count_four = "data.frame",
        f_total_four = "numeric",
        f_count_five = "data.frame",
        f_total_five = "numeric"
    ),
    methods = list(
        init = function(one_gram,
                        two_gram = NULL,
                        three_gram = NULL,
                        four_gram = NULL,
                        five_gram = NULL,
                        threshold = 2,
                        keep = 4) {
            f_keep <<- keep
            # If the model is setup to use only one-grams
            if (is.null(three_gram) &&
                is.null(two_gram) &&
                is.null(four_gram) &&
                is.null(five_gram)) {
                flog.info("MODEL 1", name = 'markov.log')
                f_Numwords <<- 0
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
                f_count_one <<- head(f_count_one, f_keep * 2)
                flog.debug(paste("1-gram column names: ", colnames(f_count_one)), name = 'markov.log')
                flog.debug("1-gram head: ", name = 'markov.log')
                flog.debug(head(f_count_one), name = 'markov.log')
                
                # Store empty dataframes for the other n-grams
                f_count_two <<- data.frame()
                f_total_two <<- 0
                f_count_three <<- data.frame()
                f_total_three <<- 0
                
                # Throw out those with fewer occurrences than the threshold
                f_count_one <<-
                    data.frame(f_count_one[f_count_one$"count" >= threshold, ])
            }
            # If the model is setup to use only the 1- and 2-grams
            else if (is.null(three_gram) &&
                     !is.null(two_gram) &&
                     is.null(four_gram) &&
                     is.null(five_gram)) {
                flog.info("MODEL 2", name = 'markov.log')
                f_Numwords <<- 1
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
                    f_count_one$first_word <<-
                        row.names(f_count_one)
                    # For this one we want to calculate the probabilities and
                    # trim down to only the number specified plus some extra if needed
                    f_count_one <<-
                        calc_dist(f_count_one, f_total_one)
                    f_count_one <<- head(f_count_one, f_keep * 2)
                    flog.debug(paste("1-gram column names: ", colnames(f_count_one)), name = 'markov.log')
                    flog.debug("1-gram head: ", name = 'markov.log')
                    flog.debug(head(f_count_one), name = 'markov.log')
                },
                warning = function(warn) {
                    flog.warn("MY WARNING: %s", warn, name = 'markov.log')
                },
                error = function(err) {
                    flog.error(paste("MY ERROR: ", err), name = 'markov.log')
                },
                finally = function(f) {
                    flog.trace(paste("f: ", f), name = 'markov.log')
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
                    f_count_two <<-
                        separate(
                            f_count_two,
                            col = "two_gram",
                            into = c("first_word", "second_word", "extra"),
                            sep = "_",
                            extra = "merge",
                            fill = "right"
                        )
                    flog.debug("2-gram column names: %s",
                               colnames(f_count_two),
                               name = 'markov.log')
                    flog.debug("2-gram head: ", name = 'markov.log')
                    flog.debug(head(f_count_two), name = 'markov.log')
                    flog.debug("2-gram extras: ", name = 'markov.log')
                    extras <-f_count_two[,!is.na(f_count_two$extra)]
                    flog.debug("count of extras: %s", sum(!is.na(f_count_two$extra)), name = 'markov.log')
                    flog.debug(head(extras), name = 'markov.log')
                },
                warning = function(warn) {
                    flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
                },
                error = function(err) {
                    flog.error(paste("MY ERROR: ", err), name = 'markov.log')
                },
                finally = function(f) {
                    flog.trace(paste("f: ", f), name = 'markov.log')
                })
                
                # Store empty dataframes for the other n-grams
                f_count_three <<- data.frame()
                f_total_three <<- 0
                
                # keep only the most frequent words
                f_count_one <<-
                    data.frame(f_count_one[f_count_one$"count" >= threshold, ])
                f_count_two <<-
                    data.frame(f_count_two[f_count_two$"count" >= threshold, ])
            }
            # WHen the model is setup to use 1-, 2-, and 3-grams
            else if (!is.null(three_gram) &&
                     !is.null(two_gram) &&
                     is.null(four_gram) &&
                     is.null(five_gram)) {
                f_Numwords <<- 2
                flog.info("MODEL 3", name = 'markov.log')
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
                    f_count_one$first_word <<-
                        row.names(f_count_one)
                    # For this one we want to calculate the probabilities and
                    # trim down to only the number specified plus some extra if needed
                    f_count_one <<-
                        calc_dist(f_count_one, f_total_one)
                    f_count_one <<- head(f_count_one, f_keep * 2)
                    flog.debug(paste("1-gram column names: ", colnames(f_count_one)), name = 'markov.log')
                    flog.debug("1-gram head: ", name = 'markov.log')
                    flog.debug(head(f_count_one), name = 'markov.log')
                },
                warning = function(warn) {
                    flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
                },
                error = function(err) {
                    flog.error(paste("MY ERROR: ", err), name = 'markov.log')
                },
                finally = function(f) {
                    flog.trace(paste("f: ", f), name = 'markov.log')
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
                    f_count_two <<-
                        separate(f_count_two,
                                 "two_gram",
                                 c("first_word", "second_word", "extra"),
                                 sep = "_",
                                 extra = "merge",
                                 fill = "right")
                    flog.debug("2-gram column names: %s",
                               colnames(f_count_two),
                               name = 'markov.log')
                    flog.debug("2-gram head: ", name = 'markov.log')
                    flog.debug(head(f_count_two), name = 'markov.log')
                    flog.debug("2-gram extras: ", name = 'markov.log')
                    extras <-f_count_two[,!is.na(f_count_two$extra)]
                    flog.debug("count of extras: %s", sum(!is.na(f_count_two$extra)), name = 'markov.log')
                    flog.debug(head(extras), name = 'markov.log')
                },
                warning = function(warn) {
                    flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
                },
                error = function(err) {
                    flog.error(paste("MY ERROR: ", err), name = 'markov.log')
                },
                finally = function(f) {
                    flog.trace(paste("f: ", f), name = 'markov.log')
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
                    f_count_three$three_gram <<-
                        row.names(f_count_three)
                    f_count_three <<-
                        separate(
                            f_count_three,
                            "three_gram",
                            c("first_word", "second_word", "third_word", "extra"),
                            sep = "_",
                            extra = "merge",
                            fill = "right"
                        )
                    flog.debug("3-gram column names: %s",
                               colnames(f_count_three),
                               name = 'markov.log')
                    flog.debug("3-gram head: ", name = 'markov.log')
                    flog.debug(head(f_count_three), name = 'markov.log')
                    flog.debug("3-gram extras: ", name = 'markov.log')
                    extras <-f_count_three[,!is.na(f_count_three$extra)]
                    flog.debug("count of extras: %s", sum(!is.na(f_count_three$extra)), name = 'markov.log')
                    flog.debug(head(extras), name = 'markov.log')
                },
                warning = function(warn) {
                    flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
                },
                error = function(err) {
                    flog.error(paste("MY ERROR: ", err), name = 'markov.log')
                },
                finally = function(f) {
                    flog.trace(paste("f: ", f), name = 'markov.log')
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
                    flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
                },
                error = function(err) {
                    flog.error(paste("MY ERROR: ", err), name = 'markov.log')
                },
                finally = function(f) {
                    flog.trace(paste("f: ", f), name = 'markov.log')
                })
            }
            # WHen the model is setup to use 1- through 5-grams
            else if (!is.null(three_gram) &&
                     !is.null(two_gram) &&
                     !is.null(four_gram) &&
                     !is.null(five_gram)) {
                f_Numwords <<- 4
                flog.info("MODEL 5", name = 'markov.log')
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
                    f_count_one$first_word <<-
                        row.names(f_count_one)
                    # For this one we want to calculate the probabilities and
                    # trim down to only the number specified plus some extra if needed
                    f_count_one <<-
                        calc_dist(f_count_one, f_total_one)
                    f_count_one <<- head(f_count_one, f_keep * 2)
                    flog.debug(paste("1-gram column names: ", colnames(f_count_one)), name = 'markov.log')
                    flog.debug("1-gram head: ", name = 'markov.log')
                    flog.debug(head(f_count_one), name = 'markov.log')
                },
                warning = function(warn) {
                    flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
                },
                error = function(err) {
                    flog.error(paste("MY ERROR: ", err), name = 'markov.log')
                },
                finally = function(f) {
                    flog.trace(paste("f: ", f), name = 'markov.log')
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
                    f_count_two <<-
                        separate(f_count_two,
                                 "two_gram",
                                 c("first_word", "second_word", "extra"),
                                 sep = "_",
                                 extra = "merge",
                                 fill = "right")
                    flog.debug("2-gram column names: %s",
                               colnames(f_count_two),
                               name = 'markov.log')
                    flog.debug("2-gram head: ", name = 'markov.log')
                    flog.debug(head(f_count_two), name = 'markov.log')
                    flog.debug("2-gram extras: ", name = 'markov.log')
                    extras <-f_count_two[,!is.na(f_count_two$extra)]
                    flog.debug("count of extras: %s", sum(!is.na(f_count_three$extra)), name = 'markov.log')
                    flog.debug(head(extras), name = 'markov.log')
                },
                warning = function(warn) {
                    flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
                },
                error = function(err) {
                    flog.error(paste("MY ERROR: ", err), name = 'markov.log')
                },
                finally = function(f) {
                    flog.trace(paste("f: ", f), name = 'markov.log')
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
                    f_count_three$three_gram <<-
                        row.names(f_count_three)
                    f_count_three <<- separate(
                        f_count_three,
                        "three_gram",
                        c("first_word",
                          "second_word",
                          "third_word", "extra"),
                        sep = "_",
                        extra = "merge",
                        fill = "right"
                    )
                    flog.debug("3-gram column names: %s",
                               colnames(f_count_three),
                               name = 'markov.log')
                    flog.debug("3-gram head: ", name = 'markov.log')
                    flog.debug(head(f_count_three), name = 'markov.log')
                    flog.debug("3-gram extras: ", name = 'markov.log')
                    extras <-f_count_three[,!is.na(f_count_three$extra)]
                    flog.debug("count of extras: %s", sum(!is.na(f_count_three$extra)), name = 'markov.log')
                    flog.debug(head(extras), name = 'markov.log')
                },
                warning = function(warn) {
                    flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
                },
                error = function(err) {
                    flog.error(paste("MY ERROR: ", err), name = 'markov.log')
                },
                finally = function(f) {
                    flog.trace(paste("f: ", f), name = 'markov.log')
                })
                
                # Store the 4-gram in the field and
                # change the column name, and calculate
                # the total number of words.
                tryCatch({
                    f_count_four <<-
                        data.frame(four_gram)
                    colnames(f_count_four)[1] <<-
                        "count"
                    f_total_four <<-
                        sum(f_count_four$count)
                    f_count_four$four_gram <<-
                        row.names(f_count_four)
                    f_count_four <<-
                        separate(
                            f_count_four,
                            "four_gram",
                            c(
                                "first_word",
                                "second_word",
                                "third_word",
                                "fourth_word"
                            ),
                            sep = "_"
                        )
                    flog.debug(paste("four_gram column names: ", colnames(f_count_four)), name = 'markov.log')
                    flog.debug("four_gram head: ", name = 'markov.log')
                    flog.debug(head(f_count_four), name = 'markov.log')
                },
                warning = function(warn) {
                    flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
                },
                error = function(err) {
                    flog.error(paste("MY ERROR: ", err), name = 'markov.log')
                },
                finally = function(f) {
                    flog.trace(paste("f: ", f), name = 'markov.log')
                })
                
                # Store the 5-gram in the field and
                # change the column name, and calculate
                # the total number of words.
                tryCatch({
                    f_count_five <<-
                        data.frame(five_gram)
                    colnames(f_count_five)[1] <<-
                        "count"
                    f_total_five <<-
                        sum(f_count_five$count)
                    f_count_five$five_gram <<-
                        row.names(f_count_five)
                    f_count_five <<-
                        separate(
                            f_count_five,
                            "five_gram",
                            c(
                                "first_word",
                                "second_word",
                                "third_word",
                                "fourth_word",
                                "fifth_word"
                            ),
                            sep = "_"
                        )
                    flog.debug(paste("five_gram column names: ", colnames(f_count_five)), name = 'markov.log')
                    flog.debug("five_gram head: ", name = 'markov.log')
                    flog.debug(head(f_count_five), name = 'markov.log')
                },
                warning = function(warn) {
                    flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
                },
                error = function(err) {
                    flog.error(paste("MY ERROR: ", err), name = 'markov.log')
                },
                finally = function(f) {
                    flog.trace(paste("f: ", f), name = 'markov.log')
                })
                
                # keep only the most frequent words
                tryCatch({
                    f_count_one <<-
                        data.frame(f_count_one[f_count_one$"count" >= threshold, ])
                    f_count_two <<-
                        data.frame(f_count_two[f_count_two$"count" >= threshold, ])
                    f_count_three <<-
                        data.frame(f_count_three[f_count_three$"count" >= threshold, ])
                    f_count_four <<-
                        data.frame(f_count_four[f_count_four$"count" >= threshold, ])
                    f_count_five <<-
                        data.frame(f_count_five[f_count_five$"count" >= threshold, ])
                },
                warning = function(warn) {
                    flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
                },
                error = function(err) {
                    flog.error(paste("MY ERROR: ", err), name = 'markov.log')
                },
                finally = function(f) {
                    flog.trace(paste("f: ", f), name = 'markov.log')
                })
            } else {
                errorCondition(
                    "Invalid combination of parameters for one_gram, two_gram, and three_gram:",
                    one_gram,
                    two_gram,
                    three_gram
                )
            }
        },
        
        # Calculate the probability for each entry in the word count create_matrix
        # This will work for any of the n-grams input as long as the first column contains
        # the count of occurances. We pass in the total which is calculated before the threshold
        # so the probability is against all n-grams found, not just those surviving the thresholding
        calc_dist = function(word_count, words_total) {
            dist <- word_count
            flog.debug(summary(dist))
            colnames(dist)[1] <-
                "count"
            flog.trace(summary(dist), name = 'markov.log')
            flog.trace(paste("Dist colnames: ", colnames(dist)), name = 'markov.log')
            flog.trace(head(f_count_one), name = 'markov.log')
            flog.trace(paste("f_count_one colnames: ", colnames(f_count_one)), name = 'markov.log')
            flog.trace(paste("Number of words: ", words_total), name = 'markov.log')
            flog.trace(paste("Number of NAs: ", sum(is.na(dist$count))), name = 'markov.log')
            tryCatch({
                dist <-
                    dist %>% arrange(desc(dist$count))
                
                # Add a probability column and return
                dist <-
                    dist %>% mutate(prob = dist$count / words_total)
            },
            warning = function(warn) {
                flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
            },
            error = function(err) {
                flog.error(paste("MY ERROR: ", err), name = 'markov.log')
            },
            finally = function(f) {
                flog.trace(paste("f: ", f), name = 'markov.log')
            })
            return(dist)
        },
        predict = function(word1 = NULL,
                           word2 = NULL,
                           word3 = NULL,
                           word4 = NULL) {
            predictedList <- data.frame()
            # print(paste("PREDICTION FOR MODEL", f_Numwords))
            
            if (f_Numwords == 0) {
                predictedList <- predict1(word1)
            } else if (f_Numwords == 1) {
                predictedList <- predict2(word1)
            } else if (f_Numwords == 2) {
                predictedList <- predict3(word1, word2)
            } else if (f_Numwords == 4) {
                predictedList <- predict5(word1, word2,
                                          word3, word4)
            } else {
                errorCondition("Predict: No valid parameters passed")
            }
            
            return(predictedList)
        },
        
        ## Prediction using only the one-gram list of
        ## words. Regardless of the word passed in, the
        ## prediction is based on the overall probability
        ## of words appearing
        predict1 = function(word) {
            flog.info("PREDICT ONE", name = 'markov.log')
            flog.debug(summary(f_count_one), name = 'markov.log')
            flog.debug(paste("f_count_one colnames: ", colnames(f_count_one)), name = 'markov.log')
            flog.debug(paste("Number of words: ", f_total_one), name = 'markov.log')
            # dist <- calc_dist(f_count_one, f_total_one)
            dist <- rename(f_count_one, word = first_word)
            dist <- subset(dist, select = c(word, prob))
            # keep only the most frequent words
            dist <- head(dist, n = f_keep)
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
        predict2 = function(word) {
            flog.info("PREDICT TWO", name = 'markov.log')
            flog.info(paste("Word to find: ", word), name = 'markov.log')
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
                dist <- head(dist, n = f_keep * 2)
                
                # Get the most likely words based on the single word choice
                # count_11 <- calc_dist(f_count_one, f_total_one)
                # count_11 <- count_11 %>% select(c(first_word, prob))
                count_11 <- rename(f_count_one, word = first_word)
                count_11 <- subset(count_11, select = c(word, prob))
                # count_11 <- head(count_11, n=f_keep)
                # print("count_11 Head: ")
                # print(count_11)
                
                dist <- rbind(dist, count_11)
                dist <-
                    dist %>%
                    arrange(desc(dist$prob)) %>%
                    distinct(word, .keep_all = TRUE)
                # print("dist: ")
                # print(dist)
                dist <- head(dist, n = f_keep)
                
                return(dist)
            },
            warning = function(warn) {
                flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
            },
            error = function(err) {
                flog.error(paste("MY ERROR: ", err), name = 'markov.log')
            },
            finally = function(f) {
                flog.trace(paste("f: ", f), name = 'markov.log')
            })
            return(dist)
        },
        predict3 = function(word1, word2) {
            flog.info("PREDICT THREE", name = 'markov.log')
            flog.info(paste("Words to find: ", word1, " and ", word2), name = 'markov.log')
            tryCatch({
                # Get the list of 3-grams that start with the two words passed in
                count_3 <-
                    filter(f_count_three, first_word == word1, second_word == word2)
                # print(paste("count_3 colnames: ", colnames(count_3)))
                # print("count_3 Head: ")
                # print(head(count_3))
                count_word_first <- sum(count_3$count)
                # print(paste("Count of word pairs: ", count_word_first))
                count_33 <- calc_dist(count_3, count_word_first)
                # print("count_33 Head: ")
                # print(head(count_33))
                
                dist <- count_33 %>% select(c(third_word, prob))
                dist <- rename(dist, word = third_word)
                dist <- dist %>% arrange(desc(dist$prob))
                dist <- head(dist, n = f_keep * 2)
                # print(dist)
                # Check to see if there are enough guesses to fill out the list.
                # If not, go to the list using only one prior word
                # create distribution list with just the most likely second_word selections
                count_33_len <- nrow(dist)
                if (count_33_len < f_keep) {
                    # Get the list of 2-grams that start with the second word passed in
                    count_2 <-
                        filter(f_count_two, first_word == word2)
                    # print(paste("count_2 colnames: ", colnames(count_2)))
                    # print("count_2 Head: ")
                    # print(head(count_2))
                    count_word_first <- sum(count_2$count)
                    # print(paste("Count of word pairs: ", count_word_first))
                    count_22 <- calc_dist(count_2, count_word_first)
                    # print("count_22 Head: ")
                    # print(head(count_22))
                    
                    # create distribution list with just the most likely second_word selections
                    count_22 <-
                        count_22 %>% select(c(second_word, prob))
                    # Do double the number to keep to make sure we have enough after
                    # removing the duplicates
                    count_22 <- rename(count_22, word = second_word)
                    count_22 <-
                        count_22 %>% arrange(desc(count_22$prob))
                    count_22 <- head(count_22, n = f_keep * 2)
                    # print("count_22")
                    # print(count_22)
                    # Add these to the end of the guess with two prior words
                    dist <- rbind(dist, count_22)
                    # Sort and remove any duplicates
                    dist <-
                        dist %>% distinct(word, .keep_all = TRUE)
                    # Check to see if we have enough in the list now.
                    # If not, go to the list using no prior words
                    count_22_len <- nrow(dist)
                    if (count_22_len < f_keep) {
                        # Get the most likely words based on the single word choice
                        # count_11 <- calc_dist(f_count_one, f_total_one)
                        # count_11 <- count_11 %>% select(c(first_word, prob))
                        count_11 <-
                            rename(f_count_one, word = first_word)
                        count_11 <-
                            subset(count_11, select = c(word, prob))
                        # count_11 <- head(count_11, n=f_keep)
                        # print("count_11 Head: ")
                        # print(count_11)
                        
                        dist <- rbind(dist, count_11)
                        # Sort and remove any duplicates
                        dist <-
                            dist %>% distinct(word, .keep_all = TRUE)
                    }
                }
                # print("dist: ")
                # print(dist)
                dist <- head(dist, n = f_keep)
                
                return(dist)
            },
            warning = function(warn) {
                flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
            },
            error = function(err) {
                flog.error(paste("MY ERROR: ", err), name = 'markov.log')
            },
            finally = function(f) {
                flog.trace(paste("f: ", f), name = 'markov.log')
            })
            return(dist)
        },
        predict5 = function(word1, word2, word3, word4) {
            flog.info("PREDICT FIVE", name = 'markov.log')
            flog.info(paste("Words to find: ", word1, " and ", word2), name = 'markov.log')
            tryCatch({
                # Get the list of 5-grams that start with the four words passed in
                count_5 <- filter(
                    f_count_five,
                    first_word == word1,
                    second_word == word2,
                    third_word == word3,
                    fourth_word == word4
                )
                flog.trace(paste("count_5 colnames: ", colnames(count_5)), name = 'markov.log')
                flog.trace("count_5 Head: ", name = 'markov.log')
                flog.trace(head(count_5), name = 'markov.log')
                count_word_first <- sum(count_5$count)
                flog.trace(paste("Count of word pairs: ", count_word_first), name = 'markov.log')
                count_55 <- calc_dist(count_5, count_word_first)
                flog.trace("count_55 Head: ", name = 'markov.log')
                flog.trace(head(count_55), name = 'markov.log')
                
                dist <- count_55 %>% select(c(fifth_word, prob))
                dist <- rename(dist, word = fifth_word)
                dist <- dist %>% arrange(desc(dist$prob))
                dist <- head(dist, n = f_keep * 2)
                flog.trace(dist)
                # Check to see if there are enough guesses to fill out the list.
                # If not, go to the list using only one prior word
                # create distribution list with just the most likely second_word selections
                count_55_len <- nrow(dist)
                if (count_55_len < f_keep) {
                    # Get the list of 3-grams that start with the two words passed in
                    count_4 <- filter(
                        f_count_four,
                        first_word == word2,
                        second_word == word3,
                        third_word == word4
                    )
                    # print(paste("count_3 colnames: ", colnames(count_3)))
                    # print("count_3 Head: ")
                    # print(head(count_3))
                    count_word_first <- sum(count_4$count)
                    # print(paste("Count of word pairs: ", count_word_first))
                    count_44 <- calc_dist(count_4, count_word_first)
                    # print("count_33 Head: ")
                    # print(head(count_33))
                    
                    dist <- count_4 %>% select(c(fourth_word, prob))
                    dist <- rename(dist, word = fourth_word)
                    dist <- dist %>% arrange(desc(dist$prob))
                    dist <- head(dist, n = f_keep * 2)
                    # Add these to the end of the guess with four prior words
                    dist <- rbind(dist, count_44)
                    # Sort and remove any duplicates
                    dist <-
                        dist %>% distinct(word, .keep_all = TRUE)
                    # print(dist)
                    # Check to see if there are enough guesses to fill out the list.
                    # If not, go to the list using only one prior word
                    # create distribution list with just the most likely second_word selections
                    count_44_len <- nrow(dist)
                    if (count_44_len < f_keep) {
                        # Get the list of 3-grams that start with the two words passed in
                        count_3 <-
                            filter(f_count_three,
                                   first_word == word3,
                                   second_word == word4)
                        # print(paste("count_3 colnames: ", colnames(count_3)))
                        # print("count_3 Head: ")
                        # print(head(count_3))
                        count_word_first <- sum(count_3$count)
                        # print(paste("Count of word pairs: ", count_word_first))
                        count_33 <-
                            calc_dist(count_3, count_word_first)
                        # print("count_33 Head: ")
                        # print(head(count_33))
                        
                        dist <-
                            count_33 %>% select(c(third_word, prob))
                        dist <- rename(dist, word = third_word)
                        dist <- dist %>% arrange(desc(dist$prob))
                        dist <- head(dist, n = f_keep * 2)
                        # Add these to the end of the guess with four prior words
                        dist <- rbind(dist, count_33)
                        # Sort and remove any duplicates
                        dist <-
                            dist %>% distinct(word, .keep_all = TRUE)
                        
                        # print(dist)
                        # Check to see if there are enough guesses to fill out the list.
                        # If not, go to the list using only one prior word
                        # create distribution list with just the most likely second_word selections
                        count_33_len <- nrow(dist)
                        if (count_33_len < f_keep) {
                            # Get the list of 2-grams that start with the second word passed in
                            count_2 <-
                                filter(f_count_two, first_word == word2)
                            # print(paste("count_2 colnames: ", colnames(count_2)))
                            # print("count_2 Head: ")
                            # print(head(count_2))
                            count_word_first <- sum(count_2$count)
                            # print(paste("Count of word pairs: ", count_word_first))
                            count_22 <-
                                calc_dist(count_2, count_word_first)
                            # print("count_22 Head: ")
                            # print(head(count_22))
                            
                            # create distribution list with just the most likely second_word selections
                            count_22 <-
                                count_22 %>% select(c(second_word, prob))
                            # Do double the number to keep to make sure we have enough after
                            # removing the duplicates
                            count_22 <-
                                rename(count_22, word = second_word)
                            count_22 <-
                                count_22 %>% arrange(desc(count_22$prob))
                            count_22 <-
                                head(count_22, n = f_keep * 2)
                            # print("count_22")
                            # print(count_22)
                            # Add these to the end of the guess with two prior words
                            dist <- rbind(dist, count_22)
                            # Sort and remove any duplicates
                            dist <-
                                dist %>% distinct(word, .keep_all = TRUE)
                            # Check to see if we have enough in the list now.
                            # If not, go to the list using no prior words
                            count_22_len <- nrow(dist)
                            if (count_22_len < f_keep) {
                                # Get the most likely words based on the single word choice
                                # count_11 <- calc_dist(f_count_one, f_total_one)
                                # count_11 <- count_11 %>% select(c(first_word, prob))
                                count_11 <-
                                    rename(f_count_one, word = first_word)
                                count_11 <-
                                    subset(count_11, select = c(word, prob))
                                # count_11 <- head(count_11, n=f_keep)
                                # print("count_11 Head: ")
                                # print(count_11)
                                
                                dist <- rbind(dist, count_11)
                                # Sort and remove any duplicates
                                dist <-
                                    dist %>% distinct(word, .keep_all = TRUE)
                            }
                        }
                    }
                }
                # print("dist: ")
                # print(dist)
                dist <- head(dist, n = f_keep)
                
                return(dist)
            },
            warning = function(warn) {
                flog.warn(paste("MY WARNING: ", warn), name = 'markov.log')
            },
            error = function(err) {
                flog.error(paste("MY ERROR: ", err), name = 'markov.log')
            },
            finally = function(f) {
                flog.trace(paste("f: ", f), name = 'markov.log')
            })
            return(dist)
        },
        get_numwords = function() {
            f_Numwords
        },
        set_numwords = function() {
            
        }
    )
)

init_models <- function() {
    load(file = "Data\\final\\en_US\\totals_onegram.RData")
    model1 <- markovmodel()
    model1$init(one_gram = totals_onegram,
                threshold = 0,
                keep = 5)
    # print(model1)
    
    load(file = "Data\\final\\en_US\\totals_2gram.RData")
    
    model2 <- markovmodel()
    model2$init(
        one_gram = totals_onegram,
        two_gram = totals_2gram,
        threshold = 0,
        keep = 5
    )
    # print(model2)
    
    load(file = "Data\\final\\en_US\\totals_3gram.RData")
    model3 <- markovmodel()
    model3$init(
        one_gram = totals_onegram,
        two_gram = totals_2gram,
        three_gram = totals_3gram,
        threshold = 0,
        keep = 5
    )
    
    load(file = "Data\\final\\en_US\\totals_4gram.RData")
    load(file = "Data\\final\\en_US\\totals_5gram.RData")
    model5 <- markovmodel()
    model5$init(
        one_gram = totals_onegram,
        two_gram = totals_2gram,
        three_gram = totals_3gram,
        four_gram = totals_4gram,
        five_gram = totals_5gram,
        threshold = 0,
        keep = 5
    )
    
    # print(model3)
    
    model_list <-
        list(
            "model_one" = model1,
            "model_two" = model2,
            "model_three" = model3,
            "model_five" = model5
        )
    return(model_list)
}

# model_except <- markovmodel()
# model_except$init(one_gram = totals_onegram,
#                   three_gram = totals_3gram,
#                   threshold = 2, keep = 4)

main <- function(loggingTreshold = INFO,
                 logFile = NULL) {
    library(microbenchmark)
    library(ggplot2)
    
    flog.threshold(loggingTreshold, name = 'markov.log')
    if (is.null(logFile)) {
        flog.appender(appender.console(), name = 'markov.log')
    }
    else {
        flog.appender(appender.file(logFile), name = 'markov.log')
    }
    
    model_list <- init_models()
    flog.debug(model_list, name = 'markov.log')
    
    # results <- microbenchmark(
    #     model_list$model_one$predict(),
    #     model_list$model_two$predict("case"),
    #     model_list$model_three$predict("case", "of"),
    #     model_list$model_five$predict("bring", "a", "case", "of"),
    #     times = 50
    # )
    # print(results)
    # print(autoplot(results))
    
    predictedList <- model_list$model_one$predict()
    flog.debug("Predicted List for Model 1:", name = 'markov.log')
    flog.debug(predictedList, name = 'markov.log')
    
    predictedList <- model_list$model_two$predict('where')
    flog.debug("Predicted List for Model 2:", name = 'markov.log')
    flog.debug(predictedList, name = 'markov.log')
    
    predictedList <- model_list$model_three$predict3('heavy', 'cre')
    flog.debug("Predicted List for Model 3:", name = 'markov.log')
    flog.debug(predictedList, name = 'markov.log')
    
    predictedList <-
        model_list$model_five$predict("bring", "a", "case", "of")
    flog.debug("Predicted List for Model 5:")
    flog.debug(predictedList, name = 'markov.log')
    
    save(model_list, file = "Data\\final\\en_US\\model_list5.RData")
    save(model_list$model_one, file = "Data\\final\\en_US\\model_one.RData")
    save(model_list$model_two, file = "Data\\final\\en_US\\model_two.RData")
    save(model_list$model_three, file = "Data\\final\\en_US\\model_three.RData")
    save(model_list$model_five, file = "Data\\final\\en_US\\model_five.RData")
    
    return(model_list)
}

models <- main(loggingTreshold = DEBUG, logFile = 'MarkovLog.txt')
