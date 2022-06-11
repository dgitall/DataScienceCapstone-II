library(stringi)

inputFiles <- c("Data\\final\\en_US\\en_US.twitter.txt", 
                "Data\\final\\en_US\\en_US.news.txt", 
                "Data\\final\\en_US\\en_US.blogs.txt")

# The files are very large so lets split the data up into smaller data set.
# * Development data -- A small subset used to develop our methodology (1% of data)
#       dev-training data -- for training the NLP model (0.7% of data)
#       dev-testing data -- for testing the NLP model (0.3% of data)
# * Production data -- A large subset we will use for final learning and testing (100% of data
#       prod-training data -- for training the NLP model (40%)
#       prod-testing data -- for testing the NLP model (10%)
#       prod-validation data -- large data set set aside for final validation (50%)
chanceDev <- 0.01
chanceDevTrain <- 0.7
chanceProdValid <- 0.5
chanceProdTrain <- 0.8

devTrainFile<-"Data\\final\\en_US\\dev-training.txt"
if(file.exists(devTrainFile)) unlink(devTrainFile)
file.create(devTrainFile)
conDevTrain <- file(devTrainFile, open="w")

devTestFile<-"Data\\final\\en_US\\dev-testing.txt"
if(!file.exists(devTestFile)) unlink(devTestFile)
file.create(devTestFile)
conDevTest <- file(devTestFile, open="w")

prodValidFile<-"Data\\final\\en_US\\prod-validation.txt"
if(!file.exists(prodValidFile)) unlink(prodValidFile)
file.create(prodValidFile)
conProdValid <- file(prodValidFile, open="w")

prodTrainFile<-"Data\\final\\en_US\\prod-training.txt"
if(!file.exists(prodTrainFile)) unlink(prodTrainFile)
file.create(prodTrainFile)
conProdTrain <- file(prodTrainFile, open="w")

prodTestFile<-"Data\\final\\en_US\\prod-testing.txt"
if(!file.exists(prodTestFile)) unlink(prodTestFile)
file.create(prodTestFile)
conProdTest <- file(prodTestFile, open="w")

# variables for quiz
longestLine <- data.frame(length=0, file="")
countLove <- 0
countHate <- 0

# Loop through all three input files and split them out
for(i in 1:length(inputFiles)) {
    conInput <- file(inputFiles[i], "r") 
    # Read in the file line by line
    while (TRUE) {
        line = readLines(conInput, n = 1)
        if ( length(line) == 0 ) {
            break
        }
        # Flip coin to include in the development data set
        if(rbinom(1, 1, chanceDev)==1) {
            # Flip coin if in development training set
            if(rbinom(1, 1, chanceDevTrain) == 1) {
                writeLines(line, conDevTrain)
            } 
            else {
                writeLines(line, conDevTest)
            }
        }
        # All of the data will be included in one of three production data sets
        # Flip a coin if added to validation data set
        if(rbinom(1, 1, chanceProdValid)==1) {
            writeLines(line, conProdValid)
        }
        else if(rbinom(1, 1, chanceProdTrain)==1) {
            writeLines(line, conProdTrain)
        } else {
            writeLines(line, conProdTest)
        }
        
        # Do stuff for the excercises
        # Find the longest line
        if(stri_length(line) > longestLine$length) {
            longestLine$length <- stri_length(line)
            longestLine$file <- inputFiles[i]
        }
        
        # In the twitter file, check if line contains "love" or "hate" and
        # increment counter
        if( i==1) {
            if(length(grep("love", line))>0) {
                countLove <- countLove+1
            }
            if(length(grep("hate", line))>0) {
                countHate <- countHate+1
            }
            if(length(grep("biostats", line))>0) {
                print(line)
            }
            if(length(grep("^A computer once beat me at chess, but it was no match for me at kickboxing$", line))>0) {
                print(line)
            }
            
            
        }
    }
    # Close the input file
    close(conInput)
}

# Close all of the output files
close(conDevTrain)
close(conDevTest)
close(conProdValid)
close(conProdTrain)
close(conProdTest)


print(longestLine)
print(countLove)
print(countHate)
print(countLove/countHate)

# Perform on the data, doing the same on each data set