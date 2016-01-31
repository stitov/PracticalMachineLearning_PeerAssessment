lapply (c ('caret', 'dplyr', 'doParallel'), require, character.only = T)

setwd ("~/Coursera")
data <- read.csv ('pml-training.csv', stringsAsFactors = F, na.strings = c (NA, '#DIV/0!'))
quiz <- read.csv ('pml-testing.csv', stringsAsFactors = F, na.strings = c (NA, '#DIV/0!'))

# removing NAs
NAs = apply (data, 2, function (x) sum (is.na (x))) / nrow (data)
data <- data [ , -which (NAs > 0.95)]
quiz <- quiz [ , -which (NAs > 0.95)]

# removing columns with date and time stamps
data <- data [, -c (1, 3:7)]
quiz <- quiz [, -c (1, 3:7)]

# reorder columns for better view
data <- select (data, user_name, classe, everything())
quiz <- select (quiz, user_name, problem_id, everything())

# set char variables as factors
data <- mutate (data, user_name = as.factor (data$user_name), classe = as.factor (data$classe))
quiz <- mutate (quiz, user_name = as.factor (quiz$user_name))

# make a cluster for better perfomance
cl <- makeCluster (detectCores ())
registerDoParallel (cl)

# split data to training and testing set
set.seed (1)
inTrain <- createDataPartition (data$classe, p = 0.7, list = F)
training <- data [inTrain, ]
testing  <- data [-inTrain, ]

# number of observations of each participant
rbind (training = table (training$user_name), testing = table (testing$user_name))

# split sets by user name for better accuracy and performance
trainSplit <- split (training [, -1], training$user_name)
testSplit <- split (testing [, -1], testing$user_name)
quizSplit <- split (quiz [, -1], quiz$user_name)

# Declare a function for model fit
fitModel <- function (training, method = 'rf') {
        trControl <- trainControl (method = 'boot',
                                   number = 10,
                                   allowParallel = T)
        
        fit <- train (training$classe ~ ., 
                      method = method,
                      verbose = F,
                      trControl = trControl,
                      preProcess = c ('center','scale'), 
                      data = training)
        
}

mClSum <- NULL
quizPred <- NULL

for (i in 1:length (trainSplit)) {

        fit <- fitModel (trainSplit [[i]], method = 'rf')
        
        mCl <- multiClassSummary (data.frame (pred = predict (fit, newdata = testSplit [[i]]), 
                                              obs = testSplit [[i]]$classe), 
                                              lev = levels (testSplit [[i]]$classe))
        mClSum <- rbind (mClSum, mCl)

        quizPred <- rbind (quizPred, data.frame (problem_id = quizSplit [[i]]$problem_id, 
                                                 classe = predict (fit, newdata = quizSplit [[i]])))
}

mClSum

# validate answer
valid <- read.csv ('WearableComputing_weight_lifting_exercises_biceps_curl_variations.csv', stringsAsFactors = F)
valid <- mutate (valid, timestamp = paste0 (raw_timestamp_part_1,'_', raw_timestamp_part_2))

quiz <- read.csv ('pml-testing.csv', stringsAsFactors = F, na.strings = c (NA, '#DIV/0!'))
quiz <- mutate (quiz, timestamp = paste0 (raw_timestamp_part_1,'_', raw_timestamp_part_2))
valid <- left_join (select(quiz, timestamp, problem_id) , select (valid, timestamp, classe))

table (arrange (quizPred, problem_id)$classe, valid$classe)

stopCluster (cl)