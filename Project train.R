# Split the imported training data
inTrain <- createDataPartition(y=trainImport$classe, p=0.7, list=FALSE)
training <- trainImport[inTrain,]
testing <- trainImport[-inTrain,]

fitTree <- train(classe ~ ., data=training, method="rpart")
# fancyRpartPlot(tmp$finalModel)

trainPred <- predict(fitTree$finalModel, newdata=training)
testPred <- predict(fitTree$finalModel, newdata=testing)

# Find which class has the highest probability

getPrediction <- function(predMatrix, truth) {
        classes <- c("A","B","C","D","E")
        pmax <- apply(predMatrix, 1, max)
        pcol <- apply(predMatrix, 1, which.max)
        predClass <- classes[pcol]
        x <- data.frame(predClass, pmax, truth)
        names(x) <- c("Predict", "Prob", "Truth")
        x$Correct <- x$Predict==x$Truth
        return(x)
}

trainingResults <- getPrediction(trainPred, training$classe)
testResults <- getPrediction(testPred, testing$classe)
percCorrTrain <- sum(trainingResults$Correct)/nrow(trainingResults)
percCorrTest <- sum(testResults$Correct)/nrow(testResults)

# trainMax <- apply(trainPred, 1, max)
# trainCol <- apply(trainPred, 1, which.max)
# trainClass <- classes[trainCol]
# trainingResults <- data.frame(trainClass, trainMax, training$classe)
# names(trainingResults) <- c("Predict", "Prob", "Truth")

# table(trainingResults$Predict, trainingResults$Truth)

getProb <- function(probs,targets) {
        values <- vector()
        for(i in c(1:length(targets))) {
                values <- c(values, probs[i,targets[i]])
        }
        return(values)
}

correctTraining <- getProb(as.data.frame(trainPred),training$classe)
rmsErrorTrain <- sqrt(sum((1-correctTraining)^2)/length(correctTraining))

correctTesting <- getProb(as.data.frame(testPred),testing$classe)
rmsErrorTest <- sqrt(sum((1-correctTesting)^2)/length(correctTesting))


# getProb <- function(probs,target){return(probs[target])}

# accTrain <- mapply(getProb, trainPred, training$classe)
# accTest <- mapply(getProb, testPred, testing$classe)
