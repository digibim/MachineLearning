
getPrediction <- function(predMatrix, truth) {
        classes <- c("A","B","C","D","E")
        pmax <- apply(predMatrix, 1, max)
        pcol <- apply(predMatrix, 1, which.max)
        predClass <- classes[pcol]
        x <- data.frame(predClass, pmax, truth)
        names(x) <- c("Predict", "Prob", "Truth")
        x$Correct <- as.character(x$Predict)==as.character(x$Truth)        
        return(x)
}

getProb <- function(probs,targets) {
        values <- vector()
        for(i in c(1:length(targets))) {
                values <- c(values, probs[i,targets[i]])
        }
        return(values)
}

numreps <- 10
allRMStrain <- vector()
allRMStest <- vector()
for(i in c(1:numreps)) {        
        inTrain <- createDataPartition(y=trainImport$classe, p=0.7, list=FALSE)
        training <- trainImport[inTrain,]
        testing <- trainImport[-inTrain,]
        
        fitTree <- train(classe ~ ., data=training, method="rpart")
        trainPred <- predict(fitTree$finalModel, newdata=training)
        testPred <- predict(fitTree$finalModel, newdata=testing)
        
        trainingResults <- getPrediction(trainPred, training$classe)
        testResults <- getPrediction(testPred, testing$classe)
        
        correctTraining <- getProb(as.data.frame(trainPred),training$classe)
        rmsErrorTrain <- sqrt(sum((1-correctTraining)^2)/length(correctTraining))
        
        correctTesting <- getProb(as.data.frame(testPred),testing$classe)
        rmsErrorTest <- sqrt(sum((1-correctTesting)^2)/length(correctTesting))
        
        allRMStrain <- c(allRMStrain, rmsErrorTrain)
        allRMStest <- c(allRMStest, rmsErrorTest)
}
