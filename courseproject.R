#Load Data
training <- read.csv("pml-training.csv", header=TRUE, na.strings=c("","NA"))
testing <- read.csv("pml-testing.csv", header=TRUE, na.strings=c("","NA"))

#function for testing columns for NAs
na.test <-  function (x) {
    w <- sapply(x, function(x)all(is.na(x)))
    which(w==TRUE)
}

#Clean Data
trainSet <- training[training$new_window=="no",]
dropcols <- c(1, 2, 3, 4, 5, 6, 7)
trainSet <- trainSet[,-dropcols]
NAcols <- na.test(trainSet)
trainSet <- trainSet[,-NAcols]
testSet <- testing[,-dropcols]
testSet <- testSet[,-NAcols]

library(caret)
library(randomForest)
set.seed(160)
modFit <-randomForest(classe~., data=trainSet, xtest=testSet[,-53])
print(modFit)

result <- rfcv(trainSet[,-53], trainSet$classe)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

modImportance <- as.data.frame(modFit$importance)
modRowNames <- rownames(modImportance)
modImportance$Predictors <- rownames(modImportance)
modImportance <- modImportance[order(modImportance$MeanDecreaseGini, decreasing=TRUE),]

pred <- modFit$test[1]$predicted

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(pred)

