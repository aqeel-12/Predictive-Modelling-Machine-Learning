## the following is the set up needed for KNN.
rm(list = ls())
library(MASS)
library(class)
data(Boston)
library(corrplot)
housingdataset <- as.data.frame(Boston) # attach a label to the dataframe object

#Use correlation matrix to find the relationship among variables with target variable

M<-cor(housingdataset)
M_abs<-abs(M)
corrplot(M_abs, method="number")
#head(round(M_abs, 2),15)


Value <- ifelse(housingdataset$medv > median(housingdataset$medv),
                "High", "Low")
Value <- as.factor(Value)
summary(Value)

housingdataset <- data.frame(housingdataset, Value)
housingdataset$chas <- as.factor(housingdataset$chas)
summary(housingdataset)



for (iternum in (1:3)){
  print(paste("Shuffle number:", iternum))
  housingdataset <- housingdataset[sample(nrow(housingdataset)), ]
}

sz <- nrow(housingdataset) * 0.90
training_testing <- housingdataset[1:sz,]
holdout <- housingdataset[(sz):nrow(housingdataset),]


#The columns LSTAT, INDUS, RM, TAX, NOX, PTRAIO has a 
#correlation score above 0.45 with MEDV which is a good 
#indication of using as predictors. Lets plot these columns against MEDV.
trn <- cbind(training_testing$lstat,
             training_testing$rm,
             training_testing$ptratio,
             training_testing$tax,
             training_testing$indus,
             training_testing$nox)

tst <- cbind(holdout$lstat, holdout$rm, holdout$ptratio,
             holdout$tax, holdout$indus, holdout$nox)

cl <- training_testing$Value

preds <- knn(trn, tst, cl, k = 3)
preds5 <- knn(trn, tst, cl, k = 5)
tbl.results <- table(holdout$Value, preds)
tbl.results5<- table(holdout$Value, preds5)

print(tbl.results)

accuracy <- (round((tbl.results[1,1] +
             tbl.results[2,2])/sum(tbl.results), 4)*100)
accuracy5 <- (round((tbl.results5[1,1] +
             tbl.results5[2,2])/sum(tbl.results5), 4)*100)



print(paste("Accuracy on Holdout set with k=3 : ",
            round(accuracy, 6)))


print(paste("Accuracy on Holdout set with k=5 : ",
            round(accuracy5, 6)))


###############################################################



## k-fold CV  (5-folds)
# k=3
samplesize <- nrow(training_testing)
numfolds <- 5

quotient <- samplesize %/% numfolds
remainder <- samplesize %% numfolds 

vct.sizes <- rep(quotient, numfolds) 
if(remainder > 0){
  for(i in 1:remainder){
    vct.sizes[i] <- vct.sizes[i] + 1 
  }
}

print(paste("K:", numfolds, "n:", samplesize))
print(vct.sizes)

set.seed(99)
training_testing <- training_testing[sample(nrow(training_testing)), ]

## Using k-fold CV 
tv_dataset <- training_testing
accuracies <- numeric(numfolds)
accuracies_5 <- numeric(numfolds)

startval <- 1
for(kth in (1:numfolds)){
  endval <- vct.sizes[kth] + startval - 1

  trn <- cbind(tv_dataset[-(startval:endval), ]$lstat,
               tv_dataset[-(startval:endval), ]$rm,
               tv_dataset[-(startval:endval), ]$ptratio,
               tv_dataset[-(startval:endval), ]$tax,
               tv_dataset[-(startval:endval), ]$indus,
               tv_dataset[-(startval:endval), ]$nox)

  tst <- cbind(tv_dataset[startval:endval, ]$lstat,
               tv_dataset[startval:endval, ]$rm,
               tv_dataset[startval:endval, ]$ptratio,
               tv_dataset[startval:endval, ]$tax,
               tv_dataset[startval:endval, ]$indus,
               tv_dataset[startval:endval, ]$nox)

  cl <- tv_dataset[-(startval:endval), ]$Value

  preds <- knn(trn, tst, cl, k = 3)
  preds_5 <- knn(trn, tst, cl, k = 5)
  
  tbl.results <- table(tv_dataset[startval:endval, ]$Value, preds)
  tbl.results_5 <- table(tv_dataset[startval:endval, ]$Value, preds_5)


  #print(tbl.results)

  accuracy <- (round((tbl.results[1,1] + tbl.results[2,2])/sum(tbl.results), 4)*100)
  accuracy_5 <- (round((tbl.results_5[1,1] + tbl.results_5[2,2])/sum(tbl.results_5), 4)*100)
  
  accuracies[kth] <- accuracy
  accuracies_5[kth] <- accuracy_5

  startval <- endval + 1
}
print(paste0("Avg. CV accuracy with k=3 :  ",
             mean(accuracies)))
print(paste0("Avg. CV accuracy with k=5 : ",
             mean(accuracies_5)))


