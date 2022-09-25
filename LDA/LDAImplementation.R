library(MASS)

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
#summary(housingdataset)
head(housingdataset)

for (iternum in (1:3)){
  print(paste("Shuffle number:", iternum))
  housingdataset <- housingdataset[sample(nrow(housingdataset)), ]
}

sz <- nrow(housingdataset) * 0.90
training_testing <- housingdataset[1:sz,]
holdout <- housingdataset[(sz):nrow(housingdataset),]
#summary(holdout)

## k-fold CV

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
#The columns LSTAT, INDUS, RM, TAX, NOX, PTRAIO has a 
#correlation score above 0.45 with MEDV which is a good 
#indication of using as predictors. Lets plot these columns against MEDV.
accuracies <- numeric(numfolds)
startval <- 1
for(kth in (1:numfolds)){
  endval <- vct.sizes[kth] + startval - 1
  lda.1.fit <- lda(Value ~ lstat+rm+ptratio+tax+nox+indus, #formula representing the model equation
                 data=training_testing, #indicating which dataset to use
                 subset=-(startval:endval)) #specifying which subset of observations to use in fitting the model

  lda.1.pred <- predict(lda.1.fit, # use the previously-fit LDA model to make predictions
                      training_testing[startval:endval, ])
  
     tbl.results <- table(training_testing[startval:endval, 15], lda.1.pred$class)
  #print(tbl.results)
  
  accuracy <- (round((tbl.results[1,1] + tbl.results[2,2])/sum(tbl.results), 4)*100)
  
  accuracies[kth] <- accuracy
  startval <- endval + 1
}
overall.lda.accuracy <- mean(accuracies)
print(paste("For the best lda model, the overall k-fold CV accuracy is:",
            round(overall.lda.accuracy, 6)))





#######################################################################
#######################################################################

#using holdout set 

lda.1.fit <- lda(Value ~ lstat+rm+ptratio+tax+nox+indus, #formula representing the model equation
                 data=training_testing) #indicating which dataset to use
                 #subset=1:sz) #specifying which subset of observations to use in fitting the model


lda.1.pred <- predict(lda.1.fit, # use the previously-fit LDA model to make predictions
                      holdout) # use the hold-out subset of observations for making predictions


plot(lda.1.pred$class, main="Plot of distribution of predicted levels of Value")

tbl.results <- table(holdout$Value, ## select all rows starting after the mid-point, and in 15th column - Value. Why?
lda.1.pred$class)
print(tbl.results)

## compute the accuracy by dividing the sum of correct predictions by the total number of predictions
print (paste("Accuracy of model on holdout data:", round((tbl.results[1,1] + tbl.results[2,2])/sum(tbl.results), 4)*100, "%"))


#######################################################
#######################################################

