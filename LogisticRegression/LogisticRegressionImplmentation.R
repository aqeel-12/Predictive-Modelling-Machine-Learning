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
#summary(Value)

housingdataset <- data.frame(housingdataset, Value)
housingdataset$chas <- as.factor(housingdataset$chas)
#summary(housingdataset)

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

## Using k-fold CV for building and estimating the performance of a logistic
## regression model

#The columns LSTAT, INDUS, RM, TAX, NOX, PTRAIO has a 
#correlation score above 0.45 with MEDV which is a good 
#indication of using as predictors. Lets plot these columns against MEDV.

accuracies <- numeric(numfolds)
startval <- 1
for(kth in (1:numfolds)){
  endval <- vct.sizes[kth] + startval - 1
  
  glm_model <- glm(Value ~ lstat+rm+ptratio+tax+nox+indus,
                   family=binomial, 
                   data = training_testing[-(startval:endval), ])
  glm.pred.vals <- predict(glm_model,
                           newdata = training_testing[startval:endval, ],
                           type="response")
  glm.predictions <- rep("High", length(glm.pred.vals))
  glm.predictions [glm.pred.vals > 0.50] <- "Low" 
  
  tbl.results <- table(training_testing[startval:endval, 15], glm.predictions)
  
  #print(tbl.results)
  
  accuracy <- (round((tbl.results[1,1] + tbl.results[2,2])/sum(tbl.results), 4)*100)
  
  accuracies[kth] <- accuracy
  startval <- endval + 1
}


overall.glm.accuracy <- mean(accuracies)
print(paste("For the best glm model, the overall k-fold CV accuracy is:",
            round(overall.glm.accuracy, 6)))




#################################

# Accuracy on Hold-out set
  glm_model <- glm(Value ~ lstat+rm+ptratio+tax+nox+indus,
                   family=binomial, 
                   data = training_testing)
  
  glm.pred.vals <- predict(glm_model,
                           newdata = holdout,
                           type="response")
  glm.holdout.predictions <- rep("High", length(glm.pred.vals))
  glm.holdout.predictions [glm.pred.vals > 0.50] <- "Low" 
  
  tbl.results <- table(holdout$Value, glm.holdout.predictions)
  
  print(tbl.results)
  
  accuracy <- (round((tbl.results[1,1] + tbl.results[2,2])/sum(tbl.results), 4)*100)

  print(paste("Accuracy on Holdout set is :",
             round(accuracy, 6)))