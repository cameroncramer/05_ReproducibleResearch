library(kernlab)
data(spam)
#First Test
# Perform the subsampling
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)
## trainIndicator
##    0    1 
## 2314 2287
trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator == 0, ]
#Data Summeries
names(trainSpam)
head(trainSpam)
table(trainSpam$type)

#PLOTS
#Average Number of Capital Letters
plot(trainSpam$capitalAve ~ trainSpam$type)
# lots of zeros in dataset, so +1 so we don't have log of 0

#Relationships between predictors
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

#Relationships between predictors
plot(log10(trainSpam[, 1:4] + 1))

#Clustering
#Take first cut at hierarchical clustering not helpful
#Sometimes cluster algorhythms are better to redo clustering after data transformation  
hCluster = hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)

#New clustering
#Revised hierarchical cluster dendogram +1 to deal with 0's
hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated)

#Statistical prediction/modeling-Second Test
trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x, y) sum(x != (y > 0.5))
cvError = rep(NA, 55)
library(boot)
for (i in 1:55) {lmFormula = reformulate(names(trainSpam)[i], response = "numType")
glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}
## Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]
## [1] "charDollar"

#Get a measure of uncertainty
## Use the best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)
## Get predictions on the test set
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])
## Classify as `spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"
## Classification table
table(predictedSpam, testSpam$type)
## Error rate
(61 + 458)/(1346 + 458 + 61 + 449)
## [1] 0.2243

#INTERPRET RESULTS
#Use the appropriate language
#describes
#correlates with/associated with
#leads to/causes
#predicts
#Give an explanation
#Interpret coefficients
#Interpret measures of uncertainty
#ABOVE EXAMPLE RESULTS
#The fraction of charcters that are dollar signs can be used to predict if an email is Spam
#Anything with more than 6.6% dollar signs is classified as Spam
#More dollar signs always means more Spam under our prediction
#Our test set error rate was 22.4%