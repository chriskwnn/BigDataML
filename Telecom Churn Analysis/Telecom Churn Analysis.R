library(dplyr)
library(dummies)
library(e1071)
library(pROC)
data.df <- read.csv("/Users/chriskwan/Desktop/Big Data Analytics/BigDataMLCourse Github/Telecom Churn Data.csv")
#head(data.df)
#str(data.df)
#Get rid of na rows and ID row
data.df <- na.omit(data.df)
data.df$ID.no <- NULL
#Run logistic with everything
correlation_matrix<-data.frame(cor(data.df))
signif_na_variables<-correlation_matrix[c("total_ic_mou_6","total_ic_mou_7","total_ic_mou_8","total_vol_6","total_vol_7","total_vol_8"),c("total_ic_mou_6","total_ic_mou_7","total_ic_mou_8","total_vol_6","total_vol_7","total_vol_8")]
signif_na_variables
regression1 <- glm(churn~.,data = data.df, family="binomial")
summary(regression1)
#Remove highly correlated rows which return NA
data.df <- subset(data.df, select=-c(total_ic_mou_6,total_ic_mou_7,total_ic_mou_8,total_vol_6,total_vol_7,total_vol_8))
head(data.df)
regression2 <- glm(churn~.,data = data.df, family="binomial")
summary(regression2)
#Remove total columns since they should say the same thing as their constituent variables
col_Total <- colnames(select(data.df,contains("total")))
data.df <- subset(data.df, select=-c(total_og_mou_6,total_og_mou_7,total_og_mou_8,total_og_to_ic_mou_6,total_og_to_ic_mou_7,total_og_to_ic_mou_8))
regression3 <- glm(churn~.,data = data.df, family="binomial")
summary(regression3)
#All ratio columns are generally insignificant. This is because its constituents similar to total already explains what it explains
data.df <- select(data.df, -contains("to"))
regression4 <- glm(churn~.,data = data.df, family="binomial")
summary(regression4)
#Use the most recent month, ignore month 6
data.df <- select(data.df, -contains("6"))
regression5 <- glm(churn~.,data = data.df, family="binomial")
summary(regression5)
#Remove all happy month variable because if they are happy the values wont be showing indication of leaving. Further, almost variables with sore and happy had either both as insignificant or only one as significant. This means that one month is laready telling the same information to the regression.
data.df <- select(data.df, -contains("7"))
regression5 <- glm(churn~.,data = data.df, family="binomial")
summary(regression5)
#Split revenue into a dummy variable above and below average, 1 is above average 0 is below
data.df$revenue_8 <- ifelse(data.df$revenue_8 >= 62, 1, 0) 
data.df <- cbind(data.df,dummy(data.df$revenue_8, sep = "_"))
data.df <- data.df[-c(1)]
head(data.df)
regression6 <- glm(churn~.,data = data.df, family="binomial")
summary(regression6)
#Revenue still insignificant 
data.df <- select(data.df, -contains("revenue"))
data.df <- select(data.df, -contains("data"))
regression7 <- glm(churn~.,data = data.df, family="binomial")
summary(regression7)
finalRegression<-regression7

#Confusion Matrix
pred <- predict(finalRegression, data.df, type = "response")
Output <- data.frame(actual = data.df$churn, predicted = pred)
classification <- ifelse(pred>0.05,1,0)
actual = data.df$churn
Output <- data.frame(actual, predprob = pred, classification)
library(caret)
confusionMatrix(as.factor(classification),as.factor(actual))
#ROC
library(ROCR)
predObj <- prediction(pred, data.df$churn)
rocObj = performance(predObj, measure="tpr",x.measure="fpr")
aucObj = performance(predObj, measure="auc")

plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]],4)))

#naive bayes
#Create naivebayes binary dataset
naivebayes.data.df<-data.df
naivebayes.data.df$onnet_mou_8 <- ifelse(naivebayes.data.df$onnet_mou_8 > 65.61, 1, 0) 
naivebayes.data.df$offnet_mou_8 <- ifelse(naivebayes.data.df$offnet_mou_8 > 182.79, 1, 0) 
naivebayes.data.df$roam_ic_mou_8 <- ifelse(naivebayes.data.df$roam_ic_mou_8 > 0, 1, 0) 
naivebayes.data.df$roam_og_mou_8 <- ifelse(naivebayes.data.df$roam_og_mou_8 > 0, 1, 0) 
naivebayes.data.df$loc_og_mou_8 <- ifelse(naivebayes.data.df$loc_og_mou_8 > 110.81, 1, 0) 
naivebayes.data.df$std_og_mou_8 <- ifelse(naivebayes.data.df$std_og_mou_8 > 25.48, 1, 0) 
naivebayes.data.df$isd_og_mou_8 <- ifelse(naivebayes.data.df$isd_og_mou_8 > 0, 1, 0) 
naivebayes.data.df$loc_ic_mou_8 <- ifelse(naivebayes.data.df$loc_ic_mou_8 > 128.73, 1, 0) 
naivebayes.data.df$std_ic_mou_8 <- ifelse(naivebayes.data.df$std_ic_mou_8 > 9.29, 1, 0) 
naivebayes.data.df$isd_ic_mou_8 <- ifelse(naivebayes.data.df$isd_ic_mou_8 > 0, 1, 0) 
naivebayes.data.df$vol_4g_8 <- ifelse(naivebayes.data.df$vol_4g_8 >= 0, 1, 0) 
naivebayes.data.df$vol_3g_8 <- ifelse(naivebayes.data.df$vol_3g_8 >= 0, 1, 0) 
naivebayes.data.df$aon <- ifelse(naivebayes.data.df$aon > 846, 1, 0) 
naivebayes.data.df <- na.omit(naivebayes.data.df)

#Choosing training rows
train.index <- sample(c(1:dim(naivebayes.data.df)[1]), dim(naivebayes.data.df)[1]*0.6)
selected.var<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
#Creating data training data set
train.df <- naivebayes.data.df[train.index, selected.var]
#Creating validation data set, with left over of training data points
valid.df <- naivebayes.data.df[-train.index, selected.var]
data.nb <- naiveBayes(as.factor(churn) ~ ., data = train.df)

pred.prob <- predict(data.nb, newdata = valid.df, type = "raw")
head(pred.prob)
# predict whether a churn happens
pred.class <- predict(data.nb, newdata = valid.df)
head(pred.class)

Output <- data.frame(actual = valid.df$churn, predicted = pred.class, predicted.prob = pred.prob)
classification <- ifelse(pred.prob[,2]>0.5,1,0)
confusionMatrix(as.factor(classification),as.factor(valid.df$churn))
predObj <- prediction(pred.prob[,2], valid.df$churn)
rocObj = performance(predObj, measure="tpr",x.measure="fpr")
aucObj = performance(predObj, measure="auc")
plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]],4)))









