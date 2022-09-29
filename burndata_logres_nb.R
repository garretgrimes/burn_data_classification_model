#1 Set the working directory and load the data
setwd("C:/Users/garre/desktop/DATA630")      # set the working directory
burn <- read.csv("burn.csv", as.is = FALSE)  # Read the csv file
dim(burn)      #Check the number of rows and the number of variables
head(burn)     #Preview the first 6 rows
summary(burn)  #Check the descriptive statistics
View(burn)     #View entire data set

# Load Libraries
library("tidyverse")
library("gridExtra")
library ("e1071")  
library ("arules") 

# Data pre processing
burn$ID<-NULL     #Remove the unique identifier
burn$FACILITY<-NULL    #Remove the facility as it does not pertain to analysis

#Check if there are missing values
colSums(is.na(burn))
#Convert the following variables to factor
burn$GENDER<-factor(burn$GENDER, levels = 0:1, labels = c("Female","Male"))
burn$RACEC<-factor(burn$RACEC, levels=0:1, labels = c("Non-White","White"))
burn$INH_INJ<-factor(burn$INH_INJ, levels=0:1, labels = c("No","Yes"))
burn$FLAME<-factor(burn$FLAME, levels=0:1, labels = c("No","Yes"))

# Preparing data for EDA
burn$ROUNDAGE<-round(burn$AGE)
burn$AGEBIN<-cut(burn$ROUNDAGE, breaks =c(-1, 12, 19, 30, 45, 95), 
                 labels=c('Child','Teen','Young Adult','Adult','Older Adult'))

burn$ROUNDTBSA<-round(burn$TBSA)
burn$TBSABIN<-cut(burn$ROUNDTBSA, breaks =c(-1, 2.5, 6, 16, 99), 
                 labels=c('Minor','Moderate', 'Severe', 'Very Severe'))

burn$DEATH1<-factor(burn$DEATH, levels=0:1, labels = c("Survived", "Died"))

# Exploratory Data Analysis
# Visualizing Distributions
p1 <- ggplot(burn, aes(DEATH1, ..count..,fill=DEATH1)) + geom_bar() + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p2 <- ggplot(burn, aes(GENDER, ..count..,fill=GENDER)) + geom_bar() + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p3 <- ggplot(burn, aes(RACEC, ..count..,fill=RACEC)) + geom_bar()+ 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p4 <- ggplot(burn, aes(INH_INJ, ..count..,fill=INH_INJ)) + geom_bar()+ 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p5 <- ggplot(burn, aes(FLAME, ..count..,fill=FLAME)) + geom_bar()+ 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p6 <- ggplot(burn, aes(AGEBIN, ..count..,fill=AGEBIN)) + geom_bar()+ 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(p1,p2,p3,p4,p5,p6,nrow=3)

#Visualizing Distributions against Death
p7 <- ggplot(burn, aes(GENDER, ..count..)) + geom_bar(aes(fill=DEATH1),
                                                     position='dodge')
p8 <- ggplot(burn, aes(RACEC, ..count..)) + geom_bar(aes(fill=DEATH1),
                                                     position='dodge')
p9 <- ggplot(burn, aes(INH_INJ, ..count..)) + geom_bar(aes(fill=DEATH1),
                                                     position='dodge')
p10 <- ggplot(burn, aes(FLAME, ..count..)) + geom_bar(aes(fill=DEATH1),
                                                      position='dodge')
p11 <- ggplot(burn, aes(AGEBIN, ..count..)) + geom_bar(aes(fill=DEATH1),
                                                      position='dodge')
p12 <- ggplot(burn, aes(TBSABIN, ..count..)) + geom_bar(aes(fill=DEATH1),
                                                      position='dodge')

grid.arrange(p7,p8,p9,p10,p11,p12,nrow=3)

# Remove columns created for EDA to prep for modeling
burn$ROUNDAGE<-NULL     
burn$AGEBIN<-NULL
burn$DEATH1<-NULL
burn$ROUNDTBSA<-NULL
burn$TBSABIN<-NULL

# Set the seed value to ensure that result is reproducible
set.seed(1234)

# divide the data into training and test
ind <- sample(2, nrow(burn), replace = TRUE, prob = c(0.7, 0.3))
train.data <- burn [ind == 1, ]
test.data <- burn [ind == 2, ]

# Build and interpret the model
# Store the method output in a variable model
logmodel <- glm(DEATH ~ ., data = train.data, family = binomial())
logmodel               # Output the coefficients and intercept
summary(logmodel)      # Output the p value for each coefficient
coef(logmodel)         # To display the model coefficients only
logmodel$coefficients  # Another way to display the model coefficients only
logmodel$residuals     # Output the residuals
residuals(logmodel)    # Another way to output the residuals
exp(coef(logmodel))    # Odds ratios
confint(logmodel)      # Confidence intervals

# Evaluate the model on training data
#Store the predicted values in a variable my predictions
mypredictions<-round(predict(logmodel, type="response"))
#Print the confusion matrix for the training data
table (mypredictions, train.data$DEATH, dnn=c("predicted", "actual"))
#classification accuracy for the training data
trainaccuracy<-mean(round(predict (logmodel, train.data, type="response"))== 
                      train.data$DEATH)
#Classification error for the training data
mean(round(predict (logmodel, train.data, type="response"))!= train.data$DEATH)
# Calculating Precision, Recall, and F1 for training data
trainprecision <- 576/(576+17)
trainrecall <- 576/(576+28)
trainf1<-(2*trainprecision*trainrecall)/(trainprecision+trainrecall)
traindf<-data.frame(trainaccuracy,trainprecision, trainrecall, trainf1)
traindf
# Evaluate the model on test data
#Store the predicted values in a variable mypredictions
mypredictions<-round(predict (logmodel, test.data, type="response")) 
#Confusion matrix for the test data
table (mypredictions, test.data$DEATH, dnn=c("predict", "actual"))
# Classification accuracy for test data
testaccuracy<-mean(round(predict (logmodel, test.data, type="response"))== 
                     test.data$DEATH)
# Misclassification error for test data
mean(round(predict (logmodel, test.data, type="response"))!= test.data$DEATH)
# Calculating Precision, Recall, and F1 for test data
testprecision <- 249/(249+20)
testrecall <- 249/(249+8)
testf1<-(2*testprecision*testrecall)/(testprecision+testrecall)
testdf<-data.frame(testaccuracy,testprecision, testrecall, testf1)
traindf
testdf
# Build the ROC curve
# install.packages('ROCR')
library(ROCR)                                           #Load ROCR to memory
ROCRpred <- prediction(mypredictions, test.data$DEATH)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.2), lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")                  #45 degrees line

# Residuals plot
plot(predict(logmodel),residuals(logmodel), col=c("blue"))
lines(lowess(predict(logmodel),residuals(logmodel)), col=c("black"), lwd=2)
abline(h=0, col="grey")

# Effect displays
# install.packages("effects")
library(effects)
plot(allEffects(logmodel))

# Reduced model
logmodel2<-step(logmodel)
summary(logmodel2)
plot(allEffects(logmodel2))
# Classification accuracy and confusion matrix for reduced model
mypredictions<-round(predict (logmodel2, test.data, type="response")) 
table (mypredictions, test.data$DEATH, dnn=c("predicted", "actual"))
finalaccuracy<-mean(round(predict (logmodel2, test.data, type="response"))
                    == test.data$DEATH)
# Calculating Precision, Recall, and F1 for final logistic regression model
finalprecision <- 248/(248+19)
finalrecall <- 248/(248+9)
finalf1<-(2*finalprecision*finalrecall)/(finalprecision+finalrecall)
finaldf<-data.frame(finalaccuracy,finalprecision, finalrecall, finalf1)
finaldf

# Prepping data for Naive Bayes
# Discretize the numeric variables
burn$AGE<-discretize(burn$AGE, method="frequency", 6)
burn$TBSA<-discretize(burn$TBSA, method="frequency", 6)

# Build the NB model
nbmodel<-naiveBayes(DEATH~., train.data)
nbmodel
prop.table(table(train.data$DEATH))  

#  Evaluate the NB model on a training set
# This command returns the predicted probabilities for each class
predict(nbmodel, train.data, type="raw")  
# This command returns the predicted class
predict(nbmodel, train.data, type="class") 
table(predict(nbmodel, train.data, type="class"), train.data$DEATH, 
      dnn=c("predicted", "actual"))
#classification accuracy for the training data
nbtrainaccuracy<-mean(predict (nbmodel, train.data, type="class")== 
                        train.data$DEATH)
#Classification error for the training data
mean(predict (nbmodel, train.data, type="class")!= train.data$DEATH)
# Calculating Precision, Recall, and F1 for training data
nbtrainprecision <- 566/(566+30)
nbtrainrecall <- 566/(566+27)
nbtrainf1<-(2*nbtrainprecision*nbtrainrecall)/(nbtrainprecision+nbtrainrecall)
nbtraindf<-data.frame(nbtrainaccuracy,nbtrainprecision, nbtrainrecall,
                      nbtrainf1)
nbtraindf

#  Evaluate the NB model on a test set
#This command returns the predicted probabilities for each class
predict(nbmodel, test.data, type="raw")    
#This command returns the predicted class
predict(nbmodel, test.data, type="class") 
table(predict(nbmodel, test.data, type="class"), test.data$DEATH,
      dnn=c("predicted", "actual"))
#classification accuracy for the test data
nbtestaccuracy <-mean (predict (nbmodel, test.data, type="class")== 
                         test.data$DEATH)
#Classification error for the test data
mean(predict (nbmodel, test.data, type="class")!= test.data$DEATH)

# Calculating Precision, Recall, and F1 for test data
nbtestprecision <- 245/(245+18)
nbtestrecall <- 245/(245+12)
nbtestf1<-(2*nbtestprecision*nbtestrecall)/(nbtestprecision+nbtestrecall)
nbtestdf<-data.frame(nbtestaccuracy,nbtestprecision, nbtestrecall, nbtestf1)
nbtestdf


# Plot the ROC Curve
mypredictions1<-predict(nbmodel, test.data, type="class")
ROCRpred1 <- prediction(as.numeric(mypredictions1)-1, test.data$DEATH)
ROCRperf1 <- performance(ROCRpred1, 'tpr','fpr')
plot(ROCRperf1, colorize = TRUE, text.adj = c(-0.2,1.2), lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")                  #45 degrees line


