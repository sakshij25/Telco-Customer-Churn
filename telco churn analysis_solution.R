##DATA PRE-PROCESSING

tcc <- read.csv("C:/Users/Prachi/Downloads/Telco-Customer-Churn.csv")

library(dplyr)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(plyr)
library(caTools)
library(class)

View(tcc)
class(tcc)
summary(tcc)
head(tcc)
dim(tcc)
str(tcc)

sapply(tcc, function(x) sum(is.na(x)))

tcc <- tcc[complete.cases(tcc), ]

summary(tcc)

##DATA WRANGLING

for(i in 1:ncol(tcc[,c(10:15)])) {
        tcc[,c(10:15)][,i] <- as.factor(mapvalues
                                              (tcc[,c(10:15)][,i], from =c("No internet service"),to=c("No")))
}

summary(tcc)

tcc$MultipleLines <- as.factor(mapvalues(tcc$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

min(tcc$tenure);max(tcc$tenure)

group_tenure <- function(tenure){
        if (tenure >= 0 & tenure <= 12){
                return('0-12 Month')
        }else if(tenure > 12 & tenure <= 24){
                return('12-24 Month')
        }else if (tenure > 24 & tenure <= 48){
                return('24-48 Month')
        }else if (tenure > 48 & tenure <=60){
                return('48-60 Month')
        }else if (tenure > 60){
                return('> 60 Month')
        }
}

tcc$tenure_group <- sapply(tcc$tenure,group_tenure)
tcc$tenure_group <- as.factor(tcc$tenure_group)

summary(tcc)
View(tcc)

tcc$SeniorCitizen <- factor(tcc$SeniorCitizen,
                              levels = c('0','1'),
                              labels = c('No','Yes'))


summary(tcc)
View(tcc)
tcc$customerID <- NULL
tcc$tenure <- NULL

##EXPLORATORY DATA ANALYSIS
## CORRELATION BETWEEN NUMERICAL VARIABLES

numeric.var <- sapply(tcc, is.numeric)
corr.matrix <- cor(tcc[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

tcc$TotalCharges <- NULL

View(tcc)

## VISUALIZATION OF CATEGORICAL VARIABLE

ggplot(data = tcc)+
          ggtitle("Gender")+
          geom_bar(aes(x = gender, fill = Churn), position = "dodge")

ggplot(data = tcc)+
        ggtitle("Senior Citizen")+
        geom_bar(aes(x = SeniorCitizen, fill = Churn), position = "dodge")

ggplot(data = tcc)+
        ggtitle("Partner")+
        geom_bar(aes(x = Partner, fill = Churn), position = "dodge")

ggplot(data = tcc)+
        ggtitle("Dependents")+
        geom_bar(aes(x = Dependents, fill = Churn), position = "dodge")

ggplot(data = tcc)+
        ggtitle("Phone Service")+
        geom_bar(aes(x = PhoneService, fill = Churn), position = "dodge")

ggplot(data = tcc)+
        ggtitle("Multiple lines")+
        geom_bar(aes(x = MultipleLines, fill = Churn), position = "dodge")

ggplot(data = tcc)+
        ggtitle("Internet services")+
        geom_bar(aes(x = InternetService, fill = Churn), position = "dodge")

ggplot(data = tcc)+
        ggtitle("Online Security")+
        geom_bar(aes(x = OnlineSecurity, fill = Churn), position = "dodge")

ggplot(data = tcc)+
        ggtitle("Online Backup")+
        geom_bar(aes(x = OnlineBackup, fill = Churn), position = "dodge")

ggplot(data = tcc)+
        ggtitle("Device Protection")+
        geom_bar(aes(x = DeviceProtection, fill = Churn), position = "dodge")

ggplot(data = tcc)+
          ggtitle("Tech Support")+
          geom_bar(aes(x = TechSupport, fill = Churn), position = "dodge")

ggplot(data = tcc)+
        ggtitle("Streaming TV")+
        geom_bar(aes(x = StreamingTV, fill = Churn), position = "dodge")

ggplot(data = tcc)+
        ggtitle("Streaming Movies")+
        geom_bar(aes(x = StreamingMovies, fill = Churn), position = "dodge")

ggplot(data = tcc)+
        ggtitle("Contract")+
        geom_bar(aes(x = Contract, fill = Churn), position = "dodge")

ggplot(data = tcc)+
          ggtitle("Paperless Billing")+
          geom_bar(aes(x = PaperlessBilling, fill = Churn), position = "dodge")

ggplot(data = tcc)+
          ggtitle("Payment Method")+
          geom_bar(aes(x = PaymentMethod, fill = Churn), position = "dodge")

ggplot(data = tcc)+
  ggtitle("Tenure Group")+
  geom_bar(aes(x = tenure_group, fill = Churn), position = "dodge")

## CORRELATION BETWEEN CHURN AND ALL VARIABLES


corr <- as.data.frame(lapply(tcc,as.numeric))     
corrr <- cor(corr$Churn, corr)
corrr

## LOGISTIC REGRESSION

set.seed(123)
View(tcc)
split <- sample.split(tcc$Churn, SplitRatio = 0.7)

training_set <- subset(tcc, split == TRUE)
View(training_set)

test_set <- subset(tcc, split == FALSE)

summary(test_set)

logistic_reg <- glm(formula = Churn~.,
                    family='binomial',
                    data = training_set)

summary(logistic_reg)

##PROBABILITY PREDICT

test_set$Churn <- as.character(test_set$Churn)
test_set$Churn[test_set$Churn=="No"] <- "0"
test_set$Churn[test_set$Churn=="Yes"] <- "1"
prob_pred <- predict(logistic_reg, type = 'response',newdata = test_set)
prob_pred
y_pred <- ifelse(prob_pred>0.5,1,0)
y_pred

confusion_matrix <- table(test_set[,18],y_pred)
confusion_matrix

misClasificError <- mean(y_pred != test_set$Churn)

print(paste('Logistic Regression Accuracy',1-misClasificError))

##DECISION TREE
##here we generate decision tree on most significant features i.e tenure,
##contract and paperless billing. we can check this while generating logistic regression 

tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, training_set)
plot(tree)

##DESICION TREE CONFUSION MATRIX

pred_tree <- predict(tree, test_set)

print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = test_set$Churn)

##DECISION TREE ACCURACY

p1 <- predict(tree, training_set)
tab1 <- table(Predicted = p1, Actual = training_set$Churn)
tab2 <- table(Predicted = pred_tree, Actual = test_set$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))
