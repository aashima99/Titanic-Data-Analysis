###################### IMPORTING ALL LIBRARIES ####################################
#install.packages('e1071', dependencies=TRUE)
library(ggplot2)       # Visualization
library(corrgram)      # Visualization
library(caret)
library(ROCR)
library(dummies)

###################### DATA EXPLORATION ####################################

setwd("C:/Users/HP/OneDrive/Documents/AASHIMA")                     
titanic_data <- read.csv("titanic.csv",stringsAsFactors=FALSE)  #loading dataset
head(titanic_data)

summary(titanic_data)
dim(titanic_data)

#converting values to vectors 
titanic_data$name=as.character(titanic_data$Name)
titanic_data$survived = as.factor(titanic_data$Survived);
titanic_data$sex = as.factor(titanic_data$Sex)
titanic_data$pclass = as.factor(titanic_data$Pclass);

#To know number of males and females 
sex<-table(titanic_data$Sex)
sex

#To differentiate between class of travel
pclass<-table(titanic_data$Pclass)
pclass

survived<-table(titanic_data$Survived)
survived

x<-dim(titanic_data)[1]
y<-survived[2]
z<-y/x
survival_percent<-100*z
survival_percent

#
x<-dim(titanic_data)[1]
y<-survived[1]
z<-y/x
death_percent<-100*z
death_percent

#treating missing values 
colSums(is.na(titanic_data))

###################### VISUALISATION ####################################

#price and pclass
boxplot(Fare ~ Pclass, data = titanic_data, 
        main = 'Fare with respect to Passenger Class', ylab = 'Price', col = 'darksalmon')

#Siblings.Spouses.Aboard vs survived 
#par(mfrow = c(1,3))
boxplot(Siblings.Spouses.Aboard ~ Survived, data = titanic_data, 
        main = 'price with respect to Survived', ylab = 'Count', col = 'darksalmon')
#Parents.Children.Aboard vs survived 
boxplot(Parents.Children.Aboard ~ Survived, data = titanic_data, 
        main = 'Parents / Children with respect to Survived', ylab = 'Count', col = 'darksalmon')

#Check the relation of survived vs died
ggplot.relation.object <- ggplot(titanic_data, aes(x=Survived))
ggplot.relation.object <-ggplot.relation.object+geom_bar()+ggtitle("Survived Bar Chart")
ggplot.relation.object

#Check the PClass/survived_died/sex Distriution bar chart
ggplot.relation.object <- ggplot(titanic_data, aes(x=Pclass, fill = titanic_data$Sex))
ggplot.relation.object <-ggplot.relation.object+geom_bar()+ggtitle("PClass Bar Chart")
ggplot.relation.object

# Age distribution table
summary(titanic_data$Age)
plot(titanic_data$Age)

#Pclass box vs Age
qplot(factor(Pclass), Age, data = titanic_data, geom = "boxplot")

# Age vs survival box
qplot(factor(survived), Age, data = titanic_data, geom = "boxplot")

# Fare distribution Histogram 
summary(titanic_data$Fare)
ggplot.relation.object <- ggplot(titanic_data, aes(x=Fare))
ggplot.relation.object <-ggplot.relation.object+geom_bar(colour="darkred", fill="white")+ggtitle("Fare Histogram Chart")
ggplot.relation.object

#Create correlogram that depicts correlation between variables. All variables need to be numeric
corrgram.data <- titanic_data
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Age","Siblings.Spouses.Aboard", "Fare","Parents.Children.Aboard")

#The positive correlations are shown in blue, while the negative correlations are shown in red. The darker the hue, the greater the magnitude of the correlation.
corrgram(corrgram.data[,corrgram.vars], order=TRUE,
         text.panel=panel.txt,diag.panel = panel.minmax, main="Titanic Data", legend=TRUE)

####################################
#data-preprocessing 
titanic_data <- dummy.data.frame(titanic_data, names=c("Pclass","Sex"), sep="_")
####################################

###################### BUILDING THE MODEL ####################################


## Splitting training and test data
train <- titanic_data[1:667,]
test <- titanic_data[668:887,]

## Set a random seed
set.seed(754)

## Model Creation  (note: not all possble variables are used)
model <- glm(factor(Survived) ~ pclass + sex + Age + Siblings.Spouses.Aboard + Fare + Parents.Children.Aboard,family=binomial(link='logit'),data=train)

## Model Summary
summary(model)

## Using anova() to analyze the table of devaiance
anova(model, test="Chisq")

## Predicting Test Data
result <- predict(model,newdata=test,type='response')
result <- ifelse(result > 0.5,1,0)

## Confusion matrix and statistics
confusionMatrix(as.factor(result),as.factor(test$Survived))

## ROC Curve and calculating the area under the curve(AUC)  
#plot for sensitivity vs specificity 
predictions <- predict(model, newdata=test, type="response")
ROCRpred <- prediction(predictions, test$Survived)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

##Area under the ROC curve 
auc <- performance(ROCRpred, measure = "auc") 
auc <- auc@y.values[[1]]
auc

