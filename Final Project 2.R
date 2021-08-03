#Load the data file
library(readxl)
Loan <- read_excel("C:/Users/Vaibhav-PC/Downloads/Project 2/data.xlsx")

#Not performing on orginal df to avoid loading it again and again.
Loan1 = Loan

#Gives the summary of the variable loan_default
summary(Loan1$loan_default)

#UniqueID is not required as it is a dummy variable. Thus it can be removed.
Loan1 = Loan1[-1]

#Converting Employment.Type variable into factor as it is a character vector but should be
#a categorical variable.
Loan1$Employment.Type = as.factor(Loan1$Employment.Type)

#To see the distribution of each type of employment.
summary(Loan1$Employment.Type)

#MobileNo_Avl_Flag is affecting no variable & not needed as its min and max are 1. 
#Thus removing it entirely while model creation.
Loan1 = Loan1[-13]

#Converting AVERAGE.ACCT.AGE to numeric values

library(stringr)
avr = str_split(Loan1$AVERAGE.ACCT.AGE, " ")
avr1 = 1
avr2 = 1
for (i in 1:length(avr)) {avr1[i] = avr[[i]][1]}
for (i in 1:length(avr)) {avr2[i] = avr[[i]][2]}
avr1 = gsub("[a-zA-Z]","",avr1)
avr1 = ifelse(is.na(avr1),0,avr1)
avr1 = as.numeric(avr1)

avr2 = gsub("[a-zA-Z]","",avr2)
avr2 = ifelse(is.na(avr2),0,avr2)
avr2 = as.numeric(avr2)
avr2 = avr2/12

Loan1$AVERAGE.ACCT.AGE = avr1 + avr2

#Converting CREDIT.HISTORY.LENGTH to numeric values
avr = str_split(Loan1$CREDIT.HISTORY.LENGTH, " ")
avr1 = 1
avr2 = 1
for (i in 1:length(avr)) {avr1[i] = avr[[i]][1]}
for (i in 1:length(avr)) {avr2[i] = avr[[i]][2]}
avr1 = gsub("[a-zA-Z]","",avr1)
avr1 = ifelse(is.na(avr1),0,avr1)
avr1 = as.numeric(avr1)

avr2 = gsub("[a-zA-Z]","",avr2)
avr2 = ifelse(is.na(avr2),0,avr2)
avr2 = as.numeric(avr2)
avr2 = avr2/12

Loan1$CREDIT.HISTORY.LENGTH = avr1 + avr2

rm(avr)
rm(avr1)
rm(avr2)
rm(i)

#Using date of birth and disbursal date to calculate age at time of disbursal.
#Then removing date of birth and disbursal date as they are not needed anymore.
Loan1$Date.of.Birth = as.Date(Loan1$Date.of.Birth)
Loan1$DisbursalDate = as.Date(Loan1$DisbursalDate)
library(eeptools)
Loan1$Age = age_calc(Loan1$Date.of.Birth, Loan1$DisbursalDate, units = "years")
Loan1 = Loan1[-c(8,10)]

write_xlsx(Loan1, "D://Final_Data.xlsx")

#Outliers removal from disbursed_amount
LT = mean(Loan1$disbursed_amount) - 2*sd(Loan1$disbursed_amount)
UT = mean(Loan1$disbursed_amount) + 2*sd(Loan1$disbursed_amount)

Loan2 = subset(Loan1, Loan1$disbursed_amount < UT & Loan1$disbursed_amount > LT)

#Outliers removal from asset_cost
LT = mean(Loan2$asset_cost) - 2*sd(Loan2$asset_cost)
UT = mean(Loan2$asset_cost) + 2*sd(Loan2$asset_cost)

Loan3 = subset(Loan2, Loan2$asset_cost < UT & Loan2$asset_cost > LT)

#Making -ve values in PRI.CURRENT.BALANCE as zero.
Loan3$PRI.CURRENT.BALANCE = ifelse(Loan3$PRI.CURRENT.BALANCE < 0,0,Loan3$PRI.CURRENT.BALANCE)

LT = mean(Loan3$PRI.CURRENT.BALANCE) - 2*sd(Loan3$PRI.CURRENT.BALANCE)
UT = mean(Loan3$PRI.CURRENT.BALANCE) + 2*sd(Loan3$PRI.CURRENT.BALANCE)

Loan3 = subset(Loan3, Loan3$PRI.CURRENT.BALANCE < UT & Loan3$PRI.CURRENT.BALANCE > LT)

#Outlier removals in PRI.SANCTIONED.AMOUNT
LT = mean(Loan3$PRI.SANCTIONED.AMOUNT) - 2*sd(Loan3$PRI.SANCTIONED.AMOUNT)
UT = mean(Loan3$PRI.SANCTIONED.AMOUNT) + 2*sd(Loan3$PRI.SANCTIONED.AMOUNT)

Loan3 = subset(Loan3, Loan3$PRI.SANCTIONED.AMOUNT < UT & Loan3$PRI.SANCTIONED.AMOUNT > LT)

#Outlier removals in PRI.DISBURSED.AMOUNT
LT = mean(Loan3$PRI.DISBURSED.AMOUNT) - 2*sd(Loan3$PRI.DISBURSED.AMOUNT)
UT = mean(Loan3$PRI.DISBURSED.AMOUNT) + 2*sd(Loan3$PRI.DISBURSED.AMOUNT)

Loan3 = subset(Loan3, Loan3$PRI.DISBURSED.AMOUNT < UT & Loan3$PRI.DISBURSED.AMOUNT > LT)

#Removal of -ve values and outliers from SEC.CURRENT.BALANCE
Loan3$SEC.CURRENT.BALANCE = ifelse(Loan3$SEC.CURRENT.BALANCE < 0, 0, Loan3$SEC.CURRENT.BALANCE)

LT = mean(Loan3$SEC.CURRENT.BALANCE) - 2*sd(Loan3$SEC.CURRENT.BALANCE)
UT = mean(Loan3$SEC.CURRENT.BALANCE) + 2*sd(Loan3$SEC.CURRENT.BALANCE)

Loan3 = subset(Loan3, Loan3$SEC.CURRENT.BALANCE < UT & Loan3$SEC.CURRENT.BALANCE > LT)

#outlier removal from SEC.SANCTIONED.AMOUNT
LT = mean(Loan3$SEC.SANCTIONED.AMOUNT) - 2*sd(Loan3$SEC.SANCTIONED.AMOUNT)
UT = mean(Loan3$SEC.SANCTIONED.AMOUNT) + 2*sd(Loan3$SEC.SANCTIONED.AMOUNT)

Loan3 = subset(Loan3, Loan3$SEC.SANCTIONED.AMOUNT < UT & Loan3$SEC.SANCTIONED.AMOUNT > LT)

#outlier removal from SEC.DISBURSED.AMOUNT
LT = mean(Loan3$SEC.DISBURSED.AMOUNT) - 2*sd(Loan3$SEC.DISBURSED.AMOUNT)
UT = mean(Loan3$SEC.DISBURSED.AMOUNT) + 2*sd(Loan3$SEC.DISBURSED.AMOUNT)

Loan3 = subset(Loan3, Loan3$SEC.DISBURSED.AMOUNT < UT & Loan3$SEC.DISBURSED.AMOUNT > LT)

#outlier removal from PRIMARY.INSTAL.AMT
LT = mean(Loan3$PRIMARY.INSTAL.AMT) - 2*sd(Loan3$PRIMARY.INSTAL.AMT)
UT = mean(Loan3$PRIMARY.INSTAL.AMT) + 2*sd(Loan3$PRIMARY.INSTAL.AMT)

Loan3 = subset(Loan3, Loan3$PRIMARY.INSTAL.AMT < UT & Loan3$PRIMARY.INSTAL.AMT > LT)

#outlier removal from SEC.INSTAL.AMT
LT = mean(Loan3$SEC.INSTAL.AMT) - 2*sd(Loan3$SEC.INSTAL.AMT)
UT = mean(Loan3$SEC.INSTAL.AMT) + 2*sd(Loan3$SEC.INSTAL.AMT)

Loan3 = subset(Loan3, Loan3$SEC.INSTAL.AMT < UT & Loan3$SEC.INSTAL.AMT > LT)

#Scaling of data frame as it contains numeric variables of with huge variations in range.
Loan4 = scale(Loan3[c(1,2,3,16,18:36,38)])
Loan4 = as.data.frame(Loan4)
Loan4 = cbind(Loan4, Loan3[c(4:15,17,37)])

#Using the approch of omitting the observations with NA's present. This will remove all
#the observations in Employment.Type that had empty cells in the excel data source.
Loan5 = na.omit(Loan4)

#Since PERFORM_CNS.SCORE.DESCRIPTION is used to class the score of PERFORM_CNS.SCORE in
#various categories, thus using the approach of excluding PERFORM_CNS.SCORE.DESCRIPTION
#in the model creation.
Loan5 = Loan5[-37]

#Model Creation
library(caret)
set.seed(1)
intrain = createDataPartition(Loan5$loan_default, p = 0.8, list = F)
Train = Loan5[intrain,]
Test = Loan5[-intrain,]

model0 = glm(Train$loan_default ~ ., data = Train, family = binomial(link = "logit"))
library(MASS)

#Using AIC approach to get the model.
step0 = stepAIC(model0, direction = "both") 
summary(step0)

#Predicting values using model in the Test data created using createDataPartition function.
Pred = predict(step0, newdata = Test[,-37], type = "response")

Pred1 = ifelse(Pred < 0.4, 0, 1)

#Create a confusion matrix
library(e1071)
a = table(Test$loan_default, Pred1, dnn = list("actual", "predicted"))
a

caret::confusionMatrix(a)
