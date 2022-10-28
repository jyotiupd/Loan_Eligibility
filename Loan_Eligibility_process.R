
##Loading the packages
library(dplyr)
library(mice)
library(randomForest)
library(car)

##Loading the data

setwd("E:/Data Science/DataScience/CodeGladiators-ITCinfotech-DataSet")

getwd()

train=read.csv("train_data.csv",stringsAsFactors = F)

test=read.csv("test_data.csv",stringsAsFactors = F)

##Combining the data into a single data frame
full=bind_rows(train,test)


str(full)



##working on dummy variables


##Marking the missing values as unknown for character variables
full=full %>%
  mutate(Gender=ifelse(Gender=="","Unknown",Gender))
         
full=full %>%
  mutate(Married=ifelse(Married=="","Unknown",Married))
  
full=full %>%
  mutate(Self_Employed=ifelse(Self_Employed=="","Unknown",Self_Employed))

##Marking the missing values as unknown for Dependents variable as it can be 
##easily converted to numeric w\o dummy variable
full=full %>%
  mutate(Dependents=ifelse(Dependents=="","0",Dependents))
#Creating Dummy variables for character variables:-
full=full %>%
  mutate(dummy_male=as.numeric(Gender=="M"),
         dummy_female=as.numeric(Gender=="F")
  )%>%
  select(-Gender)

full=full %>%
  mutate(dummy_married=as.numeric(Married=="Yes")
  )%>%
  select(-Married)

##Converting Dependents directly to numeric w\o dummy
full=full%>%
  mutate(Dependents=as.numeric(ifelse(Dependents=="3+",3,Dependents)))

full=full%>%
  mutate(dummy_graduate=as.numeric(Education=="Graduate"))%>%
  select(-Education)

full=full%>%
  mutate(dummy_selfEmployed=as.numeric(Self_Employed=="Yes"),
         dumm_notSelfemployed=as.numeric(Self_Employed=="No"))%>%
  select(-Self_Employed)

full=full%>%
  mutate(dummy_urban=as.numeric(Property_Area=="Urban"),
         dummy_Semiurban=as.numeric(Property_Area=="Semiurban"))%>%
  select(-Property_Area)

full=full%>%
  mutate(Loan_Status=as.numeric(Loan_Status=="Y"))

###Imputing NAs
##Replace the NA in loan amount  with the average of loan amount

full=full%>%
  mutate(LoanAmount=ifelse(is.na(LoanAmount)==T,mean(LoanAmount,na.rm=T),LoanAmount))


##Replace the NA in loan amount terms with mean of term

full=full%>%
  mutate(Loan_Amount_Term=ifelse(is.na(Loan_Amount_Term)==T,mean(Loan_Amount_Term,na.rm=T),Loan_Amount_Term))
full1=full

##Will use the mice packeage of R to impute NAs of Credit history variable

init = mice(full, maxit=0) 
meth = init$method
predM = init$predictorMatrix

predM[, c("Application_ID")]=0

meth[c("Loan_Status")]=""

meth[c("Credit_History")]="norm" 

set.seed(103)
full1 = mice(full, method=meth, predictorMatrix=predM, m=5)

full1<-complete(full1)



sapply(full1, function(x) sum(is.na(x)))



#After cleansing the data break it into train and test
train=full1[1:100,]
test=full1[101:614,]

##Breaking the train into validation test and train
set.seed(123)

s=sample(1:nrow(train),0.7*nrow(train))

val_train= train[s,]
val_test=train[-s,]

val_trainlm=val_train
val_testlm=val_test

##First we remove multi collinearity by removing the variables with high vif values one by one

fit_val=lm(Loan_Status~.,data=val_trainlm)

vif(fit_val)

fit_val=lm(Loan_Status~.-dummy_female,data=val_trainlm)

vif(fit_val)

##Building a logistic regression model.
fit_val1=glm(Loan_Status~.-dummy_female,family="binomial",data=val_trainlm)

##Remove the variables with p values using the step function

fit_val1=step(fit_val1)

formula(fit_val1)

fit_val1=glm(Loan_Status ~ Credit_History  + dummy_urban + dummy_Semiurban  
             ,family="binomial",data=val_trainlm)

summary(fit_val1)

fit_val1=glm(Loan_Status ~ Credit_History   + dummy_Semiurban  
             ,family="binomial",data=val_trainlm)

summary(fit_val1)

fit_val1=glm(Loan_Status ~ Credit_History   
             ,family="binomial",data=val_trainlm)

##Predicting the scores on validation train data

val_trainlm$score=predict(fit_val1,newdata = val_trainlm,type = "response")

##Finding out cutt off value through KS

cutoff_data=data.frame(cutoff=0, KS=99)
cutoffs=seq(0,1,length=1000)
for (cutoff in cutoffs){
  predicted=as.numeric(val_trainlm$score>cutoff)
  TP=sum(predicted==1 & val_trainlm$Loan_Status==1)
  FP=sum(predicted==1 & val_trainlm$Loan_Status==0)
  FN=sum(predicted==0 & val_trainlm$Loan_Status==1)
  TN=sum(predicted==0 & val_trainlm$Loan_Status==0)
  P=TP+FN
  N=TN+FP
  Sn=TP/P
  KS=Sn - (FP/N)
  cutoff_data=rbind(cutoff_data,c(cutoff,KS))
}
#remove the dummy data cotaining top row
cutoff_data=cutoff_data[-1,]

cutoff_KS=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]

##Predicting the scores on validation test data

val_testlm$score=predict(fit_val1,newdata = val_testlm,type = "response")

##Comparing the real and predicted loan status values
table(val_testlm$Loan_Status,as.numeric(val_testlm$score>cutoff_KS))

##Missclassification=10/30=0.33i
##Sensitivity=14/16=0.875
##Specificity =6/14=0.4285714

#Prediction on validation data using Random forest

val_train_rf=val_train
val_test_rf=val_test

val_train_rf$Loan_Status=as.factor(ifelse(val_train_rf$Loan_Status==1,"yes","no"))


##loading the randomforest library
class_rf=randomForest(Loan_Status~.,data=val_train_rf) #building random forest model on train data
#class_rf

forest.pred=predict(class_rf,newdata=val_test_rf)##predicting on test data to check the performance
table(val_test_rf$Loan_Status,forest.pred)##Comparing the real and predicted loan status values

##Misclassification rate:- 9/30=0.3
##Sensitivity:- 15/16=0.93
##Specificity :-6/14= 0.4285714




####Random Forest model on test data

train_rf=train
test_rf=test

train_rf$Loan_Status=as.factor(ifelse(train_rf$Loan_Status==1,"Y","N"))

class_rf=randomForest(Loan_Status~.,data=train_rf) #building random forest model on train data
#class_rf

forest.pred=predict(class_rf,newdata=test_rf)##predicting on test data 

test_rf$Loan_Status=forest.pred

solution=test_rf[,c(1,8)]

write.csv(solution,file='solution.csv',row.names=F)