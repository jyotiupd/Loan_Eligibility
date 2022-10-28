

##Loading the required libraries
library(dplyr)
library(ggplot2)
library(car)


##Setting the data path
setwd("E:/Data Science/Fractal_Analytics/")
getwd()

##Loading the data
train=read.csv("train.csv",stringsAsFactors = F)

test=read.csv("test.csv",stringsAsFactors = F)

##Serialising the columns of both test and train data

test=test%>%
  select(ID,Item_ID,Datetime,Category_1,Category_2,Category_3)


train=train%>%
  select(ID,Item_ID,Datetime,Category_1,Category_2,Category_3,Number_Of_Sales,Price)

##Since test do not consist of columns N.O.S and Price impute with NAs
test$Number_Of_Sales=NA

test$Price=NA

##Binding the whole data into Full data frame
full=rbind(train,test)

##length(unique(full1$ID))

##r<- gsub('_','','31149_20140528')

##Assigning the value of full to full1

full1=full
#glimpse(full1)

##Converting the character data to numeric

full1=full%>%
  mutate(ID=as.numeric(gsub('_','',ID)))

full1=full1%>%
  mutate(Datetime=as.integer(gsub("-","", Datetime)))

##Imputing the NAs of Category_2 to 0

full1=full1%>%
  mutate(Category_2=ifelse(is.na(Category_2),2.5,Category_2))

##Splitting the data again into train and test
train_new=full1[1:881876,]
test_new=full1[881877:nrow(full1),]

# p=ggplot(train,aes(x=Datetime,y=Price,color=Number_Of_Sales))
#          
# p+geom_line()
# 
# 
# glimpse(full1)


###Implementing the Linear regression to find Price
fit_price=lm(Price~.-ID-Number_Of_Sales,data=train_new)

##Checking for high vif values
vif(fit_price)

#Checking for high p values
summary(fit_price)




test_new=test_new %>%
  mutate(Price=predict(fit_price,newdata=test_new))

#test_new$Price=predict(fit_price,newdata=test_new)


###Implementing the Linear regression to find Number of sales
fit_nos=lm(Number_Of_Sales~.-ID-Price,data=train_new)

vif(fit_nos)
summary(fit_nos)

test_new$Number_Of_Sales=as.integer(predict(fit_nos,newdata=test_new))


# train_new %>%
#   mutate(pred_sales=predict(fit_nos,newdata=train_new)) %>%
#   ggplot(aes(x=Number_Of_Sales,y=pred_sales))+geom_point(alpha=0.6)


solution=test_new[,c(1,7,8)]

solution$ID=as.character(solution$ID)

solution=solution%>%
  mutate(ID=paste(substr(ID,1,5),substr(ID,6,13),sep='_'))



write.csv(solution,file='solution.csv',row.names=F)




ls()

rm("train_new")

