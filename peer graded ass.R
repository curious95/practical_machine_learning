library(dplyr)
library(caret)
library(corrplot)

train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if(!file.exists("pml-training.csv") || !file.exists("pml-testing.csv")){
  #download.file(train_url,method = "curl",destfile = "pml-training.csv")
  download.file(test_url,method = "curl",destfile = "pml-testing.csv")
  downloadDate<- date()
  
  train_data<-read.csv("pml-training.csv",sep = ",",header = TRUE,na.strings = c("",NA))
  test_data<-read.csv("pml-testing.csv",sep = ",",header = TRUE,na.strings = c("",NA))
  
}else{
  train_data<-read.csv("pml-training.csv",sep = ",",header = TRUE,na.strings = c("",NA))
  test_data<-read.csv("pml-testing.csv",sep = ",",header = TRUE,na.strings = c("",NA))
}

#removing the columns from both test and training data set where 50% of values in columns are empty

#Marking columns for removal
removed_cols<-colnames(train_data[,colSums(is.na(train_data)) > nrow(train_data)*0.50])

#Removing from training set
train_data<-train_data[,!names(train_data) %in% removed_cols]

#Removing from test set
test_data<- test_data[,!names(test_data) %in% removed_cols]

#Marking Unnecessary columns
un_cols<- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2","cvtd_timestamp", "new_window", "num_window")

#Removing Unnecessary columns from training set
train_data<-train_data[,!names(train_data) %in% un_cols]

#Removing Unnecessary columns from test set
test_data<- test_data[,!names(test_data) %in% un_cols]
dim(train_data)
dim(test_data)


corrs<-cor(train_data[,-53])
corrs[corrs==1]<-NA
corrs[abs(corrs)<0.5]<-NA
corrs<-na.omit(melt(corrs))
corrs[order(-abs(corrs$value)),][1:20,]


#cart
Dtree_model <- train(
  classe ~ ., 
  data=training[, c('classe', nznames)],
  trControl=fitControl,
  method='rpart'
)

