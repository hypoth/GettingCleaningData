getwd()
library(dplyr)
#this script has to be exectued from inside "UCI HAR Dataset" folder
tf <- "train/X_train.txt"
x_train <- read.table(tf,header=FALSE)

tf <- "test/X_test.txt"
x_test <- read.table(tf,header=FALSE)

#create one signle data set for x as requirement 1
#Merges the training and the test sets to create one data set.
x <- rbind(x_train,x_test)

tf <- "train/y_train.txt"
y_train <- read.table(tf,header=FALSE)

tf <- "test/y_test.txt"
y_test <- read.table(tf,header=FALSE)

y<- rbind(y_train, y_test)


tf <- "activity_labels.txt"
yy <- read.table(tf,header=FALSE)

#labelling Activities as per 3rd requirement of assignment
#Uses descriptive activity names to name the activities in the data set
y_labeled <- merge(y,yy,by.x="V1",by.y="V1")

tf <- "train/subject_train.txt"
yy <- read.table(tf,header=FALSE)

tf <- "test/subject_test.txt"
yyTest <- read.table(tf,header=FALSE)

subject <- rbind(yy,yyTest)

tf <- "features.txt"
features <- read.table(tf,header=FALSE)

xMeanStd <- subject
colnames(xMeanStd) <- "Subject_Id"
yy <- select(y_labeled,2)
colnames(yy) <- "Activity"
xMeanStd <- cbind(xMeanStd,yy)
#set loop to rename column names as per requirement 2,4
#4. Appropriately labels the data set with descriptive variable names.
#2. Extracts only the measurements on the mean and standard deviation for each measurement
colIndex <- 1
for(nn in features$V2) { 
  wrds <- strsplit(nn,"-")
  for (splt in wrds) {
    if ("mean()" %in% splt) {
      xx <- select(x,colIndex)
      newColName <- paste(splt[1],splt[3],"mean", sep="_", collapse=NULL)
      colnames(xx) <- newColName
      xMeanStd <- cbind(xMeanStd,xx)
    }
    if ("std()" %in% splt) {
      xx <- select(x,colIndex)
      newColName <- paste(splt[1],splt[3],"std", sep="_", collapse=NULL)
      colnames(xx) <- newColName
      xMeanStd <- cbind(xMeanStd,xx)
    }
  }
  colIndex <- colIndex+1
}

xMeanStd$Subject_Id <- as.factor(xMeanStd$Subject_Id)
xMeanStd$Activity <- as.factor(xMeanStd$Activity)

#set loop to summarize data as average across subjects and activities.
#5. From the data set in step 4, creates a second, independent tidy data
#set with the average of each variable for each activity and each subject.
sd <- filter(xMeanStd, Subject_Id == 1)
res <- sd %>% group_by(Activity) %>% summarise_each(funs(mean))

for(i in 1:max(subject)){
  sd <- filter(xMeanStd, Subject_Id == i)
  tmpres <- sd %>% group_by(Activity) %>% summarise_each(funs(mean))
  res <- rbind(res,tmpres)
}
maxColIndex <- ncol(res)
res <- res[,c(2,1,3:maxColIndex)]
write.table(res,file="results.txt",row.names = FALSE)


