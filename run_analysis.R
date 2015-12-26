


run_analysis <- function() {
  url <-
    "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  
  #check if data directory exists and create if not 
  if(!file.exists("./data")){dir.create("./data")}
  #check if already downloaded the zip file
  if(!file.exists("./data/projFiles.zip"))
    download.file(url = url,destfile = "./data/projFiles.zip", method = "curl")

  #Unzip downloaded file
  unzip(zipfile="./data/projFiles.zip",exdir="./data")
  
  #Extracted Folder: UCI HAR Dataset 
  filePath <- file.path("./data" , "UCI HAR Dataset")
  files<-list.files(filePath, recursive=TRUE)
  #files
  
  
  #Read Activity Files : Test  & Train
  datTestActivity <- read.table(file.path(filePath,"test","Y_test.txt"),header = FALSE)
  #str(datTestActivity)
  datTrainActivity <- read.table(file.path(filePath,"train","Y_train.txt"),header = FALSE)
  #str(datTrainActivity)
  
  #Read Subject Files : 
  datTestSubject <- read.table(file.path(filePath,"test","subject_test.txt"),header = FALSE)
  #str(datTestSubject)
  datTrainSubject <- read.table(file.path(filePath,"train","subject_train.txt"),header = FALSE)
  #str(datTrainSubject)
  
  #Read Feature Files :
  datTestFeature <- read.table(file.path(filePath,"test","X_test.txt"),header = FALSE)
  #str(datTestFeature)
  datTrainFeature <- read.table(file.path(filePath,"train","X_train.txt"),header = FALSE)
  #str(datTrainFeature)
  
  #Row Merge test and training Data
  datSubject <- rbind(datTestSubject,datTrainSubject)
  datActivity <- rbind(datTestActivity,datTrainActivity)
  datFeature <- rbind(datTestFeature,datTrainFeature)
  
  #Assign variable names
  names(datSubject) <- c("subject")
  names(datActivity) <-c("activity")
  featureNames <- read.table(paste(filePath,"features.txt", sep="/"),header = FALSE)
  names(datFeature) <- featureNames$V2
  
  #Column merge the data
  mergedData <- cbind(datSubject,datFeature,datActivity)
  
  #str(mergedData)
  
  #Assign descriptive names to the activity
  lblActivity <- read.table(paste(filePath,"activity_labels.txt", sep="/"),header = FALSE)
  #str(lblActivity)
  #head(mergedData$activity,10)
  
  #assign descriptive names
  names(mergedData) <- gsub("^t", "time", names(mergedData))
  names(mergedData)<-gsub("^f", "frequency", names(mergedData))
  names(mergedData)<-gsub("Acc", "Accelerometer", names(mergedData))
  names(mergedData)<-gsub("Gyro", "Gyroscope", names(mergedData))
  names(mergedData)<-gsub("Mag", "Magnitude", names(mergedData))
  names(mergedData)<-gsub("BodyBody", "Body", names(mergedData))
  
  #names(mergedData)
  
  
  #create tidy data and write back to file
  tidyData <- aggregate(. ~subject + activity, mergedData,mean)
  tidyData <- tidyData[order(tidyData$subject,tidyData$activity),]
  write.table(tidyData,file="tidyData.txt", row.names = FALSE)

  
  knit2html("codebook.Rmd");
}

