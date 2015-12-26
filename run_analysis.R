




run_analysis <- function() {
  url <-
    "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  
  zipFile <- "projFiles.zip"
  
  #check if data directory exists and create if not
  if (!file.exists("./data")) {
    dir.create("./data")
  }
  zipFilePath <- paste("./data",zipFile,sep = "/")
  #check if already downloaded the zip file
  if (!file.exists(zipFilePath))
    download.file(url = url, destfile = zipFilePath, method = "curl")
  
  #Unzip downloaded file
  unzip(zipfile = zipFilePath,exdir = "./data")
  
  #Extracted Folder: UCI HAR Dataset
  extractedfilePath <- file.path("./data" , "UCI HAR Dataset")
  files <- list.files(extractedfilePath, recursive = TRUE)
  list.files(files, recursive = TRUE)
  
  
  #Read Activity Files : Test  & Train
  datTestActivity <-
    read.table(file.path(filePath,"test","Y_test.txt"),header = FALSE)
  str(datTestActivity)
  datTrainActivity <-
    read.table(file.path(filePath,"train","Y_train.txt"),header = FALSE)
  str(datTrainActivity)
  
  #Read Subject Files :
  datTestSubject <-
    read.table(file.path(filePath,"test","subject_test.txt"),header = FALSE)
  str(datTestSubject)
  datTrainSubject <-
    read.table(file.path(filePath,"train","subject_train.txt"),header = FALSE)
  str(datTrainSubject)
  
  #Read Feature Files :
  datTestFeature <-
    read.table(file.path(filePath,"test","X_test.txt"),header = FALSE)
  str(datTestFeature)
  datTrainFeature <-
    read.table(file.path(filePath,"train","X_train.txt"),header = FALSE)
  str(datTrainFeature)
  
  #Row Merge/Append test and training Data
  datSubject <- rbind(datTestSubject,datTrainSubject)
  datActivity <- rbind(datTestActivity,datTrainActivity)
  datFeature <- rbind(datTestFeature,datTrainFeature)
  
  #Assign variable names
  names(datSubject)<-c("subject")
  names(datActivity)<- c("activity")
  
  featureNames <-
    read.table(paste(filePath,"features.txt", sep = "/"),header = FALSE)
  
  names(datFeature)<- featureNames$V2
  #setNames(datFeature, names(datFeature),featureNames$V2) 
  
  #Column merge the data
  d1 <- cbind(datSubject,datActivity)
  mergedData <- cbind(datFeature,d1)
  
  str(mergedData)
  
  # Subset Mean and SD data only
  subFeatureNames<-featureNames$V2[grep("mean\\(\\)|std\\(\\)", featureNames$V2)]
  
  selected <- c(as.character(subFeatureNames),"subject","activity")
  data <- subset(mergedData, select = selected)
  str(data)
  
  #Assign descriptive names to the activity
  lblActivity <-
    read.table(paste(filePath,"activity_labels.txt", sep = "/"),header = FALSE)
  
  str(lblActivity)
  head(data$activity,10)
  
  #assign descriptive names
  names(data) <- gsub("^t", "time", names(data))
  names(data) <- gsub("^f", "frequency", names(data))
  names(data) <- gsub("Acc", "Accelerometer", names(data))
  names(data) <- gsub("Gyro", "Gyroscope", names(data))
  names(data) <- gsub("Mag", "Magnitude", names(data))
  names(data) <- gsub("BodyBody", "Body", names(data))
  
  names(data)
  
  
  #create tidy data and write back to file
  tidyData <- aggregate(. ~ subject + activity, mergedData,mean)
  tidyData <- tidyData[order(tidyData$subject,tidyData$activity),]
  write.table(tidyData,file = "tidyData.txt", row.names = FALSE)
  
  summary(tidyData)
  
  knit("makeCodebook.Rmd", output = "codebook.md", encoding = "ISO8859-1", quiet = TRUE)
  markdownToHTML("codebook.md", "codebook.html")
  
  }
