library(dplyr)
library(data.table)
#Download and unzip data
download.file(paste("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",sep=""),
              destfile=paste(getwd(),"/Data/FprojectfilesData.zip","",sep=""),method="curl",mode="wb")
unzip(paste(getwd(),"/Data/","FprojectfilesData.zip",sep=""),exdir=paste("Data/."))

# Load the data
YTest <- read.table(paste(getwd(),"/Data/","UCI HAR Dataset/test/y_test.txt",sep=""))
XTest <- read.table(paste(getwd(),"/Data/","UCI HAR Dataset/test/X_test.txt",sep=""))
STest <- read.table(paste(getwd(),"/Data/","UCI HAR Dataset/test/subject_test.txt",sep=""))
YTrain <- read.table(paste(getwd(),"/Data/","UCI HAR Dataset/train/y_train.txt",sep=""))
XTrain <- read.table(paste(getwd(),"/Data/","UCI HAR Dataset/train/X_train.txt",sep=""))
STrain <- read.table(paste(getwd(),"/Data/","UCI HAR Dataset/train/subject_train.txt",sep=""))
Feat <- read.table(paste(getwd(),"/Data/","UCI HAR Dataset/features.txt",sep=""))

# Map the column names
colnames(XTrain) <- t(Feat[2])
colnames(XTest) <- t(Feat[2])

# Map the activities
XTrain$activities <- YTrain[, 1]
XTrain$participants <- STrain[, 1]
XTest$activities <- YTest[, 1]
XTest$participants <- STest[, 1]

# 1: Join the data sets
FullSet <- rbind(XTrain, XTest)
# 2: Remove duplicated columns
FullSet <- FullSet[, !duplicated(colnames(FullSet))]

# 3: Extract only mean and standard deviation and exclude the gravity terms
FullSubSetM <- grep("mean()", names(FullSet), value = FALSE, fixed = TRUE)
FullSubSetM<- append(FullSubSetM, 471:477)
FullSetMean <- FullSet[FullSubSetM]
FullSubSetS <- grep("std()", names(FullSet), value = FALSE)
FullSetSTD <- FullSet[FullSubSetS]

# 4: Clean up the activity names
FullSet$activities <- as.character(FullSet$activities)
FullSet$activities[FullSet$activities == 1] <- "Walking"
FullSet$activities[FullSet$activities == 2] <- "Walking Upstairs"
FullSet$activities[FullSet$activities == 3] <- "Walking Downstairs"
FullSet$activities[FullSet$activities == 4] <- "Sitting"
FullSet$activities[FullSet$activities == 5] <- "Standing"
FullSet$activities[FullSet$activities == 6] <- "Laying"
FullSet$activities <- as.factor(FullSet$activities)

# 5: Clean up the variable names.
names(FullSet) <- gsub("Acc", "Accelerator", names(FullSet))
names(FullSet) <- gsub("Mag", "Magnitude", names(FullSet))
names(FullSet) <- gsub("Gyro", "Gyroscope", names(FullSet))
names(FullSet) <- gsub("^t", "time", names(FullSet))
names(FullSet) <- gsub("^f", "frequency", names(FullSet))

# 6: Clean up the participants names
FullSet$participants <- as.character(FullSet$participants)
for (i in 1:30){   
    FullSet$participants[FullSet$participants == i] <- paste("Participant",i)
}
FullSet$participants <- as.factor(FullSet$participants)

# 7: Create the final data set
FullSetTable<- data.table(FullSet)
#This takes the mean of every column broken down by participants and activities
FinalData <- FullSetTable[, lapply(.SD, mean), by = 'participants,activities']
write.table(FinalData, file = "FinalData.txt", row.names = FALSE)


