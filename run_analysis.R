##
## installing the needed library for this code
##
library(plyr)    
##
## Section 1. Getting the Data
##      a.  Create a working directory
##      b.	Download the data
##      c.	Extract the data from the zip file 

##
## If working directory does not exist it creates it.
if (!file.exists("Workdata")) {
    dir.create("Workdata")
}

##
## If source zip file does not exist in the working directory, it is downloaded and unziped.
## Since file is rather big it save some time for repetitive runs.
##
if (!file.exists("./Workdata/UCI_dataset.zip")) {
    
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl, destfile = "./Workdata/UCI_dataset.zip")
    unzip("./Workdata/UCI_dataset.zip",exdir="./Workdata", overwrite=TRUE)
}

##
## Section 2. Reading the data and Clipping it together
##

## let`s start by getting the activites and variable names

activity <- read.table("./Workdata/activity_labels.txt")
names(activity) <- c("activity_number","activity_description")
variable <- read.table("./Workdata/features.txt")
names(variable) <- c("variable","variable_description")

##
## Select the variables names we care for, simplify them by removing undesired characters and save them on a vertor for later use
##
mean_std <- tolower(gsub("\\)","",gsub("\\(","",gsub("\\-","", 
                                                     c(as.character(variable[grepl("-mean()",variable$variable_description,fixed=T),"variable_description"]), 
                                                       as.character(variable[grepl("-std()",variable$variable_description,fixed=T),"variable_description"]))))))


variable$variable_description <- tolower(gsub("\\)","",gsub("\\(","",gsub("\\-","",variable$variable_description))))

## now let´s get the test data

subject_data <- read.table("./Workdata/test/subject_test.txt")
x_data <- read.table("./Workdata/test/x_test.txt")

## giving meaningfull column names to the table by using the data provided on the features.txt and simplified removing undesired characters
names(x_data) <- variable$variable_description 

## Clips together all the different data files into a single data frame with only the columns we care for (mean and std)

y_data <- read.table("./Workdata/test/y_test.txt")

test_data <- data.frame("Subject"=subject_data[,], 
                        "Activity" = activity[y_data[,],"activity_description"], 
                        x_data[,mean_std])

## now it read the train data

subject_data <- read.table("./Workdata/train/subject_train.txt")
x_data <- read.table("./Workdata/train/x_train.txt")

## giving meaningfull column names to the table using the data provided on the features.txt   
names(x_data) <- variable$variable_description 

## Clips together all the different data files into a single data frame with only the columns we care for (mean and std)

y_data <- read.table("./Workdata/train/y_train.txt")

train_data <- data.frame("Subject"=subject_data[,], 
                         "Activity" = activity[y_data[,],"activity_description"], 
                         x_data[,mean_std])

## And merging the two data frames together

data <- rbind(test_data,train_data)

##
## Section 3: Calculate the average of each variable for each activity and each subject and generates the tidy data file
##

## Initialize the subject and activity variables on the data2 frame to use them to lead the summarization for the mean and std variables.
data2 <- data[,1:2]

## Loop to summarize each mean or std variable by subject and activity thru applying the function mean. 

for (i in 3:length(data)) {
    data2$tobesummarized <- data[,i]
    tempsummary <- ddply(data2,.(Subject,Activity),summarize,ave = round(mean(tobesummarized),2))
    if (i == 3){
        tidydata <- tempsummary
    }
    else {
        tidydata[,i] <- tempsummary$ave
    }
}

##  Let´s enhance the data set with descriptive variable names

names(tidydata) <- names(data)

## Finally we write the tidy output file

write.table(tidydata,"averageactivity.txt",row.name=FALSE)
