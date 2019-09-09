# Course Project of Claudio Cerda
if(!file.exists("./data")) {dir.create("./data")}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"  
download.file(url,
              destfile='HAR.zip',
              method="curl", 
              mode="wb") 
unzip(zipfile = "HAR.zip") # unpack the files into subdirectories 

# read the test data
test <- read.table("~/data/UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="")

# read the train data
train <- read.table("~/data/UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="")

# read the activity data
activ_test <- read.table("~/data/UCI HAR Dataset/test/y_test.txt", quote="\"", comment.char="")
activ_train <- read.table("~/data/UCI HAR Dataset/train/y_train.txt", quote="\"", comment.char="")

# read the subject data
subject_test <- read.table("~/data/UCI HAR Dataset/test/subject_test.txt", quote="\"", comment.char="")
subject_train <-  read.table("~/data/UCI HAR Dataset/train/subject_train.txt", quote="\"", comment.char="")

# read the names of variables for the test and train data
var_names <- read.table("~/data/UCI HAR Dataset/features.txt", quote="\"", comment.char="")
var_names2 <- var_names[, 2]

# set the colnames to the test data
names(test) <- var_names2

# set the colnames to the train data
names(train) <- var_names2

# set the colnames to activity and subject data
names(activ_test) <- "Activity"
names(activ_train) <- "Activity"
names(subject_test) <- "Subject"
names(subject_train) <- "Subject"

# set colnames to the train data
names(train) <- var_names2

# bind the test, activity and subject data 
tes_act_sub <- cbind(subject_test, activ_test, test)

# bind the train, activity and subject data 
tra_act_sub <- cbind(subject_train, activ_train, train)

# bind train and test data
df_tes_tra <- rbind(tes_act_sub, tra_act_sub)
library(dplyr)
names_df <- names(df_tes_tra)  

# identify the duplicates col names (to eliminate them, because do not contain mean or std 
# and don´t allow use the select function)
dup_names <- duplicated(names_df)
df_uni <- df_tes_tra[ , !(dup_names)]

# select the variables (columns) that contain mean, Mean or std
df_uni_sel <- select(df_uni, Subject, Activity, contains("mean"), contains("Mean"), contains("std"))

# Uses descriptive activity names to name the activities in the data set
df_uni_sel$Activity <- recode(df_uni_sel$Activity, "1" = "Walking", "2" = "Walking_Upstairs", "3" = "Walking_Downstairs", "4" = "Sitting", "5" = "Standing", "6" = "Laying")

# rename de column name from 1:88
names(df_uni_sel) <- 1:88

# Grouping for subject (1) and activity (2)
df_group <- group_by(df_uni_sel, `1`, `2`)

# create an independent tidy data set with the average of each variable for each activity and each subject
df_sum <- summarize(df_group, tBAmX  = mean(`3`, na.rm = TRUE),
                    tBAmY  = mean(`4`, na.rm = TRUE),
                    tBAmZ  = mean(`5`, na.rm = TRUE),
                    tBAsX  = mean(`6`, na.rm = TRUE),
                    tBAsY  = mean(`7`, na.rm = TRUE),
                    tBAsZ  = mean(`8`, na.rm = TRUE),
                    tGAmX  = mean(`9`, na.rm = TRUE),
                    tGAmY  = mean(`10`, na.rm = TRUE),
                    tGAmZ  = mean(`11`, na.rm = TRUE),
                    tGAsX  = mean(`12`, na.rm = TRUE),
                    tGAsY  = mean(`13`, na.rm = TRUE),
                    tGAsZ  = mean(`14`, na.rm = TRUE),
                    tBAJmX  = mean(`15`, na.rm = TRUE),
                    tBAJmY  = mean(`16`, na.rm = TRUE),
                    tBAJmZ  = mean(`17`, na.rm = TRUE),
                    tBAJsX  = mean(`18`, na.rm = TRUE),
                    tBAJsY  = mean(`19`, na.rm = TRUE),
                    tBAJsZ  = mean(`20`, na.rm = TRUE),
                    tBGmX  = mean(`21`, na.rm = TRUE),
                    tBGmY  = mean(`22`, na.rm = TRUE),
                    tBGmZ  = mean(`23`, na.rm = TRUE),
                    tBGsX  = mean(`24`, na.rm = TRUE),
                    tBGsY  = mean(`25`, na.rm = TRUE),
                    tBGsZ  = mean(`26`, na.rm = TRUE),
                    tBGJmX  = mean(`27`, na.rm = TRUE),
                    tBGJmY  = mean(`28`, na.rm = TRUE),
                    tBGJmZ  = mean(`29`, na.rm = TRUE),
                    tBGJsX  = mean(`30`, na.rm = TRUE),
                    tBGJsY  = mean(`31`, na.rm = TRUE),
                    tBGJsZ  = mean(`32`, na.rm = TRUE),
                    tBAm  = mean(`33`, na.rm = TRUE),
                    tBAs  = mean(`34`, na.rm = TRUE),
                    tGAm  = mean(`35`, na.rm = TRUE),
                    tGAs  = mean(`36`, na.rm = TRUE),
                    tBAJMm  = mean(`37`, na.rm = TRUE),
                    tBAJMs  = mean(`38`, na.rm = TRUE),
                    tBGMm  = mean(`39`, na.rm = TRUE),
                    tBGMs  = mean(`40`, na.rm = TRUE),
                    tBGJMm  = mean(`41`, na.rm = TRUE),
                    tBGJMs  = mean(`42`, na.rm = TRUE),
                    fBAmX  = mean(`43`, na.rm = TRUE),
                    fBAmY  = mean(`44`, na.rm = TRUE),
                    fBAmZ  = mean(`45`, na.rm = TRUE), 
                    fBAsX  = mean(`46`, na.rm = TRUE),
                    fBAsY  = mean(`47`, na.rm = TRUE),
                    fBAsZ  = mean(`48`, na.rm = TRUE),
                    fBAmX  = mean(`49`, na.rm = TRUE),
                    fBAmY  = mean(`50`, na.rm = TRUE),
                    fBAmZ  = mean(`51`, na.rm = TRUE),
                    fBAJmX  = mean(`52`, na.rm = TRUE),
                    fBAJmY  = mean(`53`, na.rm = TRUE),
                    fBAJmZ  = mean(`54`, na.rm = TRUE),
                    fBAJsX  = mean(`55`, na.rm = TRUE),
                    fBAJsY  = mean(`56`, na.rm = TRUE),
                    fBAJsZ = mean(`57`, na.rm = TRUE),
                    fBAJmFX = mean(`58`, na.rm = TRUE),
                    fBAJmFY = mean(`59`, na.rm = TRUE),
                    fBAJmFZ = mean(`60`, na.rm = TRUE),
                    fBGmX = mean(`61`, na.rm = TRUE),
                    fBGmY = mean(`62`, na.rm = TRUE),
                    fBGmZ = mean(`63`, na.rm = TRUE),
                    fBGsX = mean(`64`, na.rm = TRUE),
                    fBGsY = mean(`65`, na.rm = TRUE),
                    fBGsZ = mean(`66`, na.rm = TRUE),
                    fBGmFX = mean(`67`, na.rm = TRUE),
                    fBGmFY = mean(`68`, na.rm = TRUE),
                    fBGmFZ = mean(`69`, na.rm = TRUE),
                    fBAMm = mean(`70`, na.rm = TRUE),
                    fBAMs = mean(`71`, na.rm = TRUE),
                    fBAMmF = mean(`72`, na.rm = TRUE),
                    fBBAJMm = mean(`73`, na.rm = TRUE),
                    fBBAJMs = mean(`74`, na.rm = TRUE),
                    fBBAJMmF = mean(`75`, na.rm = TRUE),
                    fBBGMm = mean(`76`, na.rm = TRUE),
                    fBBGMs = mean(`77`, na.rm = TRUE),
                    fBBGMmF = mean(`78`, na.rm = TRUE),
                    fBBGJMm = mean(`79`, na.rm = TRUE),
                    fBBGJMs = mean(`80`, na.rm = TRUE),
                    fBBGJMmF = mean(`81`, na.rm = TRUE),
                    atBAMg = mean(`82`, na.rm = TRUE),
                    atBAJMgM = mean(`83`, na.rm = TRUE),
                    atBGMgM = mean(`84`, na.rm = TRUE),
                    atBGJMgM = mean(`85`, na.rm = TRUE),
                    aXgM = mean(`86`, na.rm = TRUE),
                    aYgM = mean(`87`, na.rm = TRUE),
                    aZgM = mean(`88`, na.rm = TRUE))
df_sum
tidy_average <- as.data.frame(df_sum)

# write the files required
write.table(x = df_uni_sel, file = "Course Project.txt", sep = "\t", row.names = FALSE)
write.table(x = tidy_average, file = "Tidy Average.txt", sep = "\t", row.names = FALSE)


