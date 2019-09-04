# Course Project of Claudio Cerda
if(!file.exists("./data")) {dir.create("./data")}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"  
download.file(url,
              destfile='HAR.zip',
              method="curl", 
              mode="wb") 
unzip(zipfile = "HAR.zip") # unpack the files into subdirectories 
test <- read.table("~/data/UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="")
train <- read.table("~/data/UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="")
activ_test <- read.table("~/data/UCI HAR Dataset/test/y_test.txt", quote="\"", comment.char="")
activ_train <- read.table("~/data/UCI HAR Dataset/train/y_train.txt", quote="\"", comment.char="")
subject_test <- read.table("~/data/UCI HAR Dataset/test/subject_test.txt", quote="\"", comment.char="")
subject_train <-  read.table("~/data/UCI HAR Dataset/train/subject_train.txt", quote="\"", comment.char="")
var_names <- read.table("~/data/UCI HAR Dataset/features.txt", quote="\"", comment.char="")
var_names2 <- var_names[, 2]
names(activ_test) <- "Activity"
names(activ_train) <- "Activity"
names(subject_test) <- "ID_subject"
names(subject_train) <- "ID_subject"
names(test) <- var_names2
names(train) <- var_names2
tes_act_sub <- cbind(subject_test, activ_test, test)
tra_act_sub <- cbind(subject_train, activ_train, train)
df_tes_tra <- rbind(tra_act_sub, tes_act_sub)
library(dplyr)
names_df <- names(df_tes_tra)  
dup_names <- duplicated(names_df)
df_uni <- df_tes_tra[ , !(dup_names)]
names_uni <- names(df_uni)
df_uni_sel <- select(df_uni, ID_subject, Activity, contains("mean"), contains("Mean"), contains("std"))
df_uni_sel$Activity <- recode(df_uni_sel$Activity, "1" = "Walking", "2" = "Walking_Upstairs", "3" = "Walking_Downstairs", "4" = "Sitting", "5" = "Standing", "6" = "Laying")
