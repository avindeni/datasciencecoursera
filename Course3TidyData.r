run_analysis <- function () {

library(data.table)
fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if (!file.exists('./UCI HAR Dataset.zip')){
  download.file(fileurl,'./UCI HAR Dataset.zip', mode = 'wb')
  unzip("UCI HAR Dataset.zip", exdir = getwd())
}

features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])

data.train.x <- read.table('./UCI HAR Dataset/train/X_train.txt')
data.train.activity <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
data.train.subject <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

data.train <-  data.frame(data.train.subject, data.train.activity, data.train.x)
names(data.train) <- c(c('subject', 'activity'), features)

data_test <- read.table('./UCI HAR Dataset/test/X_test.txt')
data_test_activity <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
data_test_subject <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

data_test <-  data.frame(data_test_subject, data_test_activity, data_test)
names(data_test) <- c(c('subject', 'activity'), features)

data_all <- rbind(data.train, data_test)

mean_std_select <- grep('mean|std', features)
data_sub <- data_all[,c(1,2,mean_std_select + 2)]

activity_labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
activity_labels <- as.character(activity_labels[,2])
data_sub$activity <- activity_labels[data_sub$activity]

name_new <- names(data_sub)
name_new <- gsub("[(][)]", "", name_new)
name_new <- gsub("^t", "TimeDomain_", name_new)
name_new <- gsub("^f", "FrequencyDomain_", name_new)
name_new <- gsub("Acc", "Accelerometer", name_new)
name_new <- gsub("Gyro", "Gyroscope", name_new)
name_new <- gsub("Mag", "Magnitude", name_new)
name_new <- gsub("-mean-", "_Mean_", name_new)
name_new <- gsub("-std-", "_StandardDeviation_", name_new)
name_new <- gsub("-", "_", name_new)
names(data_sub) <- name_new

data_tidy <- aggregate(data_sub[,3:81], by = list(activity = data_sub$activity, subject = data_sub$subject),FUN = mean)
write.table(x = data_tidy, file = "data_tidy.txt", row.names = FALSE)

}