# Abhinav Batra


# Load required libraries
library(dplyr)
library(stringr)

# Load activity and feature labels
activity_labels <- read.table("./activity_labels.txt", col.names = c("ActivityID", "ActivityName"))
feature_labels <- read.table("./features.txt", col.names = c("FeatureID", "FeatureName"))

# Load training data
train_activity <- read.table("./train/y_train.txt", col.names = "ActivityID")
train_subject <- read.table("./train/subject_train.txt", col.names = "SubjectID")
train_data <- read.table("./train/X_train.txt")
# Assign feature labels and remove all columns except mean and std
names(train_data) <- feature_labels[,2]
train_short <- train_data[, grep("-mean\\(\\)|-std\\(\\)", feature_labels[,2])]
feature_labels <- feature_labels[grep("-mean\\(\\)|-std\\(\\)", feature_labels[,2]),2]
# Clean feature labels to user-readable format
feature_labels <- gsub("^f","Freq",gsub("^t","Time",gsub("std","Std",gsub("mean","Mean",gsub("\\(\\)","",gsub("-","",feature_labels))))))
names(train_short) <- feature_labels
# Merge activity labels with rest of the data to create tidy training set
train_activity_merge <- merge(train_activity, activity_labels, by = "ActivityID", sort = FALSE)
train_tidy <- cbind(DataType = rep("Train", nrow(train_data)), ActivityName = train_activity_merge[,2], train_subject, train_short)

# Reload feature labels
feature_labels <- read.table("./features.txt", col.names = c("FeatureID", "FeatureName"))

# Load test data
test_activity <- read.table("./test/y_test.txt", col.names = "ActivityID")
test_subject <- read.table("./test/subject_test.txt", col.names = "SubjectID")
test_data <- read.table("./test/X_test.txt")
# Assign feature labels and remove all columns except mean and std
names(test_data) <- feature_labels[,2]
test_short <- test_data[, grep("-mean\\(\\)|-std\\(\\)", feature_labels[,2])]
feature_labels <- feature_labels[grep("-mean\\(\\)|-std\\(\\)", feature_labels[,2]),2]
# Clean feature labels to user-readable format
feature_labels <- gsub("^f","Freq",gsub("^t","Time",gsub("std","Std",gsub("mean","Mean",gsub("\\(\\)","",gsub("-","",feature_labels))))))
names(test_short) <- feature_labels
# Merge activity labels with rest of the data to create tidy training set
test_activity_merge <- merge(test_activity, activity_labels, by = "ActivityID")
test_tidy <- cbind(DataType = rep("Test", nrow(test_data)), ActivityName = test_activity_merge[,2], test_subject, test_short)

# Union train and test to create full tidy data and average tidy dataset
full_tidy <- rbind(train_tidy, test_tidy)
average_tidy <- full_tidy %>% 
                group_by(ActivityName, SubjectID) %>% 
                summarize(AvgTimeBodyAccMeanX = mean(TimeBodyAccMeanX, na.rm = TRUE),
                          AvgTimeBodyAccMeanY = mean(TimeBodyAccMeanY, na.rm = TRUE),
                          AvgTimeBodyAccMeanZ = mean(TimeBodyAccMeanZ, na.rm = TRUE),
                          AvgTimeBodyAccStdX = mean(TimeBodyAccStdX, na.rm = TRUE),
                          AvgTimeBodyAccStdY = mean(TimeBodyAccStdY, na.rm = TRUE),
                          AvgTimeBodyAccStdZ = mean(TimeBodyAccStdZ, na.rm = TRUE),
                          AvgTimeGravityAccMeanX = mean(TimeGravityAccMeanX, na.rm = TRUE),
                          AvgTimeGravityAccMeanY = mean(TimeGravityAccMeanY, na.rm = TRUE),
                          AvgTimeGravityAccMeanZ = mean(TimeGravityAccMeanZ, na.rm = TRUE),
                          AvgTimeGravityAccStdX = mean(TimeGravityAccStdX, na.rm = TRUE),
                          AvgTimeGravityAccStdY = mean(TimeGravityAccStdY, na.rm = TRUE),
                          AvgTimeGravityAccStdZ = mean(TimeGravityAccStdZ, na.rm = TRUE),
                          AvgTimeBodyAccJerkMeanX = mean(TimeBodyAccJerkMeanX, na.rm = TRUE),
                          AvgTimeBodyAccJerkMeanY = mean(TimeBodyAccJerkMeanY, na.rm = TRUE),
                          AvgTimeBodyAccJerkMeanZ = mean(TimeBodyAccJerkMeanZ, na.rm = TRUE),
                          AvgTimeBodyAccJerkStdX = mean(TimeBodyAccJerkStdX, na.rm = TRUE),
                          AvgTimeBodyAccJerkStdY = mean(TimeBodyAccJerkStdY, na.rm = TRUE),
                          AvgTimeBodyAccJerkStdZ = mean(TimeBodyAccJerkStdZ, na.rm = TRUE),
                          AvgTimeBodyGyroMeanX = mean(TimeBodyGyroMeanX, na.rm = TRUE),
                          AvgTimeBodyGyroMeanY = mean(TimeBodyGyroMeanY, na.rm = TRUE),
                          AvgTimeBodyGyroMeanZ = mean(TimeBodyGyroMeanZ, na.rm = TRUE),
                          AvgTimeBodyGyroStdX = mean(TimeBodyGyroStdX, na.rm = TRUE),
                          AvgTimeBodyGyroStdY = mean(TimeBodyGyroStdY, na.rm = TRUE),
                          AvgTimeBodyGyroStdZ = mean(TimeBodyGyroStdZ, na.rm = TRUE),
                          AvgTimeBodyGyroJerkMeanX = mean(TimeBodyGyroJerkMeanX, na.rm = TRUE),
                          AvgTimeBodyGyroJerkMeanY = mean(TimeBodyGyroJerkMeanY, na.rm = TRUE),
                          AvgTimeBodyGyroJerkMeanZ = mean(TimeBodyGyroJerkMeanZ, na.rm = TRUE),
                          AvgTimeBodyGyroJerkStdX = mean(TimeBodyGyroJerkStdX, na.rm = TRUE),
                          AvgTimeBodyGyroJerkStdY = mean(TimeBodyGyroJerkStdY, na.rm = TRUE),
                          AvgTimeBodyGyroJerkStdZ = mean(TimeBodyGyroJerkStdZ, na.rm = TRUE),
                          AvgTimeBodyAccMagMean = mean(TimeBodyAccMagMean, na.rm = TRUE),
                          AvgTimeBodyAccMagStd = mean(TimeBodyAccMagStd, na.rm = TRUE),
                          AvgTimeGravityAccMagMean = mean(TimeGravityAccMagMean, na.rm = TRUE),
                          AvgTimeGravityAccMagStd = mean(TimeGravityAccMagStd, na.rm = TRUE),
                          AvgTimeBodyAccJerkMagMean = mean(TimeBodyAccJerkMagMean, na.rm = TRUE),
                          AvgTimeBodyAccJerkMagStd = mean(TimeBodyAccJerkMagStd, na.rm = TRUE),
                          AvgTimeBodyGyroMagMean = mean(TimeBodyGyroMagMean, na.rm = TRUE),
                          AvgTimeBodyGyroMagStd = mean(TimeBodyGyroMagStd, na.rm = TRUE),
                          AvgTimeBodyGyroJerkMagMean = mean(TimeBodyGyroJerkMagMean, na.rm = TRUE),
                          AvgTimeBodyGyroJerkMagStd = mean(TimeBodyGyroJerkMagStd, na.rm = TRUE),
                          AvgFreqBodyAccMeanX = mean(FreqBodyAccMeanX, na.rm = TRUE),
                          AvgFreqBodyAccMeanY = mean(FreqBodyAccMeanY, na.rm = TRUE),
                          AvgFreqBodyAccMeanZ = mean(FreqBodyAccMeanZ, na.rm = TRUE),
                          AvgFreqBodyAccStdX = mean(FreqBodyAccStdX, na.rm = TRUE),
                          AvgFreqBodyAccStdY = mean(FreqBodyAccStdY, na.rm = TRUE),
                          AvgFreqBodyAccStdZ = mean(FreqBodyAccStdZ, na.rm = TRUE),
                          AvgFreqBodyAccJerkMeanX = mean(FreqBodyAccJerkMeanX, na.rm = TRUE),
                          AvgFreqBodyAccJerkMeanY = mean(FreqBodyAccJerkMeanY, na.rm = TRUE),
                          AvgFreqBodyAccJerkMeanZ = mean(FreqBodyAccJerkMeanZ, na.rm = TRUE),
                          AvgFreqBodyAccJerkStdX = mean(FreqBodyAccJerkStdX, na.rm = TRUE),
                          AvgFreqBodyAccJerkStdY = mean(FreqBodyAccJerkStdY, na.rm = TRUE),
                          AvgFreqBodyAccJerkStdZ = mean(FreqBodyAccJerkStdZ, na.rm = TRUE),
                          AvgFreqBodyGyroMeanX = mean(FreqBodyGyroMeanX, na.rm = TRUE),
                          AvgFreqBodyGyroMeanY = mean(FreqBodyGyroMeanY, na.rm = TRUE),
                          AvgFreqBodyGyroMeanZ = mean(FreqBodyGyroMeanZ, na.rm = TRUE),
                          AvgFreqBodyGyroStdX = mean(FreqBodyGyroStdX, na.rm = TRUE),
                          AvgFreqBodyGyroStdY = mean(FreqBodyGyroStdY, na.rm = TRUE),
                          AvgFreqBodyGyroStdZ = mean(FreqBodyGyroStdZ, na.rm = TRUE),
                          AvgFreqBodyAccMagMean = mean(FreqBodyAccMagMean, na.rm = TRUE),
                          AvgFreqBodyAccMagStd = mean(FreqBodyAccMagStd, na.rm = TRUE),
                          AvgFreqBodyBodyAccJerkMagMean = mean(FreqBodyBodyAccJerkMagMean, na.rm = TRUE),
                          AvgFreqBodyBodyAccJerkMagStd = mean(FreqBodyBodyAccJerkMagStd, na.rm = TRUE),
                          AvgFreqBodyBodyGyroMagMean = mean(FreqBodyBodyGyroMagMean, na.rm = TRUE),
                          AvgFreqBodyBodyGyroMagStd = mean(FreqBodyBodyGyroMagStd, na.rm = TRUE),
                          AvgFreqBodyBodyGyroJerkMagMean = mean(FreqBodyBodyGyroJerkMagMean, na.rm = TRUE),
                          AvgFreqBodyBodyGyroJerkMagStd = mean(FreqBodyBodyGyroJerkMagStd, na.rm = TRUE))

# Write average data to output file
write.table(average_tidy, file = "UCI_HAR_Dataset_Average_Tidy.txt", row.names = FALSE)



