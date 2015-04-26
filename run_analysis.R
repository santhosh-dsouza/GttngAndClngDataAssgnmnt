# Merge the training and the test sets to create one data set
trainingData <- read.csv("samsung.data/train/X_train.txt", sep = "", header = FALSE)
testData <- read.csv("samsung.data/test/X_test.txt", sep = "", header = FALSE)
unifiedData <- rbind(trainingData, testData)

# Extract only the measurements on the mean and standard deviation for each measurement
features <- read.csv("samsung.data/features.txt", sep = "", header = FALSE)
relevantFeatures <- features[grep("mean\\(\\)|std\\(\\)", features[, 2]), ]
relevantData <- unifiedData[, relevantFeatures[, 1]]

# Use descriptive activity names to name the activities in the data set
activityLabels <- read.csv("samsung.data/activity_labels.txt", sep = "", header = FALSE)
activitiesTraining <- read.csv("samsung.data/train/y_train.txt", sep = "", header = FALSE)
activitiesTest <- read.csv("samsung.data/test/y_test.txt", sep = "", header = FALSE)

allActivities <- rbind(activitiesTraining, activitiesTest)
allActivities$id <- 1:nrow(allActivities)
mergedActivities <- merge(allActivities, activityLabels, by.x = "V1", by.y = "V1", all = FALSE)
labelledActivities <- mergedActivities[order(mergedActivities$id), ]

# Appropriately label the data set with descriptive variable names
subjectsTraining <- read.csv("samsung.data/train/subject_train.txt", sep = "", header = FALSE)
subjectsTest <- read.csv("samsung.data/test/subject_test.txt", sep = "", header = FALSE)
allSubjects <- rbind(subjectsTraining, subjectsTest)

descriptiveLabels <- gsub("  ", " ",
                      gsub("-", " ",
                           gsub("mean\\(\\)", "Mean",
                                gsub("std\\(\\)", "Std",
                                     gsub("^f", "Frequency ",
                                          gsub("^t", "Time ",
                                               gsub("([A-Z])", " \\1", relevantFeatures[, 2], perl = TRUE)))))))

names(relevantData) <- descriptiveLabels
names(labelledActivities) <- c("Activity Code", "Id", "Activity")
names(allSubjects) <- "Subject"
labelledDataset <- cbind(allSubjects, labelledActivities, relevantData)

# Create a second, independent tidy data set from this generated dataset with the average of each variable 
# for each activity and each subject
library(dplyr)

labelledDataframeTable <- tbl_df(labelledDataset)
tidyDataset <- labelledDataframeTable %>%
  select(-`Activity Code`, -Id) %>%
  group_by(Subject, Activity) %>%
  summarise_each(funs(mean))

# Output the data set as a txt file created with write.table() using row.name=FALSE
write.table(tidyDataset, file = "assignment_output.txt", row.names = FALSE)
