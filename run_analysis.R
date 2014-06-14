
#setwd("C:\\Users\\Krzysztof\\Desktop\\data")

library(plyr)

getActivityLabels <- function()
{
  read.table("./activity_labels.txt")
}

loadRawData <- function()
{  
    training <- cbind(
        read.table('./train/subject_train.txt'), 
        read.table('./train/y_train.txt'), 
        read.table('./train/X_train.txt')
    )
    
    test <- cbind(
        read.table('./test/subject_test.txt'), 
        read.table('./test/y_test.txt'), 
        read.table('./test/X_test.txt')
    )
  
    rbind(training, test)  
}





loadFeatureDictionary <- function()
{  
    f_names <- read.table('./features.txt')
    as.character(f_names[, 2])
}


# 1. Training and Set data merging
merged_set <- loadRawData()


feature_names <- loadFeatureDictionary()

colnames(merged_set) <- c('subject', 'activity', feature_names)


# 2. Extracts mean and standard deviation
focus_features <- grepl('(.*)-(mean|std)\\(\\)(.*)', feature_names)
merged_set <- merged_set[, c(TRUE, TRUE, focus_features)]

# 3. Use descriptive activity names to name the activities in the data set

dict<-getActivityLabels();

merged_set$activity <- mapvalues(merged_set$activity , dict$V1,as.character(dict$V2))


# 4. Appropriately label the data set with descriptive activity names
colnames(merged_set) <- gsub('[-()]', '', colnames(merged_set))


# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject

tidy_set <- ddply(merged_set, c('activity', 'subject'), numcolwise(mean))
write.table(tidy_set, "final.txt", quote = FALSE,row.names = FALSE, sep='\t')




