library(plyr)

##Read training and test, feature and activity labels files
x_tr <- read.table("UCI HAR Dataset/train/X_train.txt")
y_tr <- read.table("UCI HAR Dataset/train/y_train.txt")
s_tr <- read.table("UCI HAR Dataset/train/subject_train.txt")
x_ts <- read.table("UCI HAR Dataset/test/X_test.txt")
y_ts <- read.table("UCI HAR Dataset/test/y_test.txt")
s_ts <- read.table("UCI HAR Dataset/test/subject_test.txt")
features <- read.table("UCI HAR Dataset/features.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

##Merge data sets 
merge_x <- rbind(x_tr, x_ts)
merge_y <- rbind(y_tr, y_ts)
merge_s <- rbind(s_tr, s_ts)

##Create a list with merged data sets
merge_list = list(x=merge_x,y=merge_y,subject=merge_s)

##Look for the columns that contain mean and std
mean_cols <- sapply(features[,2], function(x) grepl("mean()",x,fixed=T))
std_cols <- sapply(features[,2], function(x) grepl("std()",x,fixed=T))

list_x <- merge_list$x
list_y <- merge_list$y

##Retrieve std and mean columns from data
selected_x_cols<-list_x[,(mean_cols | std_cols)]
colnames(selected_x_cols)<-features[(mean_cols | std_cols),2]

##Set activity names 
colnames(list_y)<-"activity"
list_y$activity[list_y$activity==1] = as.character(activity_labels[activity_labels$V1==1,"V2"])
list_y$activity[list_y$activity==2] = as.character(activity_labels[activity_labels$V1==2,"V2"])
list_y$activity[list_y$activity==3] = as.character(activity_labels[activity_labels$V1==3,"V2"])
list_y$activity[list_y$activity==4] = as.character(activity_labels[activity_labels$V1==4,"V2"])
list_y$activity[list_y$activity==5] = as.character(activity_labels[activity_labels$V1==5,"V2"])
list_y$activity[list_y$activity==6] = as.character(activity_labels[activity_labels$V1==6,"V2"])

colnames(merge_list$subject) <- c("subject")

##Combine data processed above
bind_data <- cbind(selected_x_cols, list_y, merge_list$subject)

##Calculate average of each variable for each activity and each subject
tidy_data <- ddply(bind_data, .(subject, activity), function(x) colMeans(x[,1:60]))

##Write tidy data in a file
write.csv(tidy_data, "tidy_dataset_UCI_HAR.csv", row.names=FALSE)
