########## Peer-graded Assignment: Getting and Cleaning Data Course Project
library(dplyr)
library(data.table)
## Test data
test_sj <- read.table("./test/subject_test.txt")
test_x <- read.table("./test/X_test.txt")
test_y <- read.table("./test/Y_test.txt")
## Train data
train_sj <- read.table("./train/subject_train.txt")
train_x <- read.table("./train/X_train.txt")
train_y <- read.table("./train/Y_train.txt")
# features file 
features <- read.table("./features.txt")
# activity labels
act_labels <- read.table("./activity_labels.txt") 
# question 1 : merge test and training dataset
all_sj <- rbind(test_sj, train_sj)
all_x <- rbind(test_x,train_x)
all_y <- rbind(test_y,train_y)
# question 2: Extracts only the measurements and std on the mean.
col_names <- array(features[,2])
names(all_x) = col_names
sub_mean_std <- all_x[,c(grep("mean",features[[2]]),grep("std",features[[2]]))]
# question 3: given descriptive activity names 
c <- seq(from = 1, to = 10299)
all_sj <- mutate(all_sj, ind=c)
all_x <- mutate(all_x, ind=c)
all_y <- mutate(all_y, ind=c)
all_labels <- merge(all_y,act_labels, "V1")
all_labels <- arrange(all_labels,ind)
names(all_labels) <- c("Act_id", "ind", "Act_name")
sub_mean_std <- mutate(sub_mean_std, ind=c)
all_mean_std <- cbind(all_sj[,1],sub_mean_std[,1:79], all_labels[,3])
# question 4: labels the data set with descriptive variable names
col_names = names(all_mean_std[,2:80])
names(all_mean_std) =c("Subject", col_names,"Activity")
names(all_mean_std) <- gsub("\\(\\)","",names(all_mean_std))
# question 5: second dataset to get mean by subject, activities
all_mean_std <- data.table(all_mean_std)
avg_all <- all_mean_std %>% group_by(Subject, Activity) %>% summarise_all(mean)

# output to file 
write.table(all_mean_std, "cleaned.txt") 
write.table(avg_all,"second.txt",row.name=FALSE) 
