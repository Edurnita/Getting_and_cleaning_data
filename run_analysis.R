#R code

#############################################################
#-------GETTING AND CLEANING DATA - FINAL PROJECT-----------#
#############################################################

## STEP 0: LOADING PACKAGES AND ALL DATA SETS
# Setting the working directory
setwd ("C:/Users/ealonso/Desktop/Coursera R/Getting and cleaning data") 
# Loading neccesary libraries
library(plyr)
# Loading all data sets
x.train <- read.table("X_train.txt")
subject.train <- read.table("subject_train.txt")
y.train <- read.table("y_train.txt")
x.test <- read.table("X_test.txt")
subject.test <- read.table("subject_test.txt")
y.test <- read.table("y_test.txt")
features <- read.table("features.txt", colClasses = c("character"))
act_labels <- read.table("activity_labels.txt", col.names = c("Actv_Id", "Activity"))

## STEP 1: MERGE THE TRAINING AND TEST SETS
# First, I merge the training and test sets
base <- rbind(cbind(cbind(x.train, subject.train), y.train), cbind(cbind(x.test, subject.test), y.test))
# Second, we define de labels of dataset
base_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "Actv_Id"))[,2]
names(base) <- base_labels
# Third, I remove the data sets that I not use more times.
rm(x.train, y.train, subject.train, x.test, y.test, subject.test)

## STEP 2: EXTRACTING MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION
base.mean.sd <- base[,grepl("mean|std|Subject|Actv_Id", names(base))]

## STEP 3: USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET
# First, I match the activity number with its respective activity names
base.mean.sd <- join(base.mean.sd, act_labels, by = "Actv_Id", match = "first")
# Second, I remove the first column, which is the activity numbers, leaving only its respective activity names
base.mean.sd <- base.mean.sd[,-1]

## STEP 4: LABEL DATA SET WITH DESCRIPTIVE VARIABLES NAMES
# I make valid the variables names for R
names(base.mean.sd) <- make.names(names(base.mean.sd))
# I change the names of variables 
names(base.mean.sd) <- gsub('\\...X',".X",names(base.mean.sd))
names(base.mean.sd) <- gsub('\\...Y',".Y",names(base.mean.sd))
names(base.mean.sd) <- gsub('\\...Z',".Z",names(base.mean.sd))
names(base.mean.sd) <- gsub('^t',"TimeDomain.",names(base.mean.sd))
names(base.mean.sd) <- gsub('^f',"FreqDomain.",names(base.mean.sd))
names(base.mean.sd) <- gsub('Acc',"Acceleration",names(base.mean.sd))
names(base.mean.sd) <- gsub('GyroJerk',"AngularAcceleration",names(base.mean.sd))
names(base.mean.sd) <- gsub('Gyro',"AngularSpeed",names(base.mean.sd))
names(base.mean.sd) <- gsub('Mag',"Magnitude",names(base.mean.sd))

## STEP 5: FROM STEP 4, I CREATE AN INDEPENDENT TIDY DATA SET WITH THE AVERAGE 
## OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT.

tidy_dataset <- ddply(base.mean.sd, c("Subject","Activity"), numcolwise(mean))
write.table(tidy_dataset, file = "tidy_dataset.txt", row.name=FALSE)

