

# -- 1. Retrieve raw data from source, unzips the file in the working directory ----

if (!file.exists("HARdata")) {
    dir.create("HARdata")
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    
    download.file(fileUrl, "HARdata.zip")
    unzip("HARdata.zip")
    download.time <<- date()
    print(c("Your file was downloaded:", date()))
}


# -- 2. Import test set, subject ID#s, activities ----

setwd("./UCI HAR Dataset")  # make the downloaded file the working directory


test.set <- read.table("./test/X_test.txt")

test.labels <- read.table("./test/y_test.txt")
test.labels <- as.vector(test.labels[[1]])
test.labels <- factor(test.labels, levels = 1:6, 
                      labels = c("walking", "walking_upstairs", "walking_downstairs",
                    "sitting", "standing", "laying"))

subject.test <- read.table("./test/subject_test.txt", col.names = "ID")


# -- 3. Import training set, subject ID#s, activities ----

training.set <- read.table("./train/X_train.txt")

training.labels <- read.table("./train/y_train.txt")
training.labels <- as.vector(training.labels[[1]])
training.labels <- factor(training.labels, levels = 1:6, 
                      labels = c("walking", "walking_upstairs", "walking_downstairs",
                                 "sitting", "standing", "laying"))

subject.train <- read.table("./train/subject_train.txt", col.names = "ID")


# -- 4. Retrieve variable measure names ----

var.names <- read.table("features.txt")
var.names <- as.vector(var.names[,2])

# Project specification asks for a specific subset of variables to be chosen,
# specifically only measures of mean and standard deviation.
# 79 existing variables from the features.txt match this qualification.


# -- 5. Subset training/test sets based on variables measuring mean/std ----

names(training.set) <- var.names

training.set.mean <- training.set[, which(grepl("mean", names(training.set)))]
training.set.std <- training.set[, which(grepl("std", names(training.set)))]

training.set <- cbind(training.set.mean, training.set.std)
rm(training.set.mean, training.set.std)
training.set <- cbind(subject.train, training.labels, training.set)
colnames(training.set)[2] <- "activities"
 
names(test.set) <- var.names

test.set.mean <- test.set[, which(grepl("mean", names(test.set)))]
test.set.std <- test.set[, which(grepl("std", names(test.set)))]

test.set <- cbind(test.set.mean, test.set.std)
rm(test.set.mean, test.set.std)
test.set <- cbind(subject.test, test.labels, test.set)
colnames(test.set)[2] <- "activities"


# -- 6. Merge both training/test set ----

system.time(both <- rbind(training.set, test.set))


# -- 7. Remove superfluous characters from var names ----

names(both) <- gsub("\\()", "", names(both))
names(both) <- gsub("-", "", names(both))
names(both) <- sub("bodybody", "body", names(both))
names(both) <- tolower(names(both))


# -- 8. Reshape dataset, retrieve variable averages for id# and activity-type ----

if (library(reshape2, logical.return=TRUE) = FALSE) {
    install.packages("reshape2")
}

library(reshape2)

bothmelt <- melt(both, id.vars = c("id", "activities"))

activitymeans <- dcast(bothmelt, activities ~ variable, mean)

subjectmeans <- dcast(bothmelt, id ~ variable, mean)

avgcombined <- cbind(subjectmeans, activitymeans) 

tidy <- melt(avgcombined, id.vars = c("id", "activities"))