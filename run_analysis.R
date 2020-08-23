# Read in the relevant files from the UCI HAR Dataset directory
library(dplyr)

subject_train <- read.table("train/subject_train.txt")
train <- read.table("train/X_train.txt")
train_label <- read.table("train/y_train.txt")
subject_test <- read.table("test/subject_test.txt")
test <- read.table("test/X_test.txt")
test_label <- read.table("test/y_test.txt")
variables <- read.table("features.txt")
variables <- as.list(variables[,2])
activities <- read.table("activity_labels.txt")

# Objective 1; Merge training and test datasets

mergeddata <- bind_rows(train, test)
mergedlabel <- bind_rows(train_label, test_label)
mergedsubject <- bind_rows(subject_train, subject_test)
mergeddata <- as_tibble(mergeddata)
data <- bind_cols(mergeddata, mergedlabel)
data <- bind_cols(data, mergedsubject)
data <- as_tibble(data)

# Objective 2; Extract only mean and standard deviation variables

cols <- grep("mean|std", variables)
# index of variables with mean or std are stored in cols
extracted <- data[,c(cols, 562, 563)]
# selects the columns as indexed by cols, and the last two columns appended
# which includes the subjects, and the activity labels
# this data frame is stored in extracted

# Objective 3; Use descriptive activity names to name the activities

# Creates a function that converts the integer values to the activities
assignactivity <- function(x){
  if(x == 1) {
    x = "walking"
  } else {
      if (x == 2){
        x = "walking upstairs"
      } else {
        if(x == 3){
          x = "walking downstairs"
        } else {
          if(x == 4){
            x = "sitting"
          } else {
            if(x == 5){
              x = "standing"
            } else {
              if(x == 6){
                x = "laying"
              }
            }
          }
        }
      }
    }
  }
# This function converts an integer value to the corresponding activity string
extracted2 = sapply(extracted$V1...562, assignactivity)
# Creates a vector with all of the activities labeled
labeled <- bind_cols(extracted, extracted2)
# Adds this vector as another column to the working data frame

# Objective 4; Label variables appropriately

oldnames <- colnames(labeled)
newlabels <- as.character(variables[cols])
labeled <- rename_at(labeled, vars(oldnames), ~c(newlabels, "Activity Num", 
                                                 "ID", "Activity"))
# This code renames the columns from "V1..., V2 etc." to variable names
names(labeled) <- gsub("-", " ", names(labeled))
names(labeled) <- gsub("[()]", " ", names(labeled))
names(labeled) <- gsub("std", "Standard Deviation", names(labeled))
# This codes removes dashes and parentheses for more appropriate labels

# Objective 5; Create dataset with the average of each variable for each
# activity and each subject

Final <- labeled %>% group_by(labeled$ID, labeled$Activity) %>%
  summarise_each(funs(mean))
# Grouping by ID, this calculates the mean of the variables
Final <- select(Final, -c("Activity", "Activity Num", "ID"))
# Removes irrelevant columns
Final <- rename(Final, ID = "labeled$ID", Activity = "labeled$Activity")
# Renames columns

# Dataset named "Final" contains averages of each variable by subject ID and by
# activity


## write.table(Final, file = "Tidy Dataset.txt", row.names = F)
