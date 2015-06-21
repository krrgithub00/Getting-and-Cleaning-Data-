#-------------------------------------------------------------------------------------------------
# Module: Extracting and Cleaning Data - Project 
# Program Name :run_analysis.R
# Date         : 06-21-2015 (Happy Fathers day!!)
# Project Scope and requirements:
# #       
#     A:Merge the training and the test sets to create one data set. 
#         Step 13:Result of merged data set for Test and Train data.
#     B:Extract only the measurements on the mean and standard deviation for each measurement. 
#         Step 5:Mean & Std Dev mesaures extraction for Test Data. 
#         Step 9:Mean & Std Dev mesaures extraction for Train Data. 
#     C:Uses descriptive activity names to name the activities in the data set
#          Step 6:Test data descriptive activity names
#          Step 11 :Train data descriptive activity names
#       
#     D:Appropriately labels the data set with descriptive variable names.
#          Step 14: Implemented via the idenity_labels variable.
#     E:From the data set in step 4,creates a second, independent tidy data set 
#       with the average of each variable for each activity and each subject.
#          Step 15: A tidy data set (tidy_data) with  mean /average is created.
# Data Sources:
#       https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# Data Description:
#       http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
#-------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------
#Step 0: Housekeeping  - cleanup your working directory, make sure data.table,reshape2 packages are installed
#-------------------------------------------------------------------------------------------------


rm(list=ls())

if(!require("data.table")) {   
    install.packages("data.table")
  }

if(!require("reshape2")) {   
  install.packages("reshape2")
}

#-------------------------------------------------------------------------------------------------
# Step 1: Define the packages and libraries that are required upfront.
#-------------------------------------------------------------------------------------------------


library(httr)       # handles & support the pull of the file below.httr makes http easy
require(data.table) # fast subset, fast grouping, fast update, fast ordered joins and list columns
require(reshape2)   # Good use of forcing a mandatory utility requirement


#-------------------------------------------------------------------------------------------------
# Step 2: Store the Activity labels("WALKING", "WALKING_UPSTAIRS" etc) 
#         in a variable.This is for the data to be importted in step3 and step8 
#-------------------------------------------------------------------------------------------------

actvty_lbls <- read.table("activity_labels.txt",header=FALSE)[,2]  # imports activity labels data
col_names <- read.table("features.txt",header=FALSE)[,2]           # imports features data


#-----------------------------------------------------------------------------------------------
#Step 3 :Import & Load the X, y and subject test data sets into tables.
#-------------------------------------------------------------------------------------------------

  # Test   datasets  

    tst_X_dataset <- read.table("X_test.txt",header=FALSE) # imports test X_test data
    tst_Y_dataset <- read.table("y_test.txt",header=FALSE) # imports y_test data
    tst_subject_dataset <- read.table ("subject_test.txt",header=FALSE) # imports subject data
    names(tst_X_dataset) = col_names
         
  

#--------------------------------------------------------------------------------------------------
# Step 4: extract the text  "mean" &  "std" (for standard deviation) of the measurements using grepl 
#  to create a logical vector. TRUE value for id, mean and stddev cols and FALSE for other columns
#      
#--------------------------------------------------------------------------------------------------  
    
    
    extr_features = (grepl("activity..",col_names) | grepl("subject..",col_names) | grepl("-mean..",col_names) & !grepl("-meanFreq..",col_names) & !grepl("mean..-",col_names) | grepl("-std..",col_names) & !grepl("-std()..-",col_names));
    
    
    
#--------------------------------------------------------------------------------------------------
# Step 5: extract the measurements on the mean and standard deviation for each measurement
#--------------------------------------------------------------------------------------------------   
    tst_X_dataset = tst_X_dataset[,extr_features]
#--------------------------------------------------------------------------------------------------
# Step 6 Load the Activity labels, assign the labels and subject to the dataset.
#--------------------------------------------------------------------------------------------------
    tst_Y_dataset[,2] = actvty_lbls[tst_Y_dataset[,1]]
    names(tst_Y_dataset) = c("Activity_ID", "Activity_Label")
    names(tst_subject_dataset) = "subject"
#--------------------------------------------------------------------------------------------------
# Step 7: Bind the test dataset
#--------------------------------------------------------------------------------------------------
    test_data <- cbind(as.data.table(tst_subject_dataset), tst_Y_dataset, tst_X_dataset)
    
    
#--------------------------------------------------------------------------------------------------
# Step 8: Process the Train data sets for X,y and subject.
#--------------------------------------------------------------------------------------------------
    # Training datasets  
    
    trg_X_dataset <- read.table("X_train.txt",header=FALSE)
    trg_Y_dataset <- read.table("y_train.txt",header=FALSE)
    trg_subject_dataset <- read.table ("subject_train.txt",header=FALSE)
    names(trg_X_dataset) = col_names
    

#--------------------------------------------------------------------------------------------------
# Step 9: extract the measurements on the mean and standard deviation for each measurement
#--------------------------------------------------------------------------------------------------   
    trg_X_dataset = trg_X_dataset[,extr_features]
    
#--------------------------------------------------------------------------------------------------
# Step 11 Load the Activity labels, assign the labels and subject to the dataset.
#--------------------------------------------------------------------------------------------------
    trg_Y_dataset[,2] = actvty_lbls[trg_Y_dataset[,1]]
    names(trg_Y_dataset) = c("Activity_ID", "Activity_Label")
    names(trg_subject_dataset) = "subject"
    
#--------------------------------------------------------------------------------------------------
# Step 12: Bind the train dataset
#--------------------------------------------------------------------------------------------------
    train_data <- cbind(as.data.table(trg_subject_dataset), trg_Y_dataset, trg_X_dataset)
    
    
    
#--------------------------------------------------------------------------------------------------
# Step 13 : Now merge the test and train data post the two cbind's and clean up the variable names
#--------------------------------------------------------------------------------------------------
    
    mrgd_data = rbind(test_data, train_data)
    mrgd_data = mrgd_data[extr_features==TRUE]
     
    # Now clean up the vraibales names in the merged data set
    
    for (i in 1:length(col_names)) 
    {
      col_names[i] = gsub("\\()","",col_names[i])
      col_names[i] = gsub("-std$","StdDev",col_names[i])
      col_names[i] = gsub("-mean","Mean",col_names[i])
      col_names[i] = gsub("^(t)","time",col_names[i])
      col_names[i] = gsub("^(f)","freq",col_names[i])
      col_names[i] = gsub("([Gg]ravity)","Gravity",col_names[i])
      col_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",col_names[i])
      col_names[i] = gsub("[Gg]yro","Gyro",col_names[i])
      col_names[i] = gsub("AccMag","AccMagnitude",col_names[i])
      col_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",col_names[i])
      col_names[i] = gsub("JerkMag","JerkMagnitude",col_names[i])
      col_names[i] = gsub("GyroMag","GyroMagnitude",col_names[i])
    };
    # Reassign the cleaned up col names to the merged data set.
    colnames(mrgd_data) = col_names;
    
#-----------------------------------------------------------------------------------------------
# Step 14 : Now apply the labels to the cols and data to the merged data set
#--------------------------------------------------------------------------------------------------
    
    identity_labels   = c("subject", "Activity_ID", "Activity_Label")
    
    data_labels = setdiff(colnames(mrgd_data), identity_labels)
    melt_data      = melt(mrgd_data, id = identity_labels, measure.vars = data_labels) 
    
     
#------------------------------------------------------------------------------------------------------------
# Step 15 : Now apply the mean function to the melt dataset using the dcast function to create a tidy dataset
#-------------------------------------------------------------------------------------------------------------
    
    tidy_data   = dcast(melt_data, subject + Activity_Label ~ variable, mean)
    
#------------------------------------------------------------------------------------------------------------
# Step 16 : Save/Write the tidy data set to a file
#-------------------------------------------------------------------------------------------------------------
    
    write.table(tidy_data, file = "tidy_data.txt",row.names=FALSE,sep='\t')
    

    #-----------------------end of code or run_analysis.R----------------------------
    
    
     
