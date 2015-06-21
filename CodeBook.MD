PURPOSE OF THE CODEBOOK AS STATED:

Describes the variables, the data, and any transformations or work that you performed 
to clean up the data 

Project : Getting and CLeaning Data
Author: Raghu Kundurthi
Data Sources:
    Data - > https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
    Description of data is in - > http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

About the Data used ....

The Human Activity(Walking, Sitting, Standing ,Inertia(Laying) etc) Recognition of wearables devices  (smartphone) usage has been tracked, trialed and tested on 30 
individuals ("subjects"). The data collection was enabled via the embedded accelerometer in the smartphone(Samsung Galaxy S II)
The subjects(just plain folks like you and me!) carried/wore these smartphones while carrying out various activites. The entire
session was vide recorded.The data collected has been classifed under three main sections - Test Data, Trial Data 
and Subject Data.About 70% of the data came from training and 40% from the test componenet of the trial.

Complete data description and the source of data is referenced above under "Data Sources"

About the variables used in the study and codebase(run_analysis.R) ........

In all, there are 15 critical variables that are listed below.
acytvty_labels
col_names
data_labels
extr_features /* could have named this much better. A proud lazy programmer that I am, I left it as is!*/
identity_labels
melt_data
mrgd_data
tst_X_dataset
tst_Y_dataset
tst_subject_dataset
trg_X_dataset
trg_Y_dataset
trg_subject_dataset
test_data
tidy_data

The code  utilizes the 16 variables to handle and process the following
    Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
    Triaxial Angular velocity from the gyroscope.
    A 561-feature vector with time and frequency domain variables.
    Its activity label.
    An identifier of the subject who carried out the experiment.

How the code layout plays out .....

The project is accomplished in 5 logical blocks ,pretty much laid out in the ask by the teach ....

 1. Merge the training and  test data sets to create a single data set.
    (Steps 0 - Steps 13 as documented in the README.md)
 2. Weed out the mean and std deviation for the measurements
    (Step 5 and Step 9 in the code)
 3. Now make the activity names meaningful 
    ( Step 14)
 4. Label the data sets correctly
    (step 13, the for loop)
5. Create a second independed , tidy data set
    (step 15, should see step 14 too)
    






