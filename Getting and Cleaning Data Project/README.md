# Getting and Cleaning Data Project
## Human Activity Recognition Using Smartphones Dataset

### Project Objective
The objective of this project is to demonstrate ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 

### Set-up Pre-requisites
Before running the "run_analysis.R" script, please ensure that the UCI Human Activity Recognition (HAR) dataset has been downloaded from this link - https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.
After download, extract the data into the R working directory. Please ensure that data is extracted **directly** into the R working directory and **not** within any sub-directory. Following should be the folder structure.

R Working Directory/  
R Working Directory/activity_labels.txt  
R Working Directory/features.txt  
R Working Directory/features_info.txt  
R Working Directory/README.txt  
R Working Directory/test/    
R Working Directory/test/subject_test.txt    
R Working Directory/test/X_test.txt  
R Working Directory/test/y_test.txt   
R Working Directory/train/  
R Working Directory/train/subject_train.txt  
R Working Directory/train/X_train.txt  
R Working Directory/train/y_train.txt   

### Execution
Once the data has been downloaded and extracted per set-up steps above, please run the "run_analysis.R" script. After script execution finishes, you should be able to see a tidy output data file called "UCI_HAR_Dataset_Average_Tidy.txt" in the R working directory.
