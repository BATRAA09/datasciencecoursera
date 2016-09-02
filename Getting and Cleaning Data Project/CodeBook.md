# Getting and Cleaning Data Project
## Human Activity Recognition Using Smartphones Dataset

### Input Data Description
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

*tBodyAcc-XYZ  
tGravityAcc-XYZ  
tBodyAccJerk-XYZ  
tBodyGyro-XYZ  
tBodyGyroJerk-XYZ  
tBodyAccMag  
tGravityAccMag  
tBodyAccJerkMag  
tBodyGyroMag  
tBodyGyroJerkMag  
fBodyAcc-XYZ  
fBodyAccJerk-XYZ  
fBodyGyro-XYZ  
fBodyAccMag  
fBodyAccJerkMag  
fBodyGyroMag  
fBodyGyroJerkMag*  

The set of variables that were estimated from these signals are: 

*mean()*: Mean value  
*std()*: Standard deviation  
*mad()*: Median absolute deviation  
*max()*: Largest value in array  
*min()*: Smallest value in array  
*sma()*: Signal magnitude area  
*energy()*: Energy measure. Sum of the squares divided by the number of values.   
*iqr()*: Interquartile range   
*entropy()*: Signal entropy  
*arCoeff()*: Autorregresion coefficients with Burg order equal to 4  
*correlation()*: correlation coefficient between two signals  
*maxInds()*: index of the frequency component with largest magnitude  
*meanFreq()*: Weighted average of the frequency components to obtain a mean frequency  
*skewness()*: skewness of the frequency domain signal  
*kurtosis()*: kurtosis of the frequency domain signal  
*bandsEnergy()*: Energy of a frequency interval within the 64 bins of the FFT of each window. 
*angle()*: Angle between to vectors.  

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

*gravityMean  
tBodyAccMean  
tBodyAccJerkMean  
tBodyGyroMean  
tBodyGyroJerkMean*  

### Data Processing / Transformation Steps
1. Load activity and feature labels data in activity_labels and feature_labels variables.
2. Load training activity, subject and measurement data in respective training data variables.
3. Assign feature names to the training data variables.
4. Remove all measurement columns except mean and standard deviation.
5. Rename feature names to a more user-readable format. For example, tBodyAcc-mean()-X becomes TimeBodyAccMeanX.
6. Merge activity names abd subject IDs with the measurement data.
7. Repeat steps 2 to 6 for test data.
8. Merge tidy outputs from steps 6 and 7 to create full tidy dataset.
9. Apply group_by and summarize functions from the dlyr package to calculate average measurement values for each activity and subject.
10. Export the summarized dataset to a space delimited file called "UCI_HAR_Dataset_Average_Tidy.txt"

### Output Data Description
The output from running the "run_analysis.R" script would be a file called "UCI_HAR_Dataset_Average_Tidy.txt". This file has 68 columns and 104 rows including the header. Each row of this file is a vector of averages for each mean and standard deviation measurement calculated at the activity and subject level. 
Following is a description of the columns.

'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

*ActivityName*: Type of activity
*SubjectID*: Subject identifier
*AvgTimeBodyAccXYZ  
AvgTimeGravityAccXYZ  
AvgTimeBodyAccJerkXYZ  
AvgTimeBodyGyroXYZ  
AvgTimeBodyGyroJerkXYZ  
AvgTimeBodyAccMag  
AvgTimeGravityAccMag  
AvgTimeBodyAccJerkMag  
AvgTimeBodyGyroMag  
AvgTimeBodyGyroJerkMag  
AvgFreqBodyAccXYZ  
AvgFreqBodyAccJerkXYZ  
AvgFreqBodyGyroXYZ  
AvgFreqBodyAccMag  
AvgFreqBodyBodyAccJerkMag  
AvgFreqBodyBodyGyroMag  
AvgFreqBodyBodyGyroJerkMag*  

Mean and Standard Deviation for each of the above variables is calculated as a separate column and averaged to the activity and subject level.
