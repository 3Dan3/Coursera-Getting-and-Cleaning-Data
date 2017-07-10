# Getting and Cleaning Data
Daniel H  
September 15, 2016  

## Load data and required packages


```r
# load  required packages
library(tidyverse)
library(stringr)

# set directory
setwd("C:/Users/Daniel/Documents/R/Code/Coursera/Getting and Cleaning Data/Project/UCI HAR Dataset")

# read data into R
y_train <- 
  read_table("train/y_train.txt", col_names = F)
y_test <- 
  read_table("test/y_test.txt", col_names = F)
features <- 
  read_table("features.txt", col_names = F) 
activity_labels <- 
  read_table("activity_labels.txt", col_names = F)
subject_train <-
  read_table("train/subject_train.txt", col_names = F)
subject_test <-
  read_table("test/subject_test.txt", col_names = F)
X_train <-
  read_table("train/X_train.txt", col_names = F)
X_test <-
  read_table("test/X_test.txt", col_names = F)
```

## Analysis


```r
#-----------------------------------  train data -----------------------------------

# Assign names to the columns of the df 'activity_labels'
colnames(activity_labels) <- c("X1","Activity")

# rename column X1 to 'subject'
subject <- 
  subject_train  %>%
  rename(subject= X1) 

# bind the subject df with activity levels (X1 in train df) in the 'y_train df'.
train  <- 
  bind_cols(y_train, subject) 

# add a column with corresponding activity_labels to the 'train' df
train1 <-
  left_join(train, activity_labels) 

# assign names from the features df(column X1) to the 'X_train' df 
colnames(X_train) <-
  features %>%
  pull(1)

# combine y_train, activity labels, X_train
train2 <- 
  bind_cols(train1, X_train) 

# remove the first column X1 from train2 df to avoid error 'duplicate column name'
train3 <-
  train2 %>%
  select(-1) 

# select columns that contains means and std
train4 <- 
  train3  %>%
  select(matches('subject|Activity|std|mean|Mean')) 




#----------------------------------  test data ------------------------------------

# assign names to the columns of the df 'activity_labels'
colnames(activity_labels) <-
  c("X1","Activity")

# rename column X1 to 'subject'
subjecta <-
  subject_test  %>%
  rename(subject= X1)

# bind the subjecta df with activity levels (X1 in train df) in the y_test df.
test <-
  bind_cols(y_test, subjecta)

# add a column with corresponding activity_labels to the test df 
test1 <- 
  left_join(test, activity_labels)

# assign names from the features df(column X1) to the 'X_test' df which contains
colnames(X_test) <-
  features %>% 
  pull(1)

# combine y_test, activity labels, X_test
test2 <- 
  bind_cols(test1,X_test)

# remove the first column from test2 to avoid error "duplicate column name" msg.
test3 <- 
  test2 %>%
  select(-1)

# select columns that contains means and std
test4 <- 
  test3  %>%
  select(matches('subject|Activity|std|mean|Mean'))


#----------------  merge train and test data + summarize--------------------------

# combine Train data with Test data in a single df 'run_analysis1'.
run_analysis1 <- 
  rbind(train4,test4)

# summarize data
run_analysis<- 
  run_analysis1 %>%
  group_by(subject,Activity) %>%
  summarise_all(funs( mean))

#---------------- clean up and write required table ----------------------


# replace features names with more descriptive variable names
names(run_analysis) <- names(run_analysis) %>%  str_replace_all("^t", "time")
names(run_analysis) <- names(run_analysis) %>%  str_replace_all("^f", "frequency")
names(run_analysis) <- names(run_analysis) %>%  str_replace_all("Acc", "Accelerometer")
names(run_analysis) <- names(run_analysis) %>%  str_replace_all("Gyro", "Gyroscope")
names(run_analysis) <- names(run_analysis) %>%  str_replace_all("Mag", "Magnitude")
names(run_analysis) <- names(run_analysis) %>%  str_replace_all("BodyBody", "Body")

names(run_analysis)
```

```
##  [1] "subject"                                           
##  [2] "Activity"                                          
##  [3] "1 tBodyAccelerometer-mean()-X"                     
##  [4] "2 tBodyAccelerometer-mean()-Y"                     
##  [5] "3 tBodyAccelerometer-mean()-Z"                     
##  [6] "4 tBodyAccelerometer-std()-X"                      
##  [7] "5 tBodyAccelerometer-std()-Y"                      
##  [8] "6 tBodyAccelerometer-std()-Z"                      
##  [9] "41 tGravityAccelerometer-mean()-X"                 
## [10] "42 tGravityAccelerometer-mean()-Y"                 
## [11] "43 tGravityAccelerometer-mean()-Z"                 
## [12] "44 tGravityAccelerometer-std()-X"                  
## [13] "45 tGravityAccelerometer-std()-Y"                  
## [14] "46 tGravityAccelerometer-std()-Z"                  
## [15] "81 tBodyAccelerometerJerk-mean()-X"                
## [16] "82 tBodyAccelerometerJerk-mean()-Y"                
## [17] "83 tBodyAccelerometerJerk-mean()-Z"                
## [18] "84 tBodyAccelerometerJerk-std()-X"                 
## [19] "85 tBodyAccelerometerJerk-std()-Y"                 
## [20] "86 tBodyAccelerometerJerk-std()-Z"                 
## [21] "121 tBodyGyroscope-mean()-X"                       
## [22] "122 tBodyGyroscope-mean()-Y"                       
## [23] "123 tBodyGyroscope-mean()-Z"                       
## [24] "124 tBodyGyroscope-std()-X"                        
## [25] "125 tBodyGyroscope-std()-Y"                        
## [26] "126 tBodyGyroscope-std()-Z"                        
## [27] "161 tBodyGyroscopeJerk-mean()-X"                   
## [28] "162 tBodyGyroscopeJerk-mean()-Y"                   
## [29] "163 tBodyGyroscopeJerk-mean()-Z"                   
## [30] "164 tBodyGyroscopeJerk-std()-X"                    
## [31] "165 tBodyGyroscopeJerk-std()-Y"                    
## [32] "166 tBodyGyroscopeJerk-std()-Z"                    
## [33] "201 tBodyAccelerometerMagnitude-mean()"            
## [34] "202 tBodyAccelerometerMagnitude-std()"             
## [35] "214 tGravityAccelerometerMagnitude-mean()"         
## [36] "215 tGravityAccelerometerMagnitude-std()"          
## [37] "227 tBodyAccelerometerJerkMagnitude-mean()"        
## [38] "228 tBodyAccelerometerJerkMagnitude-std()"         
## [39] "240 tBodyGyroscopeMagnitude-mean()"                
## [40] "241 tBodyGyroscopeMagnitude-std()"                 
## [41] "253 tBodyGyroscopeJerkMagnitude-mean()"            
## [42] "254 tBodyGyroscopeJerkMagnitude-std()"             
## [43] "266 fBodyAccelerometer-mean()-X"                   
## [44] "267 fBodyAccelerometer-mean()-Y"                   
## [45] "268 fBodyAccelerometer-mean()-Z"                   
## [46] "269 fBodyAccelerometer-std()-X"                    
## [47] "270 fBodyAccelerometer-std()-Y"                    
## [48] "271 fBodyAccelerometer-std()-Z"                    
## [49] "294 fBodyAccelerometer-meanFreq()-X"               
## [50] "295 fBodyAccelerometer-meanFreq()-Y"               
## [51] "296 fBodyAccelerometer-meanFreq()-Z"               
## [52] "345 fBodyAccelerometerJerk-mean()-X"               
## [53] "346 fBodyAccelerometerJerk-mean()-Y"               
## [54] "347 fBodyAccelerometerJerk-mean()-Z"               
## [55] "348 fBodyAccelerometerJerk-std()-X"                
## [56] "349 fBodyAccelerometerJerk-std()-Y"                
## [57] "350 fBodyAccelerometerJerk-std()-Z"                
## [58] "373 fBodyAccelerometerJerk-meanFreq()-X"           
## [59] "374 fBodyAccelerometerJerk-meanFreq()-Y"           
## [60] "375 fBodyAccelerometerJerk-meanFreq()-Z"           
## [61] "424 fBodyGyroscope-mean()-X"                       
## [62] "425 fBodyGyroscope-mean()-Y"                       
## [63] "426 fBodyGyroscope-mean()-Z"                       
## [64] "427 fBodyGyroscope-std()-X"                        
## [65] "428 fBodyGyroscope-std()-Y"                        
## [66] "429 fBodyGyroscope-std()-Z"                        
## [67] "452 fBodyGyroscope-meanFreq()-X"                   
## [68] "453 fBodyGyroscope-meanFreq()-Y"                   
## [69] "454 fBodyGyroscope-meanFreq()-Z"                   
## [70] "503 fBodyAccelerometerMagnitude-mean()"            
## [71] "504 fBodyAccelerometerMagnitude-std()"             
## [72] "513 fBodyAccelerometerMagnitude-meanFreq()"        
## [73] "516 fBodyAccelerometerJerkMagnitude-mean()"        
## [74] "517 fBodyAccelerometerJerkMagnitude-std()"         
## [75] "526 fBodyAccelerometerJerkMagnitude-meanFreq()"    
## [76] "529 fBodyGyroscopeMagnitude-mean()"                
## [77] "530 fBodyGyroscopeMagnitude-std()"                 
## [78] "539 fBodyGyroscopeMagnitude-meanFreq()"            
## [79] "542 fBodyGyroscopeJerkMagnitude-mean()"            
## [80] "543 fBodyGyroscopeJerkMagnitude-std()"             
## [81] "552 fBodyGyroscopeJerkMagnitude-meanFreq()"        
## [82] "555 angle(tBodyAccelerometerMean,gravity)"         
## [83] "556 angle(tBodyAccelerometerJerkMean),gravityMean)"
## [84] "557 angle(tBodyGyroscopeMean,gravityMean)"         
## [85] "558 angle(tBodyGyroscopeJerkMean,gravityMean)"     
## [86] "559 angle(X,gravityMean)"                          
## [87] "560 angle(Y,gravityMean)"                          
## [88] "561 angle(Z,gravityMean)"
```

```r
# write table
write.table(run_analysis,"./tidy_dataset.txt",
            sep=" ", row.name=FALSE)
```














































































