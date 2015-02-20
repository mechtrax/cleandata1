#Here are the assignement guidelines for your reference

#You should create one R script called run_analysis.R that does the following. 
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#Reading the files for the database

 subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", quote="\"")

X_test <- read.table("UCI HAR Dataset/test/X_test.txt", quote="\"")

y_test <- read.table("UCI HAR Dataset/test/y_test.txt", quote="\"")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", quote="\"")

X_train <- read.table("UCI HAR Dataset/train/X_train.txt", quote="\"")

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", quote="\"")

features <- read.table("UCI HAR Dataset/features.txt", quote="\"")
 

# Merging the two data sets.
library(plyr)
library(dplyr)

X<- rbind ( X_train, X_test)# X represents the data set
y<- rbind( y_train, y_test) # y is activity information
X1<- apply(X, 2, function(x) as.numeric(x))  ##converting values to numeric so that mean can be calculated.
subject<- rbind(subject_train, subject_test)  
data <- data.frame( subject, y, X1)# creating a dataframe which has the subject and activity coloumns
cran<- tbl_df(data)
 
 
  ##  selecting the required coloumns from the entire data set
cran2<- select(cran, V1:V6,V41:V46, V81: V86, V121: V126, V161:V166, V201: V202, V214: V215, V227:V228, V240:V241, V253:V254, V266:V271, V345: V350, V424: V429, V503:V504, V516:V517, V529: V530, V542:V543, V373:V375, V452:V454, V513,V526,V539, V552 )
 
# V1 = student , V1.1 = activity included in V1:V6, V1.2 is the first coloumn as given in the dataset
 
 
 ##renaming the activites with the description
cran2$V1.1 = as.factor(cran2$V1.1)
cran2$V1.1<- revalue(cran2$V1.1, c( "1" = "WALKING",
 "2" = "WALKING_UPSTAIRS",
 "3" ="WALKING_DOWNSTAIRS",
 "4"= "SITTING",
 "5"="STANDING",
 "6"= "LAYING"))

 
 # renaming the variables with descriptive names
 # taking the subset of the feature file which gives the names of the coloumns
 features1<- features[c( 1:6,41:46, 81: 86, 121: 126, 161:166, 201: 202, 214: 215, 227:228, 240:241, 253:254, 266:271, 345: 350, 424: 429, 503:504, 516:517, 529: 530, 542:543, 373:375, 452:454, 513,526,539, 552), 2 ]
 features1<- as.character(features1) # converting the factors to character
 features1<- gsub("[[:punct:]]", "", features1) # removing all the extra symbols in the coloumn  names
 features123<- c("student", "activity", features1)  # adding the two extra coloumn names 
 ##features123<- gsub("-" ,"",  features12)
 ##features123<- gsub("[[:punct:]]", "", features12)
 cran3<- cran2
 colnames(cran3)<- features123 # renaming the data with the appropriate variable name.
 
 #cran3  changed the names of the coloumn 
 
 
 #generating the tidy data set grouped by student and activity
 cran4<- group_by( cran3, student, activity)
 cran4<- cran3%>%group_by(student, activity)%>%summarize( mean(tBodyAccmeanX) , mean(tBodyAccmeanY) , mean(tBodyAccmeanZ) , mean(tBodyAccstdX) , mean(tBodyAccstdY) , mean(tBodyAccstdZ) , mean(tGravityAccmeanX) , mean(tGravityAccmeanY) , mean(tGravityAccmeanZ) , mean(tGravityAccstdX) , mean(tGravityAccstdY) , mean(tGravityAccstdZ) , mean(tBodyAccJerkmeanX) , mean(tBodyAccJerkmeanY) , mean(tBodyAccJerkmeanZ) , mean(tBodyAccJerkstdX) , mean(tBodyAccJerkstdY) , mean(tBodyAccJerkstdZ) , mean(tBodyGyromeanX) , mean(tBodyGyromeanY) , mean(tBodyGyromeanZ) , mean(tBodyGyrostdX) , mean(tBodyGyrostdY) , mean(tBodyGyrostdZ) , mean(tBodyGyroJerkmeanX) , mean(tBodyGyroJerkmeanY) , mean(tBodyGyroJerkmeanZ) , mean(tBodyGyroJerkstdX) , mean(tBodyGyroJerkstdY) , mean(tBodyGyroJerkstdZ) , mean(tBodyAccMagmean) , mean(tBodyAccMagstd) , mean(tGravityAccMagmean) , mean(tGravityAccMagstd) , mean(tBodyAccJerkMagmean) , mean(tBodyAccJerkMagstd) , mean(tBodyGyroMagmean) , mean(tBodyGyroMagstd) , mean(tBodyGyroJerkMagmean) , mean(tBodyGyroJerkMagstd) , mean(fBodyAccmeanX) , mean(fBodyAccmeanY) , mean(fBodyAccmeanZ) , mean(fBodyAccstdX) , mean(fBodyAccstdY) , mean(fBodyAccstdZ) , mean(fBodyAccJerkmeanX) , mean(fBodyAccJerkmeanY) , mean(fBodyAccJerkmeanZ) , mean(fBodyAccJerkstdX) , mean(fBodyAccJerkstdY) , mean(fBodyAccJerkstdZ) , mean(fBodyGyromeanX) , mean(fBodyGyromeanY) , mean(fBodyGyromeanZ) , mean(fBodyGyrostdX) , mean(fBodyGyrostdY) , mean(fBodyGyrostdZ) , mean(fBodyAccMagmean) , mean(fBodyAccMagstd) , mean(fBodyBodyAccJerkMagmean) , mean(fBodyBodyAccJerkMagstd) , mean(fBodyBodyGyroMagmean) , mean(fBodyBodyGyroMagstd) , mean(fBodyBodyGyroJerkMagmean) , mean(fBodyBodyGyroJerkMagstd) , mean(fBodyAccJerkmeanFreqX) , mean(fBodyAccJerkmeanFreqY) , mean(fBodyAccJerkmeanFreqZ) , mean(fBodyGyromeanFreqX) , mean(fBodyGyromeanFreqY) , mean(fBodyGyromeanFreqZ) , mean(fBodyAccMagmeanFreq) , mean(fBodyBodyAccJerkMagmeanFreq) , mean(fBodyBodyGyroMagmeanFreq) , mean(fBodyBodyGyroJerkMagmeanFreq) ) 

 write.table(cran4, file ="output.txt", row.names = FALSE)
 
 
 
