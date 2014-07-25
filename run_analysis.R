# R script to create a tidy date set and run the required analysis.

# data description @(at) "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"

dataurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"  # gets the url for the data

#if(!file.exists("data_smartphones")) {     #creates a directory if it does already exists
#        dir.create("data_smartphones")
#}

#download.file(dataurl,destfile="./data_smartphones/smartphoneData.zip",mode="wb")    #downloads data
DateDownloaded<-date()                                                               #registers the date the download took place 

#read into R the X_test.txt file

x_test<-read.table("./UCI HAR Dataset/test/X_test.txt")                     
x_train<-read.table("./UCI HAR Dataset/train/X_train.txt")

features_var<-read.table("UCI HAR Dataset/features.txt",stringsAsFactor=TRUE)  #read the features.txt
features<-features_var[,2]       # gets the features variables into a vector
colnames(x_test)<-features        #puts the vector of variables as column names
colnames(x_train)<-features
x_data<-rbind(x_test,x_train)


y_test<-read.table("./UCI HAR Dataset/test/y_test.txt",colClasses="factor")
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt",colClasses="factor")
y_data<-rbind(y_test,y_train)
colnames(y_data)<-"Activity"

library(plyr)
y_data$Activity<-revalue(y_data$Activity,c("1"="Walking","2"="Walking Upstairs"
                          ,"3"="Walking Downstairs","4"="Sitting","5"="Standing"   
                          ,"6"="Laying"))                                          #renames the Activity variables into readable, descriptive variables


subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt",colClasses="factor")
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt",colClasses="factor")
subject_data<-rbind(subject_test,subject_train)
colnames(subject_data)<-"Subject"

#subset the data into only mean & std measument data.

Feat<-grep("*-mean|-std*",features_var[,2])
TestPhones<-x_data[,c(Feat)]

testTidy<-cbind(TestPhones,y_data,subject_data)

NewColumns<-c("timeBodyAccelerometerMeanX","timeBodyAccelerometerMeanY","timeBodyAccelerometerMeanZ"
              ,"timeBodyAccelerometerStdDevX","timeBodyAccelerometerStdDevY","timeBodyAccelerometerStdDevZ"
              ,"timeGravityAccelerometerMeanX","timeGravityAccelerometerMeanY","timeGravityAccelerometerMeanZ"
              ,"timeGravityAccelerometerStdDevX","timeGravityAccelerometerStdDevY","timeGravityAccelerometerStdDevZ"
              ,"timeBodyAccelerometerJerkMeanX","timeBodyAccelerometerJerkMeanY","timeBodyAccelerometerJerkMeanZ"
              ,"timeBodyAccelerometerJerkStdDevX","timeBodyAccelerometerJerkStdDevY","timeBodyAccelerometerJerkStdDevZ"
              ,"timeBodyGyroscopeMeanX","timeBodyGyroscopeMeanY","timeBodyGyroscopeMeanZ"
              ,"timeBodyGyroscopeStdDevX","timeBodyGyroscopeStdDevY","timeBodyGyroscopeStdDevZ"
              ,"timeBodyGyroscopeJerkMeanX","timeBodyGyroscopeJerkMeanY","timeBodyGyroscopeJerkMeanZ"
              ,"timeBodyGyroscopeJerkStdDevX","timeBodyGyroscopeJerkStdDevY","timeBodyGyroscopeJerkStdDevZ"
              ,"timeBodyAccelerometerMagnitudeMean","timeBodyAccelerometerMagnitudeStdDev"
              ,"timeGravityAccelerometerMagnitudeMean","timeGravityAccelerometerMagnitudeStdDev"
              ,"timeBodyAccelerometerJerkMagnitudeMean","timeBodyAccelerometerJerkMagnitudeStdDev"
              ,"timeBodyGyroscopeMagnitudeMean","timeBodyGyroscopeMagnitudeStdDev"
              ,"timeBodyGyroscopeJerkMagnitudeMean","timeBodyGyroscopeJerkMagnitudeStdDev"
              ,"FreqDomainBodyAccelerometerMeanX","FreqDomainBodyAccelerometerMeanY","FreqDomainBodyAccelerometerMeanZ"
              ,"FreqDomainBodyAccelerometerStdDevX","FreqDomainBodyAccelerometerStdDevY","FreqDomainBodyAccelerometerStdDevZ"
              ,"FreqDomainBodyAccelerometerMeanFreqX","FreqDomainBodyAccelerometerMeanFreqY"
              ,"FreqDomainBodyAccelerometerMeanFreqZ","FreqDomainBodyAccelerometerJerkMeanX"
              ,"FreqDomainBodyAccelerometerJerkMeanY","FreqDomainBodyAccelerometerJerkMeanZ"
              ,"FreqDomainBodyAccelerometerJerkStdDevX","FreqDomainBodyAccelerometerJerkStdDevY"
              ,"FreqDomainBodyAccelerometerJerkStdDevZ","FreqDomainBodyAccelerometerJerkMeanFreqX"
              ,"FreqDomainBodyAccelerometerJerkMeanFreqY","FreqDomainBodyAccelerometerJerkMeanFreqZ"
              ,"FreqDomainBodyGyroscopeMeanX","FreqDomainBodyGyroscopeMeanY","FreqDomainBodyGyroscopeMeanZ"
              ,"FreqDomainBodyGyroscopeStdDevX","FreqDomainBodyGyroscopeStdDevY","FreqDomainBodyGyroscopeStdDevZ"
              ,"FreqDomainBodyGyroscopeMeanFreqX","FreqDomainBodyGyroscopeMeanFreqY","FreqDomainBodyGyroscopeMeanFreqZ"
              ,"FreqDomainBodyAccelerometerMagnitudeMean","FreqDomainBodyAccelerometerMagnitudeStdDev"
              ,"FreqDomainBodyAccelerometerMagnitudeMeanFreq","FreqDomainBodyAccelerometerJerkMagnitudeMean"
              ,"FreqDomainBodyAccelerometerJerkMagnitudeStdDev","FreqDomainBodyAccelerometerJerkMagnitudeMeanFreq"
              ,"FreqDomainBodyGyroscopeMagnitudeMean","FreqDomainBodyGyroscopeMagnitudeStdDev"
              ,"FreqDomainBodyGyroscopeMagnitudeMeanFreq","FreqDomainBodyGyroscopeJerkMagnitudeMean"
              ,"FreqDomainBodyGyroscopeJerkMagnitudeStdDev","FreqDomainBodyGyroscopeJerkMagnitudeMeanFreq"
              ,"Activity","Subject")

colnames(testTidy)<-NewColumns

library(reshape2)
Tidy<-melt(testTidy,id=c("Activity","Subject"))                     #Changes the data from a wide table to a tall table
TidyTidy<-dcast(Tidy,Subject + Activity ~ variable, mean)           #Summeraizes the data by subject, by activity  
TidyTidy$Subject<-as.numeric(TidyTidy$Subject)
TidyTidy<-arrange(TidyTidy,Subject)

write.table(TidyTidy,"TidyTidy.txt",row.names=FALSE)                #write the tidy data into ur working directory
