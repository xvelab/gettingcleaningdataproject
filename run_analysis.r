setwd("G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV/UCI HAR Dataset")
?read.table()
#Read activity labels: activity_labels.txt
activities<-read.table("activity_labels.txt",sep=" ",na.strings="NA",col.names=c("id","activity"))
#Read variable names: features.txt
variablecolumnnames<-read.table("features.txt",sep=" ",na.strings="NA")
columns<-as.vector(variablecolumnnames$V2)

# ********************* FOLDER "TEST" ***********************
#Read column "activity", for the test data table
setwd("G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV/UCI HAR Dataset/test")
vteactivity<-read.fwf("y_test.txt",c(1))
#Read column "subject", for the test data table
vtesubject<-read.fwf("subject_test.txt",c(2))
#Read variable columns (2947 rows x 561 columns) for the test data table
tedatatable<-read.table("X_test.txt",sep="",dec=".",col.names=columns,row.names=NULL)

# ********************* FOLDER "TRAIN" ***********************
#Read column "activity", for the train data table
setwd("G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV/UCI HAR Dataset/train")
vtractivity<-read.fwf("y_train.txt",c(1))
#Read column "subject", for the train data table
vtrsubject<-read.fwf("subject_train.txt",c(2))
#Read variable columns (7352 rows x 561 columns) for the train data table
trdatatable<-read.table("X_train.txt",sep="",dec=".",col.names=columns,row.names=NULL)

#********************** MERGE TABLES TEST AND TRAIN *************************
#***1. Merges the training and the test sets to create one data set.***
maintable<-rbind(tedatatable,trdatatable)
#Add vector maintablesubject and maintableactivity to the main data table
maintablesubject<-as.vector(rbind(vtesubject,vtrsubject))
maintableactivity<-as.vector(rbind(vteactivity,vtractivity))
maintable[,"sub"]  <- maintablesubject
maintable[,"act"]  <- maintableactivity

#********************** MEASUREMENT OF THE MEAN AND STANDARD DEVIATION OF ALL COLUMN VARIABLES *************************
#***2. Extracts only the measurements on the mean and standard deviation for each measurement.***
#***Mean***
variablemeans<-colMeans(maintable,na.rm=TRUE)
#***Standard Deviation***
cols<-c(1:563)
for(col in cols) {
	s<-sd(maintable[,col],na.rm=TRUE)
	if(col==1) stddev<-s
	else stddev<-c(stddev,s)
}
columns<-c(columns,"subject","activity")
variablesd<-data.frame(columns,stddev)

#********************** MERGE TABLES MAINTABLE AND ACTIVITIES *************************
#***3. Uses descriptive activity names to name the activities in the data set***
maintable=merge(maintable,activities,by.x="act",by.y="id",all.x=TRUE)

#***4. Appropriately labels the data set with descriptive variable names. ***
#This was done before (the moment of reading the data):
#Folder test: tedatatable<-read.table("X_test.txt",sep="",dec=".",col.names=columns,row.names=NULL)
#Folder train: trdatatable<-read.table("X_train.txt",sep="",dec=".",col.names=columns,row.names=NULL)
#The labels were assigned with "col.names=columns

#********************** SAVE MAIN TABLE TO DISC *************************
write.table(maintable, "G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV/maintable.csv",sep=",",row.name=FALSE)
write.table(maintable, "G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV/maintable.txt",sep="\t",row.name=FALSE)
library(xlsx)
Loading required package: rJava
Loading required package: xlsxjars
write.xlsx(maintable, "G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV/maintable.xlsx",row.name=FALSE)


#********************** NEW AVERAGE TIDY DATA SET *************************
#***5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each #subject.**

setwd("G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV")
da<-read.csv("maintable.txt", dec = ".")
library(dplyr)
ne<-group_by(da,act,sub)
columns<-names(ne)
#Create script for generating a file with the summarization instructions:
fileConn<-file("output.txt")
for(col in columns) {
	if(col=="act") line<-paste("x<-summarize(ne,average=mean(",col,", na.rm=TRUE))",sep="")
	else line<-c(line, paste("x<-c(x,summarize(ne,average=mean(",col,", na.rm=TRUE)))",sep=""))
	}
writeLines(line, fileConn)
close(fileConn)

#Execute the lines of the script in R console to produce a list of vectors with averages for activity and subject
#length(x)
#[1] 1692
#There are in x: 1692 vectors with 180 values in each one
#x[n]
#[1] activity	+3 = 4, +3 = 7
#[2] sub
#[3] average

#Column activity
for(i in seq(from=1, to=1692, by=3)) {
	if(i==1) activity<-x[i]
	else activity<-c(activity, x[i])
}
activity<-as.vector(unlist(activity))
length(activity)
#[1] 101520

#Column subject
for(i in seq(from=2, to=1692, by=3)) {
	if(i==2) subject<-x[i]
	else subject<-c(subject, x[i])
}
subject<-as.vector(unlist(subject))
length(subject)
#[1] 101520

#Column average
for(i in seq(from=3, to=1692, by=3)) {
	if(i==3) average<-x[i]
	else average<-c(average, x[i])
}
average<-as.vector(unlist(average))
length(average)
#[1] 101520

#Column variable
for(i in seq(from=1, to=564, by=1)) {
	if(i==1) variable<-rep(columns[i],180)
	else variable<-c(variable, rep(columns[i],180))
}
length(variable)

#[1] 101520
tabla1<-data.frame(activity,subject,average,variable)
setwd("G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV/UCI HAR Dataset")
#Read activity labels: activity_labels.txt
ac<-read.table("activity_labels.txt",sep="",na.strings="NA",col.names=c("id","activity"))
tabla=merge(tabla1,ac,by.x="activity",by.y="id",all.x=TRUE)
setwd("G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV")
write.table(tabla,"avgtable.txt",sep=",",row.name=FALSE)





























