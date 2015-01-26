# gettingcleaningdataproject

FIRST WE HAVE TO READ THE ACTIVITY LABELS

setwd("G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV/UCI HAR Dataset")
?read.table()
#Read activity labels: activity_labels.txt
activities<-read.table("activity_labels.txt",sep=" ",na.strings="NA")

activities
  V1                 V2
1  1            WALKING
2  2   WALKING_UPSTAIRS
3  3 WALKING_DOWNSTAIRS
4  4            SITTING
5  5           STANDING
6  6             LAYING

THEN, WE HAVE TO READ THE VARIABLE COLUMN NAMES
#Read variable names: features.txt
variablecolumnnames<-read.table("features.txt",sep=" ",na.strings="NA")

head(variablecolumnnames)
  V1                V2
1  1 tBodyAcc-mean()-X
2  2 tBodyAcc-mean()-Y
3  3 tBodyAcc-mean()-Z
4  4  tBodyAcc-std()-X
5  5  tBodyAcc-std()-Y
6  6  tBodyAcc-std()-Z

dim(variablecolumnnames)
[1] 561   2

columns<-as.vector(variablecolumnnames$V2)

# ********************* READ THE FOLDER "TEST" ***********************

#Read column "activity", for the test data table

setwd("G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV/UCI HAR Dataset/test")
?read.fwf()
vteactivity<-read.fwf("y_test.txt",c(1))
head(vteactivity)
  V1
1  5
2  5
3  5
4  5
5  5
6  5
tail(vteactivity)
     V1
2942  2
2943  2
2944  2
2945  2
2946  2
2947  2
dim(vteactivity)
[1] 2947    1

#Read column "subject", for the test data table

vtesubject<-read.fwf("subject_test.txt",c(2))
head(vtesubject)
  V1
1  2
2  2
3  2
4  2
5  2
6  2
tail(vtesubject)
     V1
2942 24
2943 24
2944 24
2945 24
2946 24
2947 24
dim(vtesubject)
[1] 2947    1

#Read variable columns (2947 rows x 561 columns) for the test data table

class(variablecolumnnames)
[1] "data.frame"
tedatatable<-read.table("X_test.txt",sep="",dec=".",col.names=columns,row.names=NULL)
dim(tedatatable)
[1] 2947  561
head(tedatatable,1)

# ********************* READ THE FOLDER "TRAIN" ***********************

#Read column "activity", for the train data table

setwd("G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV/UCI HAR Dataset/train")
vtractivity<-read.fwf("y_train.txt",c(1))
head(vtractivity)
  V1
1  5
2  5
3  5
4  5
5  5
6  5
tail(vtractivity)
     V1
7347  2
7348  2
7349  2
7350  2
7351  2
7352  2
dim(vtractivity)
[1] 7352    1

#Read column "subject", for the train data table

vtrsubject<-read.fwf("subject_train.txt",c(2))
head(vtrsubject)
  V1
1  1
2  1
3  1
4  1
5  1
6  1
tail(vtrsubject)
     V1
7347 30
7348 30
7349 30
7350 30
7351 30
7352 30
dim(vtrsubject)
[1] 7352    1

#Read variable columns (7352 rows x 561 columns) for the train data table

trdatatable<-read.table("X_train.txt",sep="",dec=".",col.names=columns,row.names=NULL)
dim(trdatatable)
[1] 7352  561
head(trdatatable,1)

#********************** MERGE TABLES TEST AND TRAIN *************************

?rbind()
maintable<-rbind(tedatatable,trdatatable)

#Add vector maintablesubject and maintableactivity to the main data table
maintablesubject<-rbind(vtesubject,vtrsubject)
maintableactivity<-rbind(vteactivity,vtractivity)
maintable[,"sub"]  <- maintablesubject
maintable[,"act"]  <- maintableactivity

> dim(maintable)
[1] 10299   563

#********************** SAVE MAIN TABLE TO DISC *************************

write.table(maintable, "G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV/maintable.txt", sep="\t")
write.table(maintable, "G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV/maintable.csv", sep=",")
library(xlsx)
Loading required package: rJava
Loading required package: xlsxjars
write.xlsx(maintable, "G:/Xavier/Estudios/Online/Data Science/Getting and cleaning data/working directory XV/maintable.xlsx")

#********************** CREATE THE NEW AVERAGE TIDY DATA SET *************************
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
