#read the data
SubjectTrain<-fread("./train/subject_train.txt")
SubjectTest <-fread("./test/subject_test.txt")

# read the activity files

ActivityTrain <- fread("./train/Y_train.txt")
ActivityTrain <- fread("./train/Y_train.txt")

# read data files
fileToDataTable <- function (f) {
    df <- read.table(f)
    dt <- data.table(df)
}
dtTrain <- fileToDataTable("./train/X_train.txt")
dtTest<- fileToDataTable("./test/X_test.txt")

# merge the data
dtSubject <- rbind(SubjectTrain,SubjectTest)
 setnames(dtSubject, "V1", "subject")
 dtActivity <- rbind(ActivityTrain, ActivityTest)
 setnames(dtActivity, "V1", "activityNum")
 dt <- rbind(dtTrain, dtTest)
 dtSubject <- cbind(dtSubject, dtActivity)
 dt <- cbind(dtSubject, dt)
 setkey(dt, subject, activityNum)


# extract mean and std 
dtFeatures <- fread("features.txt")
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)] 
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]

dtActivityNames <- fread("activity_labels.txt")
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)
setkey(dt, subject, activityNum, activityName)
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)


#Seperate features from featureName using the helper function grepthis.
grepthis <- function (regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2



# create tidy data set
etkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]