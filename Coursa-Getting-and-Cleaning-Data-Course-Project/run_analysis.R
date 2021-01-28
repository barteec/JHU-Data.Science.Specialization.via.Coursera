# FILE:  run_analysis.R


# Load Packages and get the Data----------------------------------------------------------------------
    library(data.table) # I like data.table for larger file sets it is faster
    library(reshape2)
    
    wd <- getwd() # will default to your WD
    
    #let us list awebiste where we find our data
    website <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    
    #now we need to go get our data
    download.file(website, file.path(wd, "dataFiles.zip"))
    
    #now unzip into memory of R and or WD
    unzip(zipfile = "dataFiles.zip")

    
# lets get our activity labels plus all the features we will need--------------------------------------
    #fread is really fast in data.table vs using read table or others
    act.Labels <- fread(file.path(wd, "UCI HAR Dataset/activity_labels.txt")
                            , col.names = c("classLabels", "activityName"))
    
    features <- fread(file.path(wd, "UCI HAR Dataset/features.txt")
                      , col.names = c("index", "featureNames"))
    
    #Here we want only want to extract Mean or Standard Deviation Features
    Proj.features <- grep("(mean|std)\\(\\)", features[, featureNames])
    measure <- features[Proj.features, featureNames]
    measure <- gsub('[()]', '', measure)

    
# Train data------------------------------------------------------------------------------------------
    #fread is really fast in data.table vs using read table or others
    X_train <- fread(file.path(wd, "UCI HAR Dataset/train/X_train.txt"))[, Proj.features, with = FALSE]
    
    data.table::setnames(X_train, colnames(X_train), measure) 
    
    #let us read our activities we need
    TR.Activities <- fread(file.path(wd, "UCI HAR Dataset/train/Y_train.txt")
                             , col.names = c("Activity"))
    
    #let us read our Subjects we need
    TR.Subjects <- fread(file.path(wd, "UCI HAR Dataset/train/subject_train.txt")
                           , col.names = c("SubjectNum"))
    
    #combine all train data
    X_train <- cbind(TR.Subjects, TR.Activities, X_train) 

# Test data-------------------------------------------------------------------------------------------
    #fread is really fast in data.table vs using read table or others
    X_test <- fread(file.path(wd, "UCI HAR Dataset/test/X_test.txt"))[, Proj.features, with = FALSE]
    
    data.table::setnames(X_test, colnames(X_test), measure)
    
    #let us read our activities we need
    TS.Activities <- fread(file.path(wd, "UCI HAR Dataset/test/Y_test.txt")
                            , col.names = c("Activity"))
    
    #let us read our Subjects we need
    TS.Subjects <- fread(file.path(wd, "UCI HAR Dataset/test/subject_test.txt")
                          , col.names = c("SubjectNum"))
    
    #combine all test data
    X_test <- cbind(TS.Subjects, TS.Activities, X_test)


# 1. Merges the training and the test sets to create one data set.------------------------------------
    data.merged <- rbind(X_train, X_test)

# Data.table function can do a lot more in one function vs using base R or dplyr. 
# Again Data.table is really fast with large datasets
    
    # 3. Uses descriptive activity names to name the activities in the data set-----------------------
    data.merged[["Activity"]] <- factor(data.merged[, Activity]
                                 , levels = act.Labels[["classLabels"]]
                                 , labels = act.Labels[["activityName"]]
                                 )
    # 4. Appropriately labels the data set with descriptive variable names.---------------------------
    data.merged[["SubjectNum"]] <- as.factor(data.merged[, SubjectNum])
    
    #here we will use the reshape2 package
    data.merged <- reshape2::melt(data = data.merged, id = c("SubjectNum", "Activity"))
    
    # 2. Extracts only the measurements on the mean and standard deviation for each measurement.------
    data.merged <- reshape2::dcast(data = data.merged, SubjectNum + Activity ~ variable, fun.aggregate = mean)

#last step -------------------------------------------------------------------------------------------
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for 
#    each activity and each subject.  
    
#fwrite is really fast for large datasets
data.table::fwrite(x = data.merged, file = "tidyData.csv", quote = FALSE)
# txt file created with write.table() using row.name=FALSE    
write.table(data.merged, "tidy_data.txt", row.names = FALSE, quote = FALSE)

