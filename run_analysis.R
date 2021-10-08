  # Creating directory for storing data.

  if (!file.exists("data4project")) {
		dir.create("data4project")
	}
	
  # Downloading and unzipping file.
  
	url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	download.file(url, destfile = "./data4project/dataset.zip", method = "curl")
	
	if (!file.exists("./data4project/dataset.zip")) {
	   unzip("./data4project/dataset.zip", exdir = "./data4project")
	}
	
	# Getting list of the files.
	
	path2files <- file.path("./data4project" , "UCI HAR Dataset")
	files<-list.files(path2files, recursive=TRUE)
	files
	
	# Reading files.
	
	# features
	x_test  <- read.table(file.path(path2files, "test" , "x_test.txt" ),header = FALSE)
	tx_train <- read.table(file.path(path2files, "train", "x_train.txt"),header = FALSE)
	# subjects
	subj_test  <- read.table(file.path(path2files, "test" , "subject_test.txt" ),header = FALSE)
	subj_train <- read.table(file.path(path2files, "train", "subject_train.txt"),header = FALSE)
	# activity
	y_test  <- read.table(file.path(path2files, "test" , "y_test.txt" ),header = FALSE)
	y_train <- read.table(file.path(path2files, "train", "y_train.txt"),header = FALSE)
	# variables
	name_features <- read.table(file.path(path2files, "features.txt"),head=FALSE)
	
	# Merging train/test data.
	
	X <- rbind(x_test, x_train)
	y <- rbind(y_test, y_train)
	subj <- rbind(subj_test, subj_train)
	
	# Setting column names  
	
	names(X) <- name_features$V2
	names(y) <- c("Activity")
	names(subj) <- c("Subject_number")
	
	# Creating consolidated Dataset.
	
	Dataset <- cbind(subj, y, X)
	
	# Extracting the measurements on the mean and standard deviation.
	
	select_name_features<-name_features$V2[grep("mean\\(\\)|std\\(\\)", name_features$V2)]
	
	# Creating data set with measurements on the mean and standard deviation.
	
	select_names <- c(as.character(select_name_features), "Activity", "Subject_number")
	FinalDataset <- subset(Dataset, select=select_names)
	
	# Adding descriptive activity names to name the activities in the data set.
	
	activity <- read.table(file.path(path2files, "activity_labels.txt"),header = FALSE)
	FinalDataset$Activity<-factor(FinalDataset$Activity,labels=activity[,2])
	
	# Labeling the data set with descriptive variable names. 
	
	names(FinalDataset) <- gsub("^t", "Time", names(FinalDataset))
	names(FinalDataset) <- gsub("^f", "Freq", names(FinalDataset))
	names(FinalDataset) <- gsub("-mean", "Mean", names(FinalDataset))
	names(FinalDataset) <- gsub("-std", "Std", names(FinalDataset))
	names(FinalDataset) <- gsub("[()]", "", names(FinalDataset))
	names(FinalDataset) <- gsub("BodyBody", "Body", names(FinalDataset))
	
	# Creating independent tidy data set with the average of each variable for each activity and each subject.
	
	tidyDataset <-aggregate(. ~Subject_number + Activity, FinalDataset, mean)
	tidyDataset <-tidyDataset[order(tidyDataset$Subject_number,tidyDataset$Activity),]
	write.table(tidyDataset, file = "./tidyDataset.csv",row.name=FALSE,quote = FALSE)
	
	
	
	
	
	
	
	