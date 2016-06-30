###########Function to merge the required data ####################################

merge_datasets <- function(data_X_train, data_y_train, data_X_test, data_y_test, Subject_train, Subject_test){
	
	data_train <- cbind(data_y_train, Subject_train, data_X_train)
	data_test <- cbind(data_y_test, Subject_test, data_X_test)
	merge_data <- rbind(data_test, data_train)
	return(merge_data)
	
}

######################################################


##################Function to calculate means of the required columns######################

avg_variables <- function(data_frame){
	
	
	Output <- NULL
	for(i in 1:30){
		data_temp <- data_frame[data_frame$Subject == i,]
		for(j in 1:6){
			if(j==1){
				data_temp2 <- data_temp[data_temp$Activity_Names==1,]
				temp <- colMeans(data_temp2[,3:81])
				act_name <- "WALKING"
			} else if(j==2){
				data_temp2 <- data_temp[data_temp$Activity_Names==2,]
				temp <- colMeans(data_temp2[,3:81])
				act_name <- "WALKING_UPSTAIRS"
			} else if(j==3){
				data_temp2 <- data_temp[data_temp$Activity_Names==3,]
				temp <- colMeans(data_temp2[,3:81])
				act_name <- "WALKING_DOWNSTAIRS"
			} else if(j==4){
				data_temp2 <- data_temp[data_temp$Activity_Names==4,]
				temp <- colMeans(data_temp2[,3:81])
				act_name <- "SITTING"
			} else if(j==5){
				data_temp2 <- data_temp[data_temp$Activity_Names==5,]
				temp <- colMeans(data_temp2[,3:81])
				act_name <- "STANDING"
			} else if(j==6){
				data_temp2 <- data_temp[data_temp$Activity_Names==6,]
				temp <- colMeans(data_temp2[,3:81])
				act_name <- "LAYING"
			}
			temp <- c(act_name, i, temp)
			Output <- rbind(Output,temp) 
		}
	}
	colnames(Output)[1] <- "Activity Names"
	colnames(Output)[2] <- "Subject"
	return(Output)
}
######################################################


#  Using Regular Expressions in 'grep' to extract variables which represent either Mean or
#  Standard Deviation as is desired in the exercise (Point 2)

mean_sd_extract <- function(mergedData, variable_names){

	reqVariables <- grep(".*mean.*|.*std.*", variable_names[,2])	
	reqVariables <- reqVariables +2
	mergedData <- mergedData[,c(1,2,reqVariables)]
	temp_var_names <- variable_names[(reqVariables-2),2] 
	temp_var_names<- gsub("-mean","Mean", temp_var_names)
	temp_var_names<- gsub("-std","StdDev", temp_var_names)
	temp_var_names<- gsub("[-()]","", temp_var_names)
	colnames(mergedData) <- c("Activity_Names", "Subject",temp_var_names)
	return(mergedData)
}
######################################################



######  Libraries used#######
library(reshape2)
library(plyr)
library(dplyr)

################################


#######################  Reading files as tables ##################################3
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
Sub_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
Sub_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
temp_variable <- read.table("./UCI HAR Dataset/features.txt")
activity_names <- read.table("./UCI HAR Dataset/activity_labels.txt") 

##############################################################################

x <- merge_datasets(X_train, y_train, X_test, y_test, Sub_train, Sub_test)
z <- mean_sd_extract(x,temp_variable)
b <- avg_variables(z)

###################### Creating the final file ###############################
 
write.table(b,"tidy_dataSet.txt", row.name=FALSE)





############################END OF CODE#####################################
