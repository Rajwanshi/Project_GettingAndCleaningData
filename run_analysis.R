######################### Function to merge testing and training datasets#################################################


mergeDatasets <- function(X_train, y_train, X_test, y_test, Subject_train, Subject_test){
	
	train <- cbind(X_train, y_train, Subject_train)
	test <- cbind(X_test, y_test, Subject_test)
	merge_data <- rbind(test, train)
	
	for(l in 1:ncol(merge_data)){merge_data[,l] <- as.numeric(as.character(merge_data[,l]))}
	return(merge_data)
	
}
###############################################################################################################################
################################## Function to extract the desired columns(mean and standard deviation )#####################
Sd_Mean <- function(merged_data, variable){
	i=1
	index <- NULL
	while(i <= 161){
		index <- c(index, i:(i+5))
		i <- i+40
	}
	i=201
	while(i <= 253){
		index <- c(index, i:(i+1))
		i <- i+13
		
	}
	
	i=266
	while(i <= 424){
		index <- c(index, i:(i+5))
		i <- i+79
	
	}
	i=503
	while(i <= 542){
		index <- c(index, i:(i+1))
		i <- i+13
	
	}
	index <- c(index, 294:296,373:375,452:454,513,526,539,552)
	index_new <- c(index,562,563)
	merged_data <- merged_data[, index_new]
	colnames(merged_data) <- c(as.character(variable[index,2]), "Activity_Names", "Subject")
	return(merged_data)
}

#######################################################################################################################
################# Function to calculate mean of the columns according to activity names and Subjects#######################
avgVar <- function(data_frame){
	output <- NULL
	for(i in 1:30){
		tem_Data <- data_frame[data_frame$Subject == i,]
		for(j in 1:6){
			if(j==1){
				data_temp2 <- tem_Data[tem_Data$Activity_Names==1,]
				temp <- colMeans(data_temp2[,1:79])
				act_name <- "WALKING"
			} else if(j==2){
				data_temp2 <- tem_Data[tem_Data$Activity_Names==2,]
				temp <- colMeans(data_temp2[,1:79])
				act_name <- "WALKING_UPSTAIRS"
			} else if(j==3){
				data_temp2 <- tem_Data[tem_Data$Activity_Names==3,]
				temp <- colMeans(data_temp2[,1:79])
				act_name <- "WALKING_DOWNSTAIRS"
			} else if(j==4){
				data_temp2 <- tem_Data[tem_Data$Activity_Names==4,]
				temp <- colMeans(data_temp2[,1:79])
				act_name <- "SITTING"
			} else if(j==5){
				data_temp2 <- tem_Data[tem_Data$Activity_Names==5,]
				temp <- colMeans(data_temp2[,1:79])
				act_name <- "STANDING"
			} else if(j==6){
				data_temp2 <- tem_Data[tem_Data$Activity_Names==6,]
				temp <- colMeans(data_temp2[,1:79])
				act_name <- "LAYING"
			}
			temp <- c(temp, act_name, i)
			output <- rbind(output,temp) 
		}
	}
	
	return(output)
}
################## Function to give Activity names ######################################
activityNames <- function(data_frame){
	cols <- (ncol(data_frame) -1)
	data_frame[,cols] <- as.character(data_frame[,cols])
	data_frame[,cols] <- sub("1","WALKING",data_frame[,cols])
	data_frame[,cols] <- sub("2","WALKING_UPSTAIRS",data_frame[,cols])
	data_frame[,cols] <- sub("3","WALKING_DOWNSTAIRS",data_frame[,cols])
	data_frame[,cols] <- sub("4","SITTING",data_frame[,cols])
	data_frame[,cols] <- sub("5","STANDING",data_frame[,cols])
	data_frame[,cols] <- sub("6","LAYING",data_frame[,cols])
	
	return(data_frame)
	
}
##################################################################################################################

##### requred libraries incuded#######
library(plyr)
library(dplyr)
#######################################

####### Tables being read###################
Xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
Xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
S_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
S_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
tempvariable <- read.table("./UCI HAR Dataset/features.txt")
############################################################


###################Functions being called ###########################
x <- mergeDatasets(Xtrain, ytrain, Xtest, ytest, S_train, S_test)
z <- Sd_Mean(x,tempvariable)
b <- avgVar(z)
y <- activityNames(b)
#####################################################################
colnames(y)[80] <- "Activity_Types"
colnames(y)[81] <- "Subjects"

############## Final output file ############################

write.table(y,"tidy_dataSet.txt",row.name=F)

