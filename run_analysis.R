library(data.table)
library(plyr)

if (file.exists("test") & file.exists("train")){
  print("Running....")
} else {
  print("Please place me in the 'UCI HAR Dataset' or run me where the current working directory is 'UCI HAR Dataset'...")
  print("Usage: source('./run_analysis.R')")
  stop()
}

files <- c('body_acc_x_','body_acc_y_', 'body_acc_z_', 'body_gyro_x_', 'body_gyro_y_', 'body_gyro_z_', 'total_acc_x_', 'total_acc_y_', 'total_acc_z_')
type <- c('test', 'train')
activity_list <- c('WALKING','WALKING_UPSTAIRS','WALKING_DOWNSTAIRS','SITTING','STANDING','LAYING')
test_activites <- read.table('./test/y_test.txt')  
train_activities <- read.table('./train/y_train.txt')
test_subject <- read.table('./test/subject_test.txt')
train_subject <- read.table('./train/subject_train.txt')

#function that takes two files to merge and the associated lists to perform the merge with 
#all of the necessary variables
one <- function(file_list,file,test_activites,train_activities,test_subject,train_subject){
  x <- file_list[1]
  y <- file_list[2]
  message <- paste0('Merging ',x,' and ',y)
  print(message)
  col_name1 <- paste0("subject")
  col_name2 <- paste0("activity")
  col_name3 <- paste0(file,"mean")
  col_name4 <- paste0(file,"stddev")
  ms_table <- data.table()
  test <- read.delim(x, sep = "")
  train <- read.delim(y, sep = "")
  #compute the mean of each set of observations (each row) for each type and row bind it to 
  #larger table
  for (tt in c('test','train')){
    if (tt == 'test'){
      table <- test
      for (i in c(1:dim(table)[1])){
        subject <- test_subject[i,1]
        activity_id <- test_activites[i,1]
        activity <- activity_list[activity_id]
        means <- rowMeans(table[i,],dims=1)
        stddev <- sd(table[i,])
        ms_table <- rbindlist(list(ms_table,list(subject,activity,as.numeric(means),stddev)))
      }
    }
    else if (tt == 'train'){
      table <- train
      for (i in c(1:dim(table)[1])){
        subject <- train_subject[i,1]
        activity_id <- test_activites[i,1]
        activity <- activity_list[activity_id]
        means <- rowMeans(table[i,],dims=1)
        stddev <- sd(table[i,])
        ms_table <- rbindlist(list(ms_table,list(subject,activity,as.numeric(means),stddev)))
      }
    }
  }
  #set the names of the data table and return it
  setnames(ms_table,c(col_name1,col_name2,col_name3,col_name4))
  ms_table
}

DT <- data.table()

#main
#loop through the list of files and types and pass each file from both datasets to the
#one function which merges the two files into one data_table,coputes the mean and std
#and returns a table with mean and std for all observations.  Then add this merged data
#table to a single table of all mean and std measuments.
#
for (file in files){
  filez <- c()
  for (dir in type){
    file_dir <- paste0('./',dir,'/','Inertial Signals','/',file,dir,'.txt')
    filez <- c(filez,file_dir)
  }      
  mean_std_dev <- one(filez,file,test_activites,train_activities,test_subject,train_subject)
  #cbind returned data.table object to DT
  if (length(DT) == 0) {  
    DT <- data.table(mean_std_dev)
  } else {
    DT <- data.table(DT,mean_std_dev)
  }
}

print("Merged single table has been created in a data.table object named DT - Variable names (subject,activity,measurments...)")
print("Computing averages for each subject and activity for each measument type")

#for each measument type, compute the average with ddplyr
#there probably exists a better way to do this, but I couldnt figure it out with the variable names
#that I set up.....
body_acc_x_stddev_avg <- ddply(DT,.(subject,activity),summarise,average=mean(body_acc_x_stddev))
average <- data.table(subject=body_acc_x_stddev_avg[,1],activity=body_acc_x_stddev_avg[,2],body_acc_x_stddev_avg=body_acc_x_stddev_avg[,3])
body_acc_x_mean_avg <- ddply(DT,.(subject,activity),summarise,average=mean(body_acc_x_mean))
average <- data.table(average,body_acc_x_mean_avg=body_acc_x_mean_avg[,3])
body_acc_y_stddev_avg <- ddply(DT,.(subject,activity),summarise,average=mean(body_acc_y_stddev))
average <- data.table(average,body_acc_y_stddev_avg=body_acc_y_stddev_avg[,3])
body_acc_y_mean_avg <- ddply(DT,.(subject,activity),summarise,average=mean(body_acc_y_mean))
average <- data.table(average,body_acc_y_mean_avg=body_acc_y_mean_avg[,3])
body_acc_z_stddev_avg <- ddply(DT,.(subject,activity),summarise,average=mean(body_acc_z_stddev))
average <- data.table(average,body_acc_z_stddev_avg=body_acc_z_stddev_avg[,3])
body_acc_z_mean_avg <- ddply(DT,.(subject,activity),summarise,average=mean(body_acc_z_mean))
average <- data.table(average,body_acc_z_mean_avg=body_acc_z_mean_avg[,3])
body_gyro_x_stddev_avg <- ddply(DT,.(subject,activity),summarise,average=mean(body_gyro_x_stddev))
average <- data.table(average,body_gyro_x_stddev_avg=body_gyro_x_stddev_avg[,3])
body_gyro_x_mean_avg <- ddply(DT,.(subject,activity),summarise,average=mean(body_gyro_x_mean))
average <- data.table(average,body_gyro_x_mean_avg=body_gyro_x_mean_avg[,3])
body_gyro_y_stddev_avg <- ddply(DT,.(subject,activity),summarise,average=mean(body_gyro_y_stddev))
average <- data.table(average,body_gyro_y_stddev_avg=body_gyro_y_stddev_avg[,3])
body_gyro_y_mean_avg <- ddply(DT,.(subject,activity),summarise,average=mean(body_gyro_y_mean))
average <- data.table(average,body_gyro_y_mean_avg=body_gyro_y_mean_avg[,3])
body_gyro_z_stddev_avg <- ddply(DT,.(subject,activity),summarise,average=mean(body_gyro_z_stddev))
average <- data.table(average,body_gyro_z_stddev_avg=body_gyro_z_stddev_avg[,3])
body_gyro_z_mean_avg <- ddply(DT,.(subject,activity),summarise,average=mean(body_gyro_z_mean))
average <- data.table(average,body_gyro_z_mean_avg=body_gyro_z_mean_avg[,3])
total_acc_x_stddev_avg <- ddply(DT,.(subject,activity),summarise,average=mean(total_acc_x_stddev))
average <- data.table(average,total_acc_x_stddev_avg=total_acc_x_stddev_avg[,3])
total_acc_x_mean_avg <- ddply(DT,.(subject,activity),summarise,average=mean(total_acc_x_mean))
average <- data.table(average,total_acc_x_mean_avg=total_acc_x_mean_avg[,3])
total_acc_y_stddev_avg <- ddply(DT,.(subject,activity),summarise,average=mean(total_acc_y_stddev))
average <- data.table(average,total_acc_y_stddev_avg=total_acc_y_stddev_avg[,3])
total_acc_y_mean_avg <- ddply(DT,.(subject,activity),summarise,average=mean(total_acc_y_mean))
average <- data.table(average,total_acc_y_mean_avg=total_acc_y_mean_avg[,3])
total_acc_z_stddev_avg <- ddply(DT,.(subject,activity),summarise,average=mean(total_acc_z_stddev))
average <- data.table(average,total_acc_z_stddev_avg=total_acc_z_stddev_avg[,3])
total_acc_z_mean_avg <- ddply(DT,.(subject,activity),summarise,average=mean(total_acc_z_mean))
average <- data.table(average,total_acc_z_mean_avg=total_acc_z_mean_avg[,3])

print("A new data.table named average contains the averages for each measurment grouped by subject and activity")
