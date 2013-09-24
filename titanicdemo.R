# Code to participate in http://www.kaggle.com/c/titanic-gettingStarted

# change path to the directory where train.csv/test.csv are located.
path <- "~/dev/kaggle/titanic/data_raw/"

colTypes <- c("character", "factor", "factor", "character", "factor", "numeric", "factor", "factor", "character", "numeric", "factor", "factor")

load_data <- function(path, colClassTypes = colTypes) {
  initial <- read.csv(path, colClasses=colClassTypes)
  initial$Age <- as.integer(initial$Age)
  # Move the Survived column to the end
  initial <- subset(initial, select=c(1, 3:ncol(initial), 2))
  return(initial)
} 
 
titanic <- load_data(paste0(path, "train.csv"))
titanic_test <- load_data(paste0(path, "test.csv"), colTypes[-2])

randomforest <- function(inputColumns, toBePredicted) {
  set.seed(1) # make randomForest predictable
  library(randomForest)
  titanic_train_imputed <- rfImpute(Survived ~ ., titanic[,inputColumns])
  rf <- randomForest(Survived ~ ., titanic_train_imputed)
  show(rf)

  titanic_prediction <- predict(rf, toBePredicted)  
  # didn't survive as default
  titanic_prediction[is.na(titanic_prediction)] <- 0 
  # You've seen the movie, right?
  
  output <- data.frame(PassengerId = toBePredicted$PassengerId, 
                       Survived = titanic_prediction)
  return(output)
}

runRandomforest <- function() {
  write_data("randomforest-j1", 
             randomforest(c("Pclass", "Sex", "Age", "Fare", "Survived"), 
                          titanic_test))
} 

write_data <- function(name, data) {
  write.csv(data, quote=FALSE, row.names=FALSE, 
            file=paste0(path, name, ".csv", sep=""))
}