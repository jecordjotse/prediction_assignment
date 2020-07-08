library(lattice)
library(ggplot2)
library(dplyr)
library(caret)
library(rpart)

if(!file.exists('data/pml-training.csv')){
    download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv',destfile = 'data/pml-training.csv')
}

if(!file.exists('data/pml-testing.csv')){
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv',destfile = 'data/pml-testing.csv')
}

train <- read.csv('data/pml-training.csv', na.strings = c(NA,"","#DIV/0!"))

test <- read.csv('data/pml-testing.csv', na.strings = c(NA,"","#DIV/0!"))

dim(train)
str(train)
names(train)

set.seed(2020-07-07)
inTrain <- createDataPartition(y=train$classe,p=0.7, list=FALSE)
train.training <- train[inTrain,]
train.testing <- train[-inTrain,]

round(100*sapply(train.training, function(x) sum(!is.na(x)))/nrow(train.training))
#Count number of conplete and non complete columns
table(round(100*sapply(train.training, function(x) sum(!is.na(x)))/nrow(train.training)))

#with 60 complete predictors, I will eliminate the 100 uncompleted tables
train.new_training <-train.training[,(round(100*sapply(train.training, function(x) sum(!is.na(x)))/nrow(train.training))>10)]

# double checking NAs
train.na_training <-train.training[,(round(100*sapply(train.training, function(x) sum(is.na(x)))/nrow(train.training))>10)]
sapply(train.na_training, function(x) sum(!is.na(x)))
#non-NAs are 406 out of 19622

#More Exploratory analysis
str(train.new_training)
sapply(train.new_training[,sapply(train.new_training, function(x) is.factor(x))], function(x) levels(x))

summary(train.new_training)

#heatmap to see classificationsn
training_mat <- train.new_training[,sapply(train.new_training, function(x) !is.factor(x))]
training_mat <- as.matrix(training_mat[,-c(1:3)])

heatmap(training_mat)

qplot(x=new_window, color=classe, data = train.new_training)
qplot(color=new_window, x=classe, data = train.new_training)

train.new_training_model <- train.new_training[,-c(1:6)]
fit <- train( classe ~ .,
              data = train.new_training_model,
              method = 'rf',
              trControl = trainControl( number = 3,
                                        method = 'cv',
                                        verboseIter = FALSE
                                        )
              )
summary(fit)
confusionMatrix(train.testing$classe,predict(fit,train.testing))

#No to pass full training dataset into training model fit
#with 60 complete predictors, I will eliminate the 100 uncompleted tables
new_train <- train[,(round(100*sapply(train, function(x) sum(!is.na(x)))/nrow(train))>10)]
#Select Valid predictors
new_train <- train.new_training[,-c(1:6)]

full_fit <- train( classe ~ .,
              data = new_train,
              method = 'rf',
              trControl = trainControl( number = 3,
                                        method = 'cv',
                                        verboseIter = FALSE
              )
)

test.predictions <- predict(fit,test)
test.predictions_full <- predict(full_fit,test)

fit_10 <- train( as.formula( paste("classe", paste( predictors[1:10], collapse = ' + '), sep = ' ~ ')),
              data = train.new_training_model,
              method = 'rf',
              trControl = trainControl( number = 3,
                                        method = 'cv',
                                        verboseIter = FALSE
              )
)
fit_20 <- train( as.formula( paste("classe", paste( predictors[1:20], collapse = ' + '), sep = ' ~ ')),
              data = train.new_training_model,
              method = 'rf',
              trControl = trainControl( number = 3,
                                        method = 'cv',
                                        verboseIter = FALSE
              )
)

fit_30 <- train( as.formula( paste("classe", paste( predictors[1:30], collapse = ' + '), sep = ' ~ ')),
              data = train.new_training_model,
              method = 'rf',
              trControl = trainControl( number = 3,
                                        method = 'cv',
                                        verboseIter = FALSE
              )
)

fit_40 <- train( as.formula( paste("classe", paste( predictors[1:40], collapse = ' + '), sep = ' ~ ')),
              data = train.new_training_model,
              method = 'rf',
              trControl = trainControl( number = 3,
                                        method = 'cv',
                                        verboseIter = FALSE
              )
)

fit_50 <- train( as.formula( paste("classe", paste( predictors[1:50], collapse = ' + '), sep = ' ~ ')),
                 data = train.new_training_model,
                 method = 'rf',
                 trControl = trainControl( number = 3,
                                           method = 'cv',
                                           verboseIter = FALSE
                 )
)

fit_all <- train( classe ~ .,
                 data = train.new_training_model,
                 method = 'rf',
                 trControl = trainControl( number = 3,
                                           method = 'cv',
                                           verboseIter = FALSE
                 )
)