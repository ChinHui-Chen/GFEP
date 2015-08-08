library("nnet")  # The R built-in neural networks package
library("e1071") # A non-built-in package supports nnet performance tuning

# Parameter settings: file, window, and step
file <- "dataset//BRS_weekly"
inputWindowLen <- 52
numStep <- 1
totalWindowLen <- inputWindowLen + numStep

# Prepare dataset
data <- read.table(file, fileEncoding="utf-8")
## Transforma to time series object in order to use window()
tsdata <- ts(data)
tsLen <- length(tsdata)
inputs <- vector()
targets <- vector()
## Applying windowing transformation
for(i in inputWindowLen:(tsLen-numStep))
{
  inputs <- rbind(inputs, window(tsdata, i-inputWindowLen+1, i)[1:inputWindowLen])
  targets <- rbind(targets, window(tsdata,i+1,i+numStep)[1:numStep])
}
## Split training and testing data (2 years for training)
trainIndices <- 1:(2*52-totalWindowLen+1)
testIndices <- (2*52-totalWindowLen+2):(tsLen-totalWindowLen+1)

# Hotfix for issue 2: use formulae instead of matrix as the input of tune()
dataframe <- data.frame(input=inputs, output=targets)
if(numStep==1)
{
  myformula <- formula("output~.")
}else{
  myformula <- formula(sprintf("cbind(%s)~.", paste("output.", 1:numStep, sep='', collapse=',')))
}

# Start tuning nnet perf  mances using tune() in e1071
nnetTune <- tune(nnet, train.x=myformula, data=dataframe[trainIndices,],
                  size=52, MaxNWts=3000, maxit=300, linout=TRUE, trace=TRUE,
                  ranges=list(rang=seq(0.3,0.3,0.1), decay=c(5,10)),
                  tunecontrol=tune.control(cross=2))

# Start predicting using the best model
prediction <- predict(nnetTune$best.model, dataframe[testIndices,])

# Calculate evaluation metrics: RMSE and NRMSE
best_training_rmse <- sqrt(nnetTune$best.performance)
best_training_nrmse <- 100*(best_training_rmse/(max(targets[trainIndices,],na.rm=TRUE)-min(targets[trainIndices,],na.rm=TRUE)))
testing_rmse <- sqrt(mean((targets[testIndices,]-prediction)^2))
testing_nrmse <- 100*(testing_rmse/(max(targets[testIndices,],na.rm=TRUE)-min(targets[testIndices,],na.rm=TRUE)))
cat("\nBest training data RMSE: ", best_training_rmse)
cat("\nBest training data NRMSE: ", best_training_nrmse)
cat("\nTesting data (the 3rd year) RMSE: ", testing_rmse)
cat("\nTesting data (the 3rd year) NRMSE: ", testing_nrmse)
