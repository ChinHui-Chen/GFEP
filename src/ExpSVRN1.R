source("src//NNetNSteps.R")
source("src//ARIMANSteps.R")
source("src//HWESNSteps.R")
source("src//Features.R")

# Include SVR packages
library("e1071")

runSVRN1 <- function(){
  numStep = 13
  dataset = "dataset//BRS_weekly"
  
  # Phase1
  # Train/Predict
  #ph1 = nnetNSteps( dataset, numStep, FALSE, 0.1, 10)
  #ph1 = ArimaNSteps( dataset, numStep )
  ph1 = hwesNSteps(dataset, numStep)
  
  # Phase2 
  # Training instance (52+4 -> 104)
  inputs_train <- vector()
  targets_train <- vector()
  for(i in (52+numStep):104)
  {
    # extract features
    inputs_train <- rbind(inputs_train, extractFeatures(i, numStep, ph1$result, ph1$gt) )
    targets_train <- rbind(targets_train, ph1$gt[i,] )
  }
  
  m <- tune( svm, train.x=inputs_train, train.y=targets_train,  
            ranges = list(gamma = 2^(-3:3), cost = 2^(-1:6)),
            tunecontrol = tune.control(cross=5)
            )
  
  # Testing (105 -> 156)
  inputs_test <- vector()
  targets_test <- vector()
  for(i in 105:156)
  { 
    inputs_test <- rbind(inputs_test, extractFeatures(i, numStep, ph1$result, ph1$gt) )
    targets_test <- rbind(targets_test, ph1$gt[i,]) 
  }
  
  svr_result = predict(m$best.model, inputs_test)
  rmse <- mean(sqrt((targets_test-svr_result)^2))
  
#  print( cor( inputs_test, targets_test-svr_result, method = c("pearson", "kendall", "spearman") ) )
#  print( cor( inputs_test, targets_test-svr_result, method = "spearman" ) )
  
  return (rmse)
}

runSVRN1()
