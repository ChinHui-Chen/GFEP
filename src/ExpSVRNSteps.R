source("src//NNetNSteps.R")
source("src//ARIMANSteps.R")
source("src//Features.R")
# Include SVR packages
library("e1071")


runSVRNSteps <- function( numSVR ){
  
  numStep = 13
  dataset = "dataset//BRS_weekly"
  
  # Phase1 (Model Selection)
  # Train/Predict
  ph1 = nnetNSteps( dataset, numStep, FALSE, 0.1, 10)
  #ph1 = ArimaNSteps( dataset, numStep )
  
  # Phase2 
  # Training instance (52+n -> 104) (numSVR)
  arrSVR = vector()
  for( j in 1:numSVR  )
  {
    inputsTrain <- vector()
    targetsTrain <- vector()
    for(pos in (52+numStep):104)
    {
      # Extract features
      inputsTrain <- rbind(inputsTrain, extractFeatures(pos, numStep, ph1$result, ph1$gt, j ) )
      targetsTrain <- rbind(targetsTrain, ph1$gt[(pos+j-1),] ) 
    }
    
    m <- tune( svm, train.x=inputsTrain, train.y=targetsTrain,  
               ranges = list(gamma = 2^(-3:3), cost = 2^(-1:6)),
               tunecontrol = tune.control(cross=5)
               )
    
    arrSVR = rbind(arrSVR, m)
  }
    
  # Testing (105:(156-numStep+1))
  svrResultList = vector()
  for( j in 1:numSVR  )
  {
    inputsTest <- vector()
    targetsTest <- vector()
    for(pos in 105:(156-numStep+1))
    { 
      # Extract features
      inputsTest <- rbind(inputsTest, extractFeatures(pos, numStep, ph1$result, ph1$gt, j ) )
      targetsTest <- rbind(targetsTest, ph1$gt[(pos+j-1),]) 
    }

    svrResult = predict( ((as.list( arrSVR[j,] ))$best.model) , inputsTest )  
  
    svrResultList = cbind( svrResultList , svrResult  )    
  }
  
  
  # Time to use NN or other models to predict
  # Start to predict h=13
  
  # Predict
  tsData <- ts(read.table(dataset, fileEncoding="utf-8"))
  svrNStepsResultList <- vector()
  svrNStepsBLResultList <- vector()
  svrNStepsResultGTList <- vector()
  for(pos in 105:(156-numStep+1) )
  {
    offset <- pos-104
    svrPredict <- svrResultList[offset,]
  
    # (Model Selection) Fix It!
    # Use NNet
    v <- as.vector( c(window(tsData, pos-numSVR-(52-numSVR)+1, pos-numSVR), svrPredict) ) 
    svrNStepsResult <- predict(ph1$model, v)
    svrNStepsResult <- as.vector( c(svrPredict , svrNStepsResult[1,1:(numStep-numSVR)]) ) 
    
    # Use NN Baseline
    v <- as.vector( window(tsData, pos-52, pos-1 ) )
    svrNStepsBLResult <- predict(ph1$model, v)
    
    
    # Use Arima
#    fit <- Arima( c(window(tsData, 1, pos-1), svrPredict) , model=ph1$model )
#    svrNStepsResult <- as.vector( forecast(fit,h=numStep)$mean)
#    svrNStepsResult <- as.vector( c(svrPredict , svrNStepsResult[1:(numStep-numSVR)]) )
    
    # Use Arima BL
#    fit <- Arima( window(tsData, 1, pos-1), model=ph1$model )
#    svrNStepsBLResult <- as.vector( forecast(fit,h=numStep)$mean )
    
    # Get Ground Truth
    svrNStepsResultGT <- ph1$gt[pos:(pos+numStep-1),]
        
    svrNStepsResultList <- rbind(svrNStepsResultList, svrNStepsResult)
    svrNStepsBLResultList <- rbind( svrNStepsBLResultList, svrNStepsBLResult )
    svrNStepsResultGTList <- rbind( svrNStepsResultGTList, svrNStepsResultGT )
  }
  
  avgRMSE = mean(sqrt( rowMeans( (svrNStepsResultGTList - svrNStepsResultList)^2 ) ))
  avgRMSEBL = mean(sqrt( rowMeans( (svrNStepsResultGTList - svrNStepsBLResultList)^2 ) ))
  
  cat("avgRMSE:", avgRMSE, "\n")
  cat("avgRMSEBL:", avgRMSEBL, "\n")
  cat("Improved:", ((avgRMSE-avgRMSEBL)*-1)/avgRMSEBL, "\n" )
  
  # eval
  return (avgRMSE)  
}


runSVRNSteps(5)




