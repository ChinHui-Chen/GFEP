# This code was done in a rush. Need to refactor it in the future.

source("src//NNetNSteps.R")
source("src//ARIMANSteps.R")
source("src//WHESNSteps.R")
source("src//Features.R")
# Include SVR packages
library("e1071")

runSVR <- function(){
  numStep = 13
  dataset = "dataset//BRS_weekly"
  
  # Phase1
  # Train/Predict
  #ph1 = nnetNSteps( dataset, numStep, FALSE, 0.1, 10)
  ph1 = ArimaNSteps( dataset, numStep )
  #ph1 = whesNSteps(dataset, numStep)
  
  # Phase2 
  # Training instance (52+4 -> 104)
  inputs_train <- vector()
  targets_train <- vector()
  for(i in (52+numStep):104)
  {
    # extract features
    inputs_train <- rbind(inputs_train, extractFeatures(i, numStep, ph1$result, ph1$gt) )
    targets_train <- rbind(targets_train, ph1$gt[i,] )
    
  #  k = extractFeatures(i, numStep, ph1$result, ph1$gt)
  #  l = ph1$gt[i,] 
  #  plot(k, type="o", col=2)
  #  lines(l)
  }
  
  m <- tune( svm, train.x=inputs_train, train.y=targets_train,  
            ranges = list(gamma = 2^(-3:3), cost = 2^(-1:6)),
            tunecontrol = tune.control(cross=5)
            )
  
#  m <- svm(inputs_train, targets_train, cross=5)
  
  # Testing (105 -> 156)
  inputs_test <- vector()
  targets_test <- vector()
  for(i in 105:156)
  { 
    inputs_test <- rbind(inputs_test, extractFeatures(i, numStep, ph1$result, ph1$gt) )
    targets_test <- rbind(targets_test, ph1$gt[i,]) 
  }
  
  svr_result = predict(m$best.model, inputs_test)
  
  # train
  data <- read.table(dataset, fileEncoding="utf-8")
  tsdata <- ts( data )
  tsdata_pd <- ts( data , frequency=52)
  fitted <- Arima( window( tsdata_pd, start=c(1,1), end=c(2,52) ), 
                   order = c(2,0,1), 
                   seasonal = list(order = c(1,0,0), period=52) ) 
  

  # predict
  svr_result_long <- vector()
  svr_result_no_svr <- vector()
  svr_result_gt <- vector()
  for(i in 105:(156-numStep+1) )
  {
    offset_i <- i-104
    
    one_predict <- svr_result[offset_i]
    
    # ARIMA: 1:i-1:one_predict
    fit <- Arima( rbind( window(tsdata, 1, i-1), one_predict) , model=fitted )
    fit_no_svr <- Arima( window(tsdata, 1, i-1) , model=fitted )
    
    # HW
#    fit <- HoltWinters( ts( c(as.vector( window(tsdata, 1, i-1) ), one_predict) , frequency=52) )
#    fit_no_svr <- HoltWinters( ts( as.vector( window(tsdata, 1, i-1) ), frequency=52) )
    
    # predict h=12 + one_predict
    svr_result_long <- rbind( svr_result_long, c(one_predict, as.vector(forecast(fit,h=(numStep-1))$mean) ) )
    
    # predict h=13
    svr_result_no_svr <- rbind( svr_result_no_svr, as.vector(forecast(fit_no_svr,h=(numStep))$mean)  )
    
    # ground truth
    svr_result_gt <- rbind( svr_result_gt, ph1$gt[i:(i+numStep-1),] )
    
  }
  
  avg_rmse = mean(sqrt( rowMeans( (svr_result_gt - svr_result_long)^2 ) ))
  avg_rmse_no_svr = mean(sqrt( rowMeans( (svr_result_gt - svr_result_no_svr)^2 ) ))
  cat("avg_rmse:", avg_rmse, "\n")
  cat("avg_rmse_no_svr", avg_rmse_no_svr, "\n")
  
  # eval
  return (avg_rmse)
  
}

runSVR()
