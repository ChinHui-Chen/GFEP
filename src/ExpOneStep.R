source("src//NNetNSteps.R")
source("src//ARIMANSteps.R")
source("src//WHESNSteps.R")

runOnestep <- function(numStep){
  dataset = "dataset//BRS_weekly"
  
  #ph1 = nnetNSteps( dataset , numStep, FALSE, 0.1, 6)
  #ph1 = ArimaNSteps( dataset , numStep )
  ph1 = whesNSteps( dataset , numStep )
  
  gts <- vector()
  predict <- vector()
  for(i in 105:156)
  {
    predict <- rbind(predict, window(ph1$result, i, i)[1] )
    gts <- rbind(gts, window(ph1$gt, i, i)[1] )
  }
  
  plot(gts, type="o")
  lines(predict, type="o", col=2)
  
  rmse <- mean( sqrt( (gts-predict)^2) )
  
  return(rmse)
}

runOnestep(1)
