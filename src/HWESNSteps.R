library("forecast")

hwesNSteps <- function(file, numStep){

  inputWindowLen <- 52
  
  # Prepare dataset
  data <- read.table(file, fileEncoding="utf-8")
  tsdata <- ts( ( c(t(data)[1:52], t(data)) ) )
  
  # Testing phase
  targets_test <- vector()
  ph1Result <- vector()
  for(i in 52:(3*52-1))
  {
    i_bar = i+52
    
#    fit <- HoltWinters( ts( as.vector( window(tsdata, i-inputWindowLen+1, i) ), frequency=26) )
    fit <- HoltWinters( ts( as.vector( window(tsdata, 1, i_bar) ), frequency=52) )
    
#    plot( forecast(fit,h=numStep) )
    
    ph1Result <- rbind( ph1Result, as.vector( forecast(fit,h=numStep)$mean) )
    targets_test <- rbind(targets_test, window( tsdata, i_bar+1, i_bar+numStep )[1:numStep])  
    
  }
    
  # calculate avg rmse
  length = 1:(nrow(ph1Result)-numStep+1)
  avg_rmse = 0 
  if( numStep == 1 ){
    avg_rmse = mean(sqrt( (ph1Result[length,] - targets_test[length,])^2 ) )
  }else{
    avg_rmse = mean(sqrt( rowMeans( (ph1Result[length,] - targets_test[length,])^2 ) ))
  }
  
  
  
  # return 
  return( list( gt= ts(data) ,
                result= ts(ph1Result, start = 53), 
                target= ts(targets_test, start = 53),
                rmse=avg_rmse) 
  )
  
}

#hwesNSteps("dataset//BRS_weekly", 13)