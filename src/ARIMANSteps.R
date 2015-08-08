library("forecast")

# ArimaTrain <- function(){
#   file = "dataset//BRS_weekly"
#   
#   # Prepare dataset
#   tsdata <- ts( read.table(file, fileEncoding="utf-8") , frequency=52)
#   
#   fit <- auto.arima( window( tsdata, 1, 104 ) )
#   
#   accuracy(fit)
#   
# }

ArimaNSteps <- function(file, numStep){
    
  inputWindowLen <- 52
    
  # Prepare dataset
  tsdata <- ts( read.table(file, fileEncoding="utf-8") )
  tsdata_pd <- ts( read.table(file, fileEncoding="utf-8") , frequency=52)
  
  fitted <- Arima( window( tsdata_pd, start=c(1,1), end=c(2,52) ), 
                   order = c(2,0,1), 
                   seasonal = list(order = c(1,0,0), period=52) ) 
  
  # Testing phase
  targets_test <- vector() 
  ph1Result <- vector() 
  for(i in 52:(3*52-1))
  {
#       fit <- auto.arima( window(tsdata, i-inputWindowLen+1, i)[1:inputWindowLen] , 
#                          max.p = 52, max.q = 52, max.P = 52, max.Q=52, start.p=52,
#                          max.order = 208,
#                          stepwise = TRUE
#                          )
#       
#       fit <- auto.arima(  ts( as.vector( window(tsdata, 1, i) ) , frequency=26) , 
#                          max.p = 15, max.q = 15, max.P = 5, max.Q=5,
#                          max.order = 60,
#                           stepwise = FALSE
#                        )
#      fit <- Arima( ts( as.vector( window(tsdata, 1, i) ) , frequency=26) ,
#                    order=c(0,0,0),
#                    seasonal=list(order=c(1,0,0))
#                        )
      fit <- Arima( window(tsdata, 1, i) , model=fitted )
    
      #plot( forecast(fit,h=numStep) )
      
      ph1Result <- rbind( ph1Result, as.vector( forecast(fit,h=numStep)$mean) )
      targets_test <- rbind(targets_test, window(tsdata,i+1,i+numStep)[1:numStep])
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
  return( list( gt=tsdata,
                result= ts(ph1Result, start = 53), 
                target= ts(targets_test, start = 53),
                rmse=avg_rmse,
                model=fitted) 
  )
}

#ArimaNSteps("dataset//BRS_weekly", 13)

