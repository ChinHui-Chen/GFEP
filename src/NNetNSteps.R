# Warning: There are no comments. 
library("nnet")

nnetNSteps <- function(file, numStep,CV=FALSE, par_rang=0.1, par_decay=5e-4){
  
  inputWindowLen <- 52
  
  # Prepare dataset
  data <- read.table(file, fileEncoding="utf-8")
  
  tsdata <- ts(data)
  
  # Training phase
  inputs_train <- vector()
  targets_train <- vector()
  
  for(i in inputWindowLen:(2*52-numStep))
  {
    inputs_train <- rbind(inputs_train, window(tsdata, i-inputWindowLen+1, i)[1:inputWindowLen])
    targets_train <- rbind(targets_train, window(tsdata,i+1,i+numStep)[1:numStep])
  }

  
  
  if( CV ){
    # Hotfix for issue 2: use formulae instead of matrix as the input of tune()
    dataframe <- data.frame(input=inputs_train, output=targets_train)
    if(numStep==1)
    {
      myformula <- formula("output~.")
    }else{
      myformula <- formula(sprintf("cbind(%s)~.", paste("output.", 1:numStep, sep='', collapse=',')))
    }
    # Start tuning nnet perf  mances using tune() in e1071
    nnetTune <- tune(nnet, train.x=myformula, data=dataframe[1:nrow(inputs_train),],
                     size=52, MaxNWts=4000, maxit=300, linout=TRUE, trace=TRUE,
                     ranges=list(rang=seq(0.1,0.5,0.1), decay=seq(0,10,2)),
                     #ranges=list(rang=seq(0.1,0.5,0.1), decay=c(5,10)),
                     #ranges=list(rang=seq(0.3,0.3,0.1), decay=c(5)),
                     tunecontrol=tune.control(cross=5))
      
    return(nnetTune)
    
  }else{

    nnetModel <- nnet(inputs_train[1:nrow(inputs_train),], targets_train[1:nrow(targets_train),],
                      size=52, MaxNWts=4000, maxit=300, linout=TRUE, trace=TRUE,
                      rang = par_rang, decay = par_decay)
    
    # Testing phase
    inputs_test <- vector()
    targets_test <- vector()
    for(i in 52:(3*52-1))
    {
      inputs_test <- rbind(inputs_test, window(tsdata, i-inputWindowLen+1, i)[1:inputWindowLen])
      targets_test <- rbind(targets_test, window(tsdata,i+1,i+numStep)[1:numStep])
    }
    #dataframe_test <- data.frame(input=inputs_test, output=targets_test)
    ph1Result <- predict(nnetModel, inputs_test[1:nrow(inputs_test),] )
    
    # calculate avg rmse
    length = 1:(nrow(ph1Result)-numStep+1)
    avg_rmse = 0 
    if( numStep == 1 ){
      avg_rmse = mean(sqrt( (ph1Result[length,] - targets_test[length,])^2 ) )
    }else{
      avg_rmse = mean(sqrt( rowMeans( (ph1Result[length,] - targets_test[length,])^2 ) ))
    }
    
    # return 
    return( list(gt=tsdata, 
                 result= ts(ph1Result, start = 53), 
                 target=  ts(targets_test, start = 53),
                 rmse=avg_rmse,
                 model=nnetModel
                 ) 
    )
    
  }
 
}


# CV for parameters
#BRS_1 = nnetNSteps( "dataset//BRS_weekly" , 1)
#nnetNSteps( "dataset//BRS_weekly" , 2, FALSE, 0.1, 6)
#BRS_13 = nnetNSteps( "dataset//BRS_weekly" , 13)
#TRS_1 = nnetNSteps( "dataset//TRS_weekly" , 1)
#TRS_13 = nnetNSteps( "dataset//TRS_weekly" , 13)
