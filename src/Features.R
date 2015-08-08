library("lsa")

# Build function for extracting features
extractFeatures <- function(pos, numStep, testResult, gt, numSVR = 1)
{
  featureS3 = TRUE

  # Calculate pos with offset
  posOffset = pos+numSVR-1
  
  # S3 Periodicity 
  period = 52
  periodFeature = gt[posOffset-period]
  
#  periodResult = gt[(posOffset-period-1):(posOffset-period-numStep+1)]
  length = numStep-numSVR
  periodResult = gt[(pos-period-1 -length+1 ):(pos-period-1)]  

  # S1 Statistic
  
  # Get all predicted results of pos
  allPredicted = c()
  for(i in 1:(numStep-numSVR+1))
  {
    x = window(testResult, pos-i+1, pos-i+1)[,(i+numSVR-1)]
    allPredicted = c(allPredicted,x)
  }
  if(featureS3)
  {
    allPredicted = c(allPredicted, periodFeature)  
  }
  
  s1 = c()
  # mean
  s1 = c(s1, mean(allPredicted) )
  
  # last N
  s1 = c(s1, allPredicted[1] )
  
  # Gaussian Distribution
  s1 = c(s1,  rnorm(1, mean(allPredicted), sd(allPredicted))  ) 

  # S2 Reliability
  pre = c()
  for(i in 1:(numStep-numSVR+1))
  { 
    leftPos <- (pos - i + 1)
    length <- (i - 1)
      
    if(i == 1){
      preResult <- NA
      preGT <- NA
    }else{
      preResult <- window(testResult, leftPos, leftPos)[, 1:length]
      preGT <- c( gt[leftPos:(pos-1)] )
    } 
    preFeature <- window(testResult, leftPos, leftPos)[ ,(i+numSVR-1)]
    
    pre = rbind( pre , list( f=preFeature, r=rev(preResult) , gt=rev(preGT) ))
  }
  
  if(featureS3)
  {
    pre = rbind( pre , list( f=periodFeature, r=rev(periodResult) , gt=rev( gt[  (pos-(numStep-numSVR)):(pos-1)]  ) ))
  }
  
  # Get pre results
  error_min <- 9999999999
  error_feature <- 0
  error_min_last <- 999999999
  error_feature_last <- 0 
  trend_max <- -99999999
  trend_feature <- 0

  for(i in 1:nrow(pre))
  {
    if(i == 1){
      next
    }
    
    # avg error
    e <- mean( abs( pre[i,]$r - pre[i,]$gt ) )
    if( e < error_min )
    {
      error_min <- e
      error_feature <- pre[i,]$f
    }
    
    # error last one
    e <- mean( abs( pre[i,]$r[1] - pre[i,]$gt[1] )  )
    if( e < error_min_last )
    {
      error_min_last <- e
      error_feature_last <- pre[i,]$f
    }    
    
    # trend (trend_count=4)
    trend_count <- 4
    if( length(pre[i,]$r) >= trend_count )
    {
       tr <- cosine( diff( pre[i,]$r[1:trend_count] , differences=1 ) ,
                    diff( pre[i,]$gt[1:trend_count] , differences=1 ) )
       
       if( (!is.nan(tr)) && (tr > trend_max) )  
       {
         trend_max <- tr
         trend_feature <- pre[i,]$f
       }
    }
  }
  
  s2 = c()
  s2 = c(s2, error_feature)
  s2 = c(s2, error_feature_last)
  s2 = c(s2, trend_feature)
  
  return( c(s1,s2) )
  
  
#   for(i in 1:numStep)
#   {
#     if(i == 1){
#       next
#     }
#     
#     leftPos <- (pos - i + 1)
#     length <- (i - 1)
#     
#     preResult <- window(testResult, leftPos, leftPos)[, 1:length]
#     preFeature <- window(testResult, leftPos, leftPos)[ ,i]
#     preGT <- c( gt[leftPos:(pos-1)] )
#     
#     # avg error
#     e <- mean( abs( preResult - preGT ) )
#     if( e < error_min )
#     {
#       error_min <- e
#       error_feature <- preFeature
#     }
#     
#     # error last one
#     e <- mean( abs( preResult[length] - preGT[length] )  )
#     if( e < error_min_last )
#     {
#       error_min_last <- e
#       error_feature_last <- preFeature
#     }
#     #print("error")
#     #print(e)
#     #print(error_min_last)
#     #print(error_feature_last)
#     
#     # trend (trend_count=4)
#     trend_count <- 4
#     if( (i-1)>=trend_count )
#     {
#       tr <- cosine( diff( preResult[(i-trend_count):length] , differences=1 ) ,
#                    diff( preGT[(i-trend_count):length] , differences = 1 )
#                  )
#       if( tr > trend_max )  
#       {
#         trend_max <- tr
#         trend_feature = preFeature  
#       }  
#     }
#     
#     
#     
#   }
#   
#   # For S3
#   if(featureS3)
#   {
#     
#   }
#   
#   
#   s2 = c(s2, error_feature)
#   s2 = c(s2, error_feature_last)
#   s2 = c(s2, trend_feature)
#   
#   #print(error_feature)
#   
#     
#   return( c(s1,s2,s3) )
  #return( c(s2) )
  #return(allPredicted)
}
