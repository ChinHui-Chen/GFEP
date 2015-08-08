# read table
RS_PT = read.table("./dataset/TRS_MSNN_p13", header=FALSE)

# read weekly data
RS_GT = scan("./dataset/TRS_weekly", skip=4)

en = 0
for( target in (117:144) ) # 117 - 144
{
  start_point = 104
  predict_horizon = 13
  
  base = target - start_point
  
  v = c()
  for(npredict in (1:predict_horizon))
  {
    row = base - predict_horizon + npredict
    col = predict_horizon - npredict +1  
    t = RS_PT[row, col]
    v = c(v, t)
  }
  error = abs(v - RS_GT[target])
  plot( error , type="o" )
  
  for(i in (1:12))
  {
    if( error[i] < error[13] )  
    {
      en = en + 1
      break
    }
  }
  
}


