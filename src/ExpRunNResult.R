source("src//ExpSVR.R")
#source("src//ExpOneStep.R")

rmses1SVR <- vector()
rmses1 <- vector()
rmses13 <- vector()

itr = 20

for(i in 1:itr)
{
  r = runSVR()
  rmses1SVR <- rbind( rmses1SVR, r )
  
  # r1= runOnestep(1)
  # rmses1 <- rbind( rmses1, r1)

  # r13 = runOnestep(13)
  # rmses13 <- rbind( rmses13, r13)
  
}

cat("rmses1SVR:", mean(rmses1SVR) )


#print(mean(rmses1))
#print(mean(rmses13))
