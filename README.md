# GFEP
A General Framework for Enhancing Prediction Performance on Time Series Data

The code is written in R. 

Total 3 baseline + 1 thesis methods:
  * ARIMA
  * HWES
  * NNet
  * GFEP

File Explanation:  
  * `<method>NSteps.R` (E.g. NNetNSteps.R): library code for specific methods.  
  * `Features.R`: library code for extracting feature in my method.  
  * `Exp<desc>.R` (E.g. ExpSVRNSteps.R): main code for experimenting. Above libs are called. Start from here.  

Presentation slide: [http://www.slideshare.net/chinhuichen/slide-print-fororal](http://www.slideshare.net/chinhuichen/slide-print-fororal)
