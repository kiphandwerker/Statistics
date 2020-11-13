d = cbind(c(57,19), c(30,67))

Reliability = function(data, p){
  SE = data[1]/sum(data[,1])
  SP = data[2,2]/sum(data[,2])
  
  Pn = SE*(p)
  Pd = SE*(p) + (1-SP)*(1-p)
  PPV = Pn/Pd
  
  Nn = SP*(1-p)
  Nd = SP*(1-p) + (1- SE)*(p)
  NPV = Nn/Nd
  
  cat('Sensitivity:',SE,'\n')
  cat('Specificity:',SP,'\n')
  cat('PPV:',PPV,'\n')
  cat('NPV:',NPV,'\n')
}

Reliability(d,0.1)