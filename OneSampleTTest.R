OnesampTT = function(d, h0){
  xbar = mean(d)
  s2 = sum(((d - xbar)^2)/ (length(d) - 1))
  s = sqrt(s2) 
  
  tstat = (xbar - h0) / (s/sqrt(length(d)))
  cat('Tstat:   ', tstat,'\n')
  cat('P-value: ', 2*(1 - pt(tstat,length(d)-1)),'\n')
  cat('One-sided: P(',h0,'>',tstat,') =',1-(1-pt(tstat,length(d)-1)),'\n')
  cat('One-sided: P(',h0,'<',tstat,') =',(1 - pt(tstat,length(d)-1)),'\n')
}

d = c(32,28,35,23,26,27,19,23,26,27,25,24,30,32,31,29,28,27)
OnesampTT(d, 30)