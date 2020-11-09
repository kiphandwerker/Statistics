McNTest = function(a,b,c,d, alpha){
  chi2stat = (b-c)^2/(b+c)
  crit = qchisq(1-alpha, 1)
  
  pvalue = (1 - pchisq(chi2stat,1))
  
  cat('Chi2 Stat: ',chi2stat,'\n')
  cat('Critical Value: ',crit,'\n')
  cat('P-value:', pvalue,'\n')
}

McNTest(2,2,6,6,0.05)