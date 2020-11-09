TwoSampProp = function(pi1,pi2,x1,x2,n1,n2, alpha){
  if(alpha == .1) zscore = 1.645
  else if(alpha == .05) zscore = 1.960
  else if (alpha == .01) zscore = 2.576
  
  p1hat = round(x1/n1,3)
  p2hat = round(x2/n2,3)
  
  zstat = (p1hat - p2hat)/(sqrt((p1hat*(1-p1hat)/n1) + (p2hat*(1-p2hat))/n2))
  
  ub = (p1hat - p2hat) + 
    zscore * (sqrt((p1hat*(1-p1hat)/n1) + (p2hat*(1-p2hat))/n2)) + 
    .5*(1/n1 + 1/n2)
  
  lb = (p1hat - p2hat) - 
    zscore * (sqrt((p1hat*(1-p1hat)/n1) + (p2hat*(1-p2hat))/n2)) + 
    .5*(1/n1 + 1/n2)
  pvalue = 1-(pnorm(abs(zstat))*2-1)
  
  cat('Zscore: ',round(zstat,3),'\n')
  cat('P-value:',round(pvalue,3),'\n')
  cat('CI :', round(c(lb,ub),3))
}

TwoSampProp(x1 = 106, x2 = 66,n1 = 299,n2 = 313,alpha = 0.05)