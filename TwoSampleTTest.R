Ttest = function(data1, data2, alpha){
  x1 = mean(data1)
  x2 = mean(data2)
  n1 = length(data1)
  n2 = length(data2)
  s1 = sd(data1)
  s2 = sd(data2)
  
  cat('Folded \n')
  F_ = ((max(s1,s2)^2)/(min(s1,s2)^2))
  p = 1- abs(pf(F_, n1-1,n2-1))
  critvalue = qf(.95,n1-1,n2-1)
  cat('F-stat:',F_,'\nCritical Value:',critvalue,'\nP-value:',p,'\n')
  
  cat('\n')
  if (p > alpha){
    cat('Pooled t-test: \n')
    cat(s1,'==',s2,'\n')
    s2p = (((n1 - 1)*(s1^2)) + ((n2 - 1)*(s2^2)))/(n1 + n2 -2)
    tstat = (x1 - x2)/sqrt((s2p^2)*((1/n1 + 1/n2)))
    df = n1 + n2 - 2
    pvalue = 2*(abs(pt(tstat,df)))
    
    lb = (x1 - x2) - (qt(1-(alpha*.5), (n1+n2 - 2))) * sqrt((s2p^2)*((1/n1)+(1/n2)))
    ub = (x1 - x2) + (qt(1-(alpha*.5), (n1+n2 - 2))) * sqrt((s2p^2)*((1/n1)+(1/n2)))
    
    CI = c(lb,ub)
    
    cat('P-value:', pvalue,'\n')
    cat('CI:     ', CI, '\n')
    cat('\n')
  }
  
  if (p < alpha){
    cat("Welch's t-test: \n")
    cat(s1,'!=',s2,'\n')
    t = (x1-x2)/(sqrt((s1^2/n1)+(s2^2/n2)))
    df = (((s1^2/n1)+(s2^2/n2))^2 / (((1/(n1-1))*(s1^2/n1)^2) + ((1/(n2-1))*(s2^2/n2)^2)) )
    pvalue = 2*(abs(pt(t,df)))
    
    CI = qt((1-(alpha*.5)),df)
    ub = (x1-x2) + CI*(sqrt((s1^2/n1)+(s2^2/n2)))
    lb = (x1-x2) - CI*(sqrt((s1^2/n1)+(s2^2/n2)))
    
    cat('t =',t, ' df =',df,', p-value =',pvalue, '\n')
    cat('CI: ', lb,'to',ub,'\n')
  }
}
A = c(10.1,10.4 ,9.6,7.7,10.3,12.0,12.0,9.2,10.5,10.4,10.5,9.6,8.8,8.7,9.7,7.0)
B = c(24,10,20,11.4,14,10,22.2,4.5,6.6,17.3,11.5,8.6,3.4,2.2,18.4,14.8,13.8,12.7)
Ttest(data1 = d$Literacy_after, data2 = B, alpha = 0.05)
