st = data.frame('Girl' = seq(1,10),
                'Score'= c(4,5,8,8,9,6,10,7,6,6))

SignTest = function(x, m){
  BD = function(n,x,p){
    ((factorial(n)) / ((factorial(n - x))*factorial(x)))*((p^x)*(1-p)^(n-x))
  }
  exc = which(x[2] == m); le = length(exc)
  neg = which(x[2] < m);  lneg = length(neg)
  pos = which(x[2] > m);  lpos = length(pos)
  
  cat(' Excluded:', le, '\n Negative:',lneg, '\n Positive:',lpos,'\n')
  
  s= 0
  for (i in 0:length(neg)){
    s = BD(lneg+lpos,i,0.5) + s
  }
  
  cat(' P-value:',s*2)
}

SignTest(st, 5)