Pst = data.frame('Instructed' = c(1.5,2.0,3.5,3.0,3.5,2.5,2.0,1.5,1.5,2.0,3.0,2.0),
                 'Not Instructed' = c(2.0,2.0,4.0,2.5,4.0,3.0,3.5,3.0,2.5,2.5,2.5,2.5))
PairedST = function(x){
  BD = function(n,x,p){
    ((factorial(n)) / ((factorial(n - x))*factorial(x)))*((p^x)*(1-p)^(n-x))
  }
  d = apply(x,1,diff)
  exc = which(d == 0); le = length(exc)
  neg = which(d > 0);  lneg = length(neg)
  pos = which(d < 0);  lpos = length(pos)
  
  cat(' Excluded:', le, '\n Negative:',lneg, '\n Positive:',lpos,'\n')
  
  s = 0
  for (i in 0:lpos){
    s = BD(lneg+lpos,i,0.5) + s
  }
  
  cat(' P-value:',s*2)
}

PairedST(Pst)