t = matrix(c(13,4,56,65),
           nrow = 2,
           dimnames = list(c('Yes','No'),
                           c('Yes','No')))

OddsRatio = function(x, alpha){
  zscore = round(qnorm(1-(alpha*.5)),3)
  
  odds = (x[1]*x[4])/(x[2]*x[3])
  zstat = (log(odds) - log(1))/sqrt(sum(1/x))
  pvalue = 2*(1-pnorm(abs(zstat)))
  ub = exp(log(odds) + (zscore*sqrt(sum(1/x))))
  lb = exp(log(odds) - (zscore*sqrt(sum(1/x))))
  
  cat('Odds Ratio:',odds, '\n')
  cat('Zstat:',zstat, '\n')
  cat('P-value:',pvalue, '\n')
  cat('CI:',c(lb,ub), '\n')
}
OddsRatio(t, 0.05)