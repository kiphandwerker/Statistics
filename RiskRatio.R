rr = matrix(c(751,623,7755,7479),
            nrow = 2,
            dimnames = list(c('Estrogen +','Estrogen -'),
                            c('Disease +','Disease -')))

RiskRatio = function(x, alpha){
  zscore = round(qnorm(1-(alpha*.5)),3)
  
  p1 = x[1]/rowSums(x)[1]
  p2 = x[2]/rowSums(x)[2]
  RR = p1/p2
  
  ub = exp(log(RR) + zscore*(sqrt(1/x[1] - 1/rowSums(x)[1] +
                                    1/x[2] - 1/rowSums(x)[2])))
  lb = exp(log(RR) - zscore*(sqrt(1/x[1] - 1/rowSums(x)[1] +
                                    1/x[2] - 1/rowSums(x)[2])))
  
  cat('Risk Ratio:',round(RR,2), '\n')
  cat('CI:',round(c(lb,ub),2), '\n')
}

RiskRatio(rr,0.1)