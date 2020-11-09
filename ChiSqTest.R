p = matrix(c(23,291,39,277),
           nrow = 2,
           dimnames = list(c('Low BW','Not Low BW'),
                           c('Instruction','No Instruction')))

q = matrix(c(41,202,330,126,210,614, 452,440,680),
           nrow = 3,
           dimnames = list(c('County 1','County 2', 'County 3'),
                           c('Vaccinated','Not Vaccinated', 'Unknown')))


ChiSqTest = function(x, RetExp = FALSE){
  cs = colSums(x)
  rs = rowSums(x)
  E = NULL
  for (j in cs){
    for (i in rs){
      E = c(E,(i*j)/sum(x))
    }
  }
  
  stat = (x-E)^2/E
  chisqstat = sum(stat)
  df = prod(dim(x)-1)
  
  pvalue = 1-pchisq(chisqstat, df)
  
  cat('ChiSq Stat: ',chisqstat,', Df: ',df,', P-value: ',pvalue,'\n')
  
  if (RetExp){
    cat('Expected values: \n')
    structure((matrix(E, nrow = nrow(x))))
  }
}
ChiSqTest(q)