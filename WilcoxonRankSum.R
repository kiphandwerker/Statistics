wc = data.frame(list('Pre-T' = c(151,174,150,171,144,139,159,140,137,179,146),
                     'Pro-T' = c(147,160,150,170,146,142,150,146,120,149,151)))
WilCoxRankSum = function(x){
  d = x[2] - x[1]
  rmvd = which(d==0)
  d = d[-rmvd,]
  
  df = cbind(d, abs(d), rank(abs(d)))
  rank = df[,1] * df[,3] / abs(df[,1])
  prs = rank[(which(rank > 0))]
  prs = sum(rank[(which(rank > 0))])
  nrs = abs(sum(rank[(which(rank < 0))]))
  
  cat('Removed:',length(rmvd),'\n')
  cat('Positive:',prs,'\n')
  cat('Negative:',nrs,'\n')
}

WilCoxRankSum(wc)