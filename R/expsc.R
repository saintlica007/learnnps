### 指数得分
expsc<-function(x)
{
  y=rank(x)
  n=length(x)
  sc=rep(0, n)
  for(i in 1:n)
  {
    for(j in 1:y[i])
    { sc[i] =  sc[i]+1/(n+1-j) }
  }
  sc
}
