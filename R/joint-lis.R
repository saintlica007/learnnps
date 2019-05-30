perm0<-function(row,col)
{
  n=sum(col)
  r=length(row)
  c=length(col)
  x=NULL
  for(i in 1:c)
  { x=c(x, rep(i,col[i]))
  }
  y=sample(x)

  freq=matrix(rep(0, r*c), r,c)
  count=0
  for(j in 1:r)
  {
    tmpy = y[(count+1):(count+row[j])]
    for(i in 1:c)
    { freq[j,i] = sum(tmpy==i) }
    count=count+row[j]
  }
  freq
}

perm1<-function(row,col)
{
  n=sum(col)
  r=length(row)
  c=length(col)
  x=NULL
  for(i in 1:r)
  { x=c(x, rep(i,row[i]))
  }
  y=sample(x)

  freq=matrix(rep(0, r*c), r,c)
  count=0
  for(j in 1:c)
  {
    tmpy = y[(count+1):(count+col[j])]
    for(i in 1:r)
    { freq[i,j] = sum(tmpy==i) }
    count=count+col[j]
  }
  freq
}


chi2<-function(freq)
{
  row=apply(freq,1,sum)
  col=apply(freq,2,sum)
  expect= matrix(row,,1)%*%matrix(col, 1,)/sum(freq)
  sum((freq-expect)^2/expect)
}
perm<-function(row,col)
{
  n=sum(col)
  r=length(row)
  c=length(col)
  x=NULL
  for(i in 1:c)
  { x=c(x, rep(i,col[i]))
  }
  y=sample(x)

  freq=matrix(rep(0, r*c), r,c)
  count=0
  for(j in 1:r)
  {
    tmpy = y[(count+1):(count+row[j])]
    for(i in 1:c)
    { freq[j,i] = sum(tmpy==i) }
    count=count+row[j]
  }
  freq
}

zstat<-function(x, i, j)
{
  ni=sum(x[i,])
  nj=sum(x[j,])
  z=x[i,]/ni - x[j,]/nj

  pbar=(x[i,]+x[j,])/(row[i]+row[j])
  v=pbar*(1-pbar)*(1/ni+1/nj)
  z/sqrt(v)
}

qstat<-function(x, i, j)
{
  tmp=zstat(x,i,j)
  max(abs(tmp))
}
##########   把频数 转化为量表数据   ##################
transfer<-function(x)
{
  k=length(x)
  y=NULL
  for(i in 1:k)
  { y=c(y, rep(i,x[i]))
  }
  y
}
###  对于全部数据，x， 计算JT统计量
jt<-function(x, group)
{ ug=unique(group)
k=length(ug)
jt=0
for(j in 1:(k-1))
{ for(i in (j+1):k)
{ xa=  x[group==ug[i]]
xb=  x[group==ug[j]]
tmp = mw(xa,xb)
jt=jt+ tmp
#      print(c(i,j,tmp))
}
}
jt
}
