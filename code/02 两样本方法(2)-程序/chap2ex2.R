siegel.tukey=function(x,y,id.col=TRUE,adjust.median=F,rnd=-1,alternative="two.sided",mu=0,paired=FALSE,
                      exact=FALSE,correct=TRUE,conf.int=FALSE,conf.level=0.95)
{
  if(id.col==FALSE){
    data=data.frame(c(x,y),rep(c(0,1),c(length(x),length(y))))
  } else {
    data=data.frame(x,y)
  }
  names(data)=c("x","y")
  data=data[order(data$x),]
  if(rnd>-1){data$x=round(data$x,rnd)}
  
  if(adjust.median==T){
    cat("\n","Adjusting medians...","\n",sep="")
    data$x[data$y==0]=data$x[data$y==0]-(median(data$x[data$y==0]))
    data$x[data$y==1]=data$x[data$y==1]-(median(data$x[data$y==1]))
  }
  cat("\n","Median of group 1 = ",median(data$x[data$y==0]),"\n",sep="")
  cat("Median of group 2 = ",median(data$x[data$y==1]),"\n","\n",sep="")
  # cat("Testing median differences...","\n")
  # print(wilcox.test(data$x[data$y==0],data$x[data$y==1]))
  
  
  cat("Performing Siegel-Tukey rank transformation...","\n","\n")
  
  sort.x<-sort(data$x)
  sort.id<-data$y[order(data$x)]
  
  data.matrix<-data.frame(sort.x,sort.id)
  
  base1<-c(1,4)
  iterator1<-matrix(seq(from=1,to=length(x),by=4))-1
  rank1<-apply(iterator1,1,function(x) x+base1)
  
  iterator2<-matrix(seq(from=2,to=length(x),by=4))
  base2<-c(0,1)
  rank2<-apply(iterator2,1,function(x) x+base2)
  
  #print(sum(rank1))
  #print(sum(rank2))
  
  if(length(rank1)==length(rank2)){
    rank<-c(rank1[1:floor(length(x)/2)],rev(rank2[1:ceiling(length(x)/2)]))
  } else{
    rank<-c(rank1[1:ceiling(length(x)/2)],rev(rank2[1:floor(length(x)/2)]))
  }
  
  
  unique.ranks<-tapply(rank,sort.x,mean)
  unique.x<-as.numeric(as.character(names(unique.ranks)))
  
  rank.matrix<-data.frame(unique.x,unique.ranks)
  
  ST.matrix<-merge(data.matrix,rank.matrix,by.x="sort.x",by.y="unique.x")
  
  print(ST.matrix)
  
  cat("\n","Performing Siegel-Tukey test...","\n",sep="")
  
  ranks0<-ST.matrix$unique.ranks[ST.matrix$sort.id==0]
  ranks1<-ST.matrix$unique.ranks[ST.matrix$sort.id==1]
  
  cat("\n","Mean rank of group 0: ",mean(ranks0),"\n",sep="")
  cat("Mean rank of group 1: ",mean(ranks1),"\n",sep="")
  cat("\n","Sum rank of group 0: ",sum(ranks0),"\n",sep="")
  cat("Sum rank of group 1: ",sum(ranks1),"\n",sep="")
  print(wilcox.test(ranks0,ranks1,alternative=alternative,mu=mu,paired=paired,
                    exact=exact,correct=correct,conf.int=conf.int,conf.level=conf.level))
}
x<-c(21.9,20.2,19.4,20.3,19.6,20.4,18.4,20.1,22.0,18.9)
y<-c(20.2,13.8,21.8,19.2,19.6,25.5,17.0,17.6,19.5,22.2)
n1=length(x); n2=length(y)
data <- c(x, y)
index <- c(rep(0,n1),rep(1,n2))
siegel.tukey(data,index, alternative='greater')
##b
alpha=0.05; n1=length(x); n2=length(y); n=n1+n2
dev_x=abs(x-median(x)); dev_y=abs(y-median(y))
data <- c(dev_x, dev_y); index <- c(rep(1,n1),rep(2,n2))
permu = combn(n, n1); N = dim(permu)[2]; z = rep(1, N)
for(i in 1:N)
{ ind = permu[,i]
z[i] = mean(data[ind]) /mean(data[-ind])
}
RMD_obs = mean(dev_x)/mean(dev_y)
cat('\n RMD_obs=', RMD_obs, '\n')
p.value <- length(z[z>=RMD_obs])/N
cat("p.value=",p.value,sep="")
prob=c(90, 95, 97.5)/100
quan=quantile(z, prob)
cat('\n Quantiles at 90%, 95% and 97.5% are ', quan, '\n')
##3.18
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
x1<-c(11,33,48,34,112,369,64,44)
y1<-c(177,80,141,332)
wilcox.test(y1,x1,alternative = 'greater')
alpha=0.05; n1=length(x1); n2=length(y1); n=n1+n2
data  <- c(x1, y1)
index <- c(rep(1,n1),rep(2,n2)) 
#score= qnorm(rank(data)/(n+1))     ### Van der Waerden 得分
score = expsc(data)                  ### 指数得分
Dobs  = sum(score[index==1]) 
permu = combn(n, n1)
N = dim(permu)[2]
z = rep(1, N)
for(i in 1:N)
{
  ind  = permu[,i]
  z[i] =  sum(score[ind])  
}
p.value <- length(z[z>=Dobs])/N
print(p.value)
t.test(x1, y1, var.equal=TRUE, conf.level=0.95)

x2<-c(5,11,16,8,12)
y2<-c(17,14,15,21,19,13)
t.test(x2, y2, var.equal=FALSE, conf.level=0.95)
alpha=0.05
m=length(x2)
n=length(y2)
diff=rep(1, n*m)
for (i in 1: m)
{
  for(j in 1: n)
  { 
    k = (i-1)*n+j    
    diff[k] = x[i]-y[j]
  }
}

diff=sort(diff)
mu=n*m/2                    ### 到后面第2.10节讲解
va=mu*(n+m+1)/6             ###  
z=qnorm(1-alpha/2)
ka =  mu - z*sqrt(va) 
kb =  mu + z*sqrt(va) + 1 

ka=round(ka)    ## 四舍五入
kb=round(kb)
print(c(diff[ka], diff[kb]))  
print(median(diff))
