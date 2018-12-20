x<-round(runif(1000,1,10))
y<-rep(0,1000)   #产生与x长度相等的y数值型向量
for(i in 1:length(x)){
  if(x[i]==3) y[i]<-0  else y[i]<-1
}
length(y[y==0])

#改写
for(i in 1:length(x)){
  y[x==3]<-0
  y[x!=3]<-1
}