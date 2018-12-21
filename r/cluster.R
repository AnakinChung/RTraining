mydata<-read.csv("data.csv",fileEncoding = "GBK")
head(mydata)

par(family='STKaiti')

n=dim(mydata)[1] 

col_ppt=rgb(red=127,green=127,blue=127,max=255)     #灰色
plot(mydata$行驶时长,mydata$行驶里程,col=col_ppt,xlab="行驶时长（小时）",ylab="行驶里程（千米）")
plot(mydata$转速平均值, mydata$平均时速,col=col_ppt,xlab="平均时速（千米/小时)",ylab="平均引擎转速（转/分钟）")

a1<-length(which(mydata$早晚高峰>0))/n   #早晚高峰路程占比
a2<-length(which(mydata$深夜出行>0))/n   #深夜出行路程占比
a3<-length(which(mydata$疲劳驾驶>0))/n   #疲劳驾驶路程占比
a<-cbind(a1,a2,a3)                       #获得出行习惯数据
r=barplot(a,beside=T,col=col_ppt,names=c("早晚高峰","深夜出行","疲劳驾驶"),ylim=c(0,0.5),xlab="出行习惯",ylab="比例")
mylab=paste(round(100*a,1),"%",sep="");
text(r,a+0.05,mylab,pos=1)

hist(mydata$平均时速,xlab="平均时速（千米/小时)",ylab="频数",main="",col=col_ppt)

par(mfrow=c(1,2))   #画1*2的图

boxplot(log(mydata$行驶里程)~mydata$车号,col=col_ppt,xlab="车号",ylab="对数行驶里程")
boxplot(log(mydata$行驶时长)~mydata$车号,col=col_ppt,xlab="车号",ylab="对数行驶时长")

boxplot(mydata$转速平均值~mydata$车号,col=col_ppt,xlab="车号",ylab="转速平均值（转/分钟）")
boxplot(mydata$平稳性~mydata$车号,col=col_ppt,xlab="车号",ylab="驾驶平稳性")

mydata0<-mydata[,-15]

par(mfrow=c(1,1))
pca.fit=princomp(mydata0)                      #使用princomp函数（使用特征分解办法求解主成分）
screeplot(pca.fit,type="lines")
fac.out=factpc(mydata0,3,rotation="varimax")
fac.out$Vars 
cbind(round(fac.out$loadings,3),round(fac.out$common,3))

fac.score=fac.out$scores              #获得每段路程的因子得分，每行是一个行程，每列是该行程在相应因子上的得分
colnames(fac.score)<-c("行驶速度","行驶强度","出行习惯")
set.seed(2)                           #固定随机种子
km.out<-kmeans(fac.score,7,nstart=20) #按7类进行聚类
km.out$centers  
km.out$size
km.out$cluster
mydata[which(km.out$cluster==4),][1,]
mydata[which(km.out$cluster==4),][2,]
mydata[which(km.out$cluster==4),][3,]
temp=table(mydata$车号,km.out$cluster)

temp.ratio<-apply(temp,1,function(x) x/sum(x))   #转化成比例数据，使结果更直观
par(xpd=T,mar=par()$mar+c(0,0,0,2))              #xpd=T允许图例出现在图形边缘空白处，c(0,0,0,2)将右侧边缘空白位置增大
barplot(temp.ratio,horiz=T,names=c(1:8),xlab="比例",ylab="车号",border=NA,
        col=c("#1C86EE","#FF7F00","#B5B5B5","#FFA500","gray61","#4169E1","gold2"),
        legend.text=paste("类",c(1:7),sep=""),args.legend=list(x="right",bty="n",inset=-0.15))