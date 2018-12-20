library("car")                         #支持方差膨胀因子计算
library("readr")                       #支持读入数据
library("corrplot") 

par(family='STKaiti')

hotel <- read.csv("hoteldata.csv")
names(hotel) <- c("hotel_name",        #--酒店名称
                  "area",              #--酒店地区
                  "site",              #--酒店地址
                  "hyg_grade",         #--卫生评分
                  "ser_grade",         #--服务评分
                  "fac_grade",         #--设施评分
                  "loc_grade",         #--位置评分
                  "evaluate",          #--评价数
                  "time",              #--装修时间
                  "type",              #--房间类型
                  "price",             #--酒店房价
                  "lon",               #--经度
                  "lat",               #--纬度
                  "company",           #--公司
                  "trip",              #--出行住宿
                  "school")            #--校园生活

hotel$total_grade <- (hotel$hyg_grade+hotel$ser_grade+hotel$fac_grade+hotel$loc_grade)/4
#对装修时间进行处理
hotel$timeCut <- cut(hotel$time, c(0,2015.5,Inf),c("旧装修","新装修"))   #分组函数
#把其他地区作为基准组
hotel$area <- factor(as.character(hotel$area),levels = c("其他城区","东城区","朝阳区","海淀区"))

par(mfrow = c(1,1))                                   
hist(x    = hotel$price,
     xlab = "酒店房价分布直方图",                          #x轴标签
     ylab = "频数",                                     #y轴标签
     ylim = c(0,300),                                  #y轴范围
     col  = "lightsteelblue4",                         #颜色为lightsteelblue4
     main = "")            

#(2)因变量数字特征
median(hotel$price)                                    #酒店房价的中位数
mean(hotel$price)                                      #酒店房价的平均数
#1.2自变量
#(1)酒店因素箱线图
par(mfrow = c(1,2))                                    #1x2画布
hotel$type= factor(hotel$type,levels = c("标准间","商务间","豪华套间"))
boxplot(log(hotel$price)~(hotel$type),                 #画箱线图
        col     = "lightsteelblue4",                   #颜色为lightsteelblue4
        ylab    = "酒店房价(取对数)",                     #y轴标签
        xlab    = "酒店类型",                            #x轴标签
        names   = c("标准","商务","豪华套间"))            #分组命名
timeCut <- cut(hotel$time, c(0,2015.5,Inf))
boxplot(log(hotel$price) ~ timeCut,                    #画箱线图
        col     = c("lightsteelblue4"),                #颜色为lightsteelblue4
        ylab    = "酒店房价(取对数)",                     #y轴标签
        xlab    = "装修时间",                           #x轴标签
        names   = c("旧装修","新装修"))                  #分组命名

#(2)评分因素相关系数
par(mfrow = c(1,1))                                    #1x1画布
grade <- data.frame(hotel$hyg_grade,hotel$ser_grade,hotel$fac_grade,hotel$loc_grade)
names(grade) <- c("卫生","服务","设施","位置")
cor_grade <- cor(grade)                                #计算相关系数矩阵
corrplot(cor_grade,                                    #相关系数矩阵可视化
         col         = NULL,
         type        = "full",                         #图形为全部填充
         order       = "AOE",                          #排序按照特征向量角序(AOE)方式
         method      = "color",                        #颜色填充方式
         tl.col      = "lightsteelblue4",              #字体颜色为lightsteelblue4
         tl.cex      = 1,                              #字体大小为1
         cl.pos      = "n",                            #图例(位置)，不需要图例
         addCoef.col = "grey")                         #系数颜色为grey

#(3)评分因素箱线图
par(mfrow = c(1,2))                                    #1X2画布
total_gradeCut <- cut(hotel$total_grade, c(0,4.5,Inf)) #分组函数
boxplot(log(hotel$price) ~ total_gradeCut,             #画箱线图
        col     = c("lightsteelblue4"),                #颜色为lightsteelblue4
        ylab    = "酒店房价(取对数)",                     #y轴标签
        xlab    = "位置评分",                            #x轴标签
        names   = c("低评分","高评分"))                   #分组(4.5分以上为高评分，4.5分一下为低评分)
evaluateCut <- cut(hotel$evaluate, c(0,median(hotel$evaluate),Inf))
boxplot(log(hotel$price) ~ evaluateCut,                #画箱线图
        col     = c("lightsteelblue4"),                #颜色为lightsteelblue4
        ylab    = "酒店房价(取对数)",                     #y轴标签
        xlab    = "评价数",                             #x轴标签
        names   = c("低评价数","高评价数"))               #分组(4.5分以上为高评分，4.5分一下为低评分)
#(4)评分因素数字特征
mean(hotel$hyg_grade)                                  #计算卫生评分均值
mean(hotel$ser_grade)                                  #计算服务评分均值
mean(hotel$fac_grade)                                  #计算设施评分均值
mean(hotel$loc_grade)                                  #计算位置评分均值
#3.对数线性回归模型
#(1)对数线性回归模型建立
lm_price <- lm(log(price) ~ area + type + timeCut + evaluate + total_grade + school + company + trip ,data = hotel)
lm_price <- step(lm_price)                             #AIC方法进行变量选择
summary(lm_price)                                      #输出模型结果
#(2)对数回归模型检验
par(mfrow = c(2,2))                                    #2X2画布
plot(lm_price,which = c(1,2,3,4))                      #模型诊断，包括残差图、QQ图、cook距离
round(vif(lm_price),2)                                 #计算对数回归模型的方差膨胀因子