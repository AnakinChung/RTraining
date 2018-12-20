#安装所需要的包
library("car")                         #支持方差膨胀因子计算
library("readr")                       #支持读入数据
library("corrplot")                    #支持相关系数矩阵可视化
#0.数据读取与清洗
#设置工作路径
setwd("C:/RWork")
###数据清洗部分###
hotel <- read_csv("hoteldata.csv")
##数据变量命名
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
#酒店因素评分
#为了避免多重共线性
hotel$total_grade <- (hotel$hyg_grade+hotel$ser_grade+hotel$fac_grade+hotel$loc_grade)/4
#对装修时间进行处理
hotel$timeCut <- cut(hotel$time, c(0,2015.5,Inf),c("旧装修","新装修"))   #分组函数
#把其他地区作为基准组
hotel$area <- factor(as.character(hotel$area),levels = c("其他城区","东城区","朝阳区","海淀区"))
#1.描述统计
#1.1因变量
#(1)因变量直方图
par(mfrow = c(1,1))                                    #1x1画布
#因变量分布直方图
hist(x    = hotel$price,
     xlab = "酒店房价分布直方图",                          #x轴标签
     ylab = "频数",                                     #y轴标签
     ylim = c(0,300),                                  #y轴范围
     col  = "lightsteelblue4",                         #颜色为lightsteelblue4
     main = "")                                        #标题为空


#(2)因变量数字特征
median(hotel$price)                                    #酒店房价的中位数
## [1] 1389.5
mean(hotel$price)                                      #酒店房价的平均数
## [1] 1655.513
#1.2自变量
#(1)酒店因素箱线图
par(mfrow = c(1,2))                                    #1x2画布
##房间类型对酒店房价的影响
#房间类型分组(标准间、商务间、豪华套间)
hotel$type= factor(hotel$type,levels = c("标准间","商务间","豪华套间"))
boxplot(log(hotel$price)~(hotel$type),                 #画箱线图
        col     = "lightsteelblue4",                   #颜色为lightsteelblue4
        ylab    = "酒店房价(取对数)",                     #y轴标签
        xlab    = "酒店类型",                            #x轴标签
        names   = c("标准","商务","豪华套间"))            #分组命名

##装修时间分组对酒店房价的影响
#装修时间分组(2016年、2017年为新装修，2015年及以前为旧装修)
timeCut <- cut(hotel$time, c(0,2015.5,Inf))
boxplot(log(hotel$price) ~ timeCut,                    #画箱线图
        col     = c("lightsteelblue4"),                #颜色为lightsteelblue4
        ylab    = "酒店房价(取对数)",                     #y轴标签
        xlab    = "装修时间",                           #x轴标签
        names   = c("旧装修","新装修"))                  #分组命名


#(2)评分因素相关系数
par(mfrow = c(1,1))                                    #1x1画布
#构造一个数据框
grade <- data.frame(hotel$hyg_grade,hotel$ser_grade,hotel$fac_grade,hotel$loc_grade)
#命名数据
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
#酒店综合品质分组对酒店房价的影响
#酒店综合品质的分组(以4.5分组)
total_gradeCut <- cut(hotel$total_grade, c(0,4.5,Inf)) #分组函数
boxplot(log(hotel$price) ~ total_gradeCut,             #画箱线图
        col     = c("lightsteelblue4"),                #颜色为lightsteelblue4
        ylab    = "酒店房价(取对数)",                     #y轴标签
        xlab    = "位置评分",                            #x轴标签
        names   = c("低评分","高评分"))                   #分组(4.5分以上为高评分，4.5分一下为低评分)

#评分数分组对酒店房价的影响
#评价数的分组(以中位数分组)
evaluateCut <- cut(hotel$evaluate, c(0,median(hotel$evaluate),Inf))
boxplot(log(hotel$price) ~ evaluateCut,                #画箱线图
        col     = c("lightsteelblue4"),                #颜色为lightsteelblue4
        ylab    = "酒店房价(取对数)",                     #y轴标签
        xlab    = "评价数",                             #x轴标签
        names   = c("低评价数","高评价数"))               #分组(4.5分以上为高评分，4.5分一下为低评分)


#(4)评分因素数字特征
mean(hotel$hyg_grade)                                  #计算卫生评分均值
## [1] 4.611691
mean(hotel$ser_grade)                                  #计算服务评分均值
## [1] 4.530036
mean(hotel$fac_grade)                                  #计算设施评分均值
## [1] 4.459173
mean(hotel$loc_grade)                                  #计算位置评分均值
## [1] 4.366187
#3.对数线性回归模型
#(1)对数线性回归模型建立
###对数线性回归模型
lm_price <- lm(log(price) ~ area + type + timeCut + evaluate + total_grade + school + company + trip ,data = hotel)
lm_price <- step(lm_price)                             #AIC方法进行变量选择
## Start:  AIC=-1104.13
## log(price) ~ area + type + timeCut + evaluate + total_grade +
##     school + company + trip
##
##               Df Sum of Sq     RSS      AIC
## - evaluate     1     0.011  73.106 -1106.05
## <none>                      73.095 -1104.13
## - company      1     0.324  73.419 -1103.67
## - area         3     1.540  74.634 -1098.54
## - trip         1     1.021  74.116 -1098.42
## - timeCut      1     1.696  74.791 -1093.38
## - school       1     1.867  74.962 -1092.11
## - total_grade  1    34.558 107.653  -890.87
## - type         2    51.412 124.507  -812.00
##
## Step:  AIC=-1106.05
## log(price) ~ area + type + timeCut + total_grade + school + company +
##     trip
##
##               Df Sum of Sq     RSS      AIC
## <none>                      73.106 -1106.05
## - company      1     0.314  73.420 -1105.66
## - area         3     1.529  74.635 -1100.54
## - trip         1     1.023  74.129 -1100.32
## - school       1     1.868  74.974 -1094.01
## - timeCut      1     1.952  75.058 -1093.39
## - total_grade  1    34.682 107.788  -892.17
## - type         2    51.421 124.527  -813.91
summary(lm_price)                                      #输出模型结果
##
## Call:
## lm(formula = log(price) ~ area + type + timeCut + total_grade +
##     school + company + trip, data = hotel)
##
## Residuals:
##      Min       1Q   Median       3Q      Max
## -0.93072 -0.24026 -0.00647  0.20420  1.39379
##
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)
## (Intercept)    0.2812327  0.3967241   0.709 0.478697
## area东城区     0.1427223  0.0613598   2.326 0.020385 *
## area朝阳区     0.1047659  0.0456232   2.296 0.022035 *
## area海淀区     0.1701495  0.0593397   2.867 0.004299 **
## type商务间     0.3002518  0.0377415   7.955 1.04e-14 ***
## type豪华套间   0.7455336  0.0382657  19.483  < 2e-16 ***
## timeCut新装修  0.2018405  0.0529088   3.815 0.000152 ***
## total_grade    1.4451856  0.0898766  16.080  < 2e-16 ***
## school        -0.0030720  0.0008231  -3.732 0.000210 ***
## company       -0.0017479  0.0011419  -1.531 0.126431
## trip           0.0014040  0.0005083   2.762 0.005935 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.3663 on 545 degrees of freedom
## Multiple R-squared:  0.5941, Adjusted R-squared:  0.5866
## F-statistic: 79.76 on 10 and 545 DF,  p-value: < 2.2e-16
#(2)对数回归模型检验
par(mfrow = c(2,2))                                    #2X2画布
plot(lm_price,which = c(1,2,3,4))                      #模型诊断，包括残差图、QQ图、cook距离


round(vif(lm_price),2)                                 #计算对数回归模型的方差膨胀因子
##             GVIF Df GVIF^(1/(2*Df))
## area        3.52  3            1.23
## type        1.00  2            1.00
## timeCut     1.02  1            1.01
## total_grade 1.15  1            1.07
## school      1.92  1            1.39
## company     4.21  1            2.05
## trip        6.13  1            2.47

