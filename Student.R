setwd('/Users/anakin/Github/RTraining')
data_test = read.csv('student.csv')
pairs(data_test) # plot(data_test)
boxplot(data_test)

Y1=as.numeric(data_test$Y1)
Y2=as.numeric(data_test$Y2)
Y3=as.numeric(data_test$Y3)
Y4=as.numeric(data_test$Y4)
Y5=as.numeric(data_test$Y5)
Y6=as.numeric(data_test$Y6)


summary(data_test)