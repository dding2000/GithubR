suppressMessages(require(quantreg))

######### 一个简单的分位数回归模型及结果
data(engel) # 加载quantreg包自带的数据集
fit1 = rq(foodexp ~ income, tau = 0.5, data = engel) # 进行分位数回归
fit1 # 直接显示分位数回归的模型和系数
summary(fit1) # 得到更加详细的显示结果
r1 = resid(fit1) # 得到残差序列，并赋值为变量r1
c1 = coef(fit1) # 得到模型的系数，并赋值给变量c1
summary(fit1, se = "nid" )  # 通过设置参数se，可以得到系数的假设检验

#########不同分位点下的系数估计值的比较
fit1 = summary(rq(foodexp ~ income, tau = 2:98/100, data = engel))
fit2 = summary( rq(foodexp ~ income, tau = c(0.05,0.25,0.5,0.75,0.95), data = engel) )
windows(5,5) # 新建一个图形窗口，可以去掉这句
plot(fit1)
windows(5,5) # 新建一个图形窗口，可以去掉这句
plot(fit2)


#########不同分位点拟合曲线的比较
# 散点图
attach(engel) # 打开engel数据集，直接运行其中的列名，就可以调用相应列
plot(income,foodexp,cex=0.25,type="n", # 画图，说明①
xlab="Household Income", ylab="Food Expenditure")
points(income,foodexp,cex=0.5,col="blue") # 添加点，点的大小为0.5
abline( rq(foodexp ~ income, tau=0.5), col="blue" ) # 画中位数回归的拟合直线，颜色蓝
abline( lm(foodexp ~ income), lty = 2, col="red" ) # 画普通最小二乘法拟合直线，颜色红
taus = c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95)
for(i in 1:length(taus)){ # 绘制不同分位点下的拟合直线，颜色为灰色
abline( rq(foodexp ~ income, tau=taus[i]), col="gray" )
}
detach(engel)






