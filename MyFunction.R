######### my function #########
DEA<-function(x){
  par(mfrow=c(2,2))
  hist(x)
  dotchart(x)
  boxplot(x, horizontal=T)
  qqnorm(x);qqline(x)
  par(mfrow=c(1,1))
}

####时间序列检验
DWBUnitRoot <- function(x, n=1, symbol="AIC"){
	require(vars)
    unitroot1=ur.df(x, type="trend", lags=n, selectlags=symbol)
	unitroot2=ur.df(x, type="drift", lags=n, selectlags=symbol)
	unitroot3=ur.df(x, type="none", lags=n, selectlags=symbol)

	require(fUnitRoots)
	unitroot4=unitrootTest(x, lags=n, type="ct")
	unitroot5=unitrootTest(x, lags=n, type="c")
	unitroot6=unitrootTest(x, lags=n, type="nc")

	unitroot=list(
		ur.df_TrendConstant=summary(unitroot1),
		ur.df_Constant=summary(unitroot2),
		ur.df_none=summary(unitroot3),
		a1=c("注****************ur.df结果输出结束***********************注"),
		adfTest_ct=unitroot4,
		adfTest_c=unitroot5 ,
		adfTset_nc=unitroot6,
		a1=c("注**************adfTest结果输出结束**********************注")
		)

	return(unitroot)
}

####线性回归检验, 出图
LinearModelTest <- function(lm.fit) {
	par(mfrow=c(2,2))
	plot(lm.fit)
	par(mfrow=c(1,1))

	##绘制学生化残差图的函数，出图
	residplot <- function(fit, nbreaks=10){
    z <- rstudent(fit)
    hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
   rug(jitter(z), col="brown")
   curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
   lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
   legend("topright",
         legend=c("Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue", "red"), cex=.7)
}
    residplot(lm.fit)

	#误差的独立性检验
	require(car)
    IndependenceOfErrorTest=durbinWatsonTest(lm.fit) #误差的独立性检验
	OutlierTest=outlierTest(lm.fit) # 离群

	#线性检验, 出图
	crPlots(lm.fit, main="线性检验")

	#非参数方差齐性检验
	n=length(lm.fit$residuals)
	m=ceiling(n/2)
	o=order(lm.fit$model[,1])
	yy1=lm.fit$residuals[o[1:m]]
	yy2=lm.fit$residuals[o[m+1:n]]
	VarianceHomogenityTest=var.test(yy1,yy2)

	ncvTest=ncvTest(lm.fit)
	spreadLevelPlot(lm.fit)

	##线性模型假设的综合验证
	require(gvlma)
	LinearModelAssumptionsTest=summary(gvlma(lm.fit))


	result=list(
		IndependenceOfErrorTest=IndependenceOfErrorTest,
		OutlierTest=OutlierTest,
		VarianceHomogenityTest=VarianceHomogenityTest,
		ncvTest=ncvTest,
        LinearModelAssumptionsTest=LinearModelAssumptionsTest
		)

   return(result)
}

#mydat:The original data
#startyear:The start year
#endyear:The End year
#vectorname:The vector of variable name
convertPaneldat <- function(mydat, startyear, endyear, vectorname) {
n = nrow(mydat)
xnum = length(vectorname)
yearinterval = endyear-startyear+1
id = as.vector(rep(mydat[,1], yearinterval))
year = as.vector(rep(c(startyear: endyear), each = n))
mat = matrix(nrow=n*yearinterval, ncol=xnum, dimnames=list(c(),vectorname))
X = as.data.frame(mat)
for (i in 1:xnum) {
    X[,i]=as.vector(as.matrix(mydat[,(2+(i-1)*yearinterval):(1+i*yearinterval)]))
    }
pdat = data.frame(id, year, X)
}
