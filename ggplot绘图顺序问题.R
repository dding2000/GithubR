##该方法用于绘制饼图，但是由于不知道ggplot2是怎么控制
##绘图顺序的——给出的图形和数据读入顺序并不完全一致。
##所以，标签和图还不能完全匹配，具体如图。

##R语言中文论坛，小王－通信运营管理，QQ：358284931
#数据
val <- c(150,50,300)
#计算百分比
percent <- round(values/sum(values) * 100,2)
#对应标签
labels <- c("C","B","E")
#每个数据折算为百分比后的数值（已经带上%号）
percent_str <- paste(percent, "%", sep="")
#建立名为values的数据框
values <- data.frame(percent, Type = labels, percent_str )

#绘图
library(ggplot2)

#(画图的顺序有问题)
pie <- ggplot(values, aes(x = "" ,y = percent, fill = Type)) +  geom_bar(width = 3) + opts(axis.ticks=theme_blank())

#添加每个百分
pie <- pie + geom_text(aes(x = rep(1,3), y = percent/2 + c(0, cumsum(percent)[-length(percent)])), label = percent_str, size=3)
#图形扭曲为圆形
pie = pie + coord_polar("y")
#清空坐标名优化，并给图注填上题目为“Types”。
pie = pie + xlab('') + ylab('') + labs(fill = "Types")
