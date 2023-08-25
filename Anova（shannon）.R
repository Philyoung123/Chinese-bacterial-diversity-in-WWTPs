#ANOVA单因素方差分析
library(ggplot2)
library(car)
data <- read.csv("C:/Users/yangkuo/Desktop/中国污水处理厂中丰富及稀有子群落生态/3.1Composition and function of different microbial sub-communities/α多样性系数（用于Anova检验）.csv",row.names = 1)
data1 <- as.data.frame(data[,1])
############正态分布############
#单组作图和分析
ggplot(data1,aes(sample = data1[,1])) +
  geom_qq() + 
  geom_qq_line()

nortest1<-shapiro.test(data1[,1])#p大于0.05则表示符合正态分布
nortest1

###############多组分类做图和分析
qqPlot(lm(plot1~group, data = data), simulate = TRUE, main = 'QQ Plot', labels = FALSE)

shapiro.test <- tapply(data$plot1, data$group, shapiro.test)
shapiro.test

#如果你的数据不满足上述前提假设，
#一是可以考虑转化数据（一般不推荐）
#二是可以考虑使用非参数的检验方法：比如WK检验也就是H检验：Kruskal-Wallis检验（kruskal.test()）、Friedman检验（friedman.test()）等


##单因素分析ANOVA#####
df <- aov(plot1~group, data = data)
summary(df)
#结果表明，3种分组下的数据具有显著差异，p值远低于0.05水平。
#上面结果的差异是在整体水平而言的，并没有告诉我们究竟谁和谁存在差异。Z C X两两间的差异
#如果我们想继续获知两两分组之间的差异，可以进行多重比较：#Tukey HSD 检验

##方差分析后，多重比较，继续探寻两两分组间的差异
vs <- TukeyHSD(df, conf.level = 0.95)
vs

##保存结果
finall <- vs$group
write.csv(finall,"pairs.csv")