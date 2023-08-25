save.wd="C:/Users/yangkuo/Desktop/中国污水处理厂中丰富及稀有子群落生态/3.2Biogeography principles of different microbial sub-communities/rare"
setwd(save.wd)
####数据预处理
##根据经纬度计算采样点的地理距离
#读取采样点的地理位置数据
site <- read.csv('Sample.coordinates.csv', row.names = 1)
#计算采样点间的地理距离
#geosphere 包 distm() 根据经纬度计算地理距离（默认距离单位，米）
#distm() 要求两列数据，第一列是经度，第二列是纬度
site_dis <- geosphere::distm(site[c('Lon', 'Lat')]) 
rownames(site_dis) <- rownames(site)
colnames(site_dis) <- rownames(site)

#将采样点地理距离矩阵转换为两两对应数值的数据框结构
site_dis <- reshape2::melt(site_dis)
site_dis <- subset(site_dis, value != 0)
head(site_dis)

##计算群落间物种组成相似度，根据原文描述，使用 Bray-curtis 相似度
#群落物种组成数据
spe <- read.csv('rare.csv',row.names = 1)
spe <- data.frame(t(spe))
spe <- spe[rownames(site), ]  #与 site 中预先选择的数据对应

#vegan 包 vegdist() 计算群落间物种组成 Bray-curtis 相异度矩阵
#并通过 1-Bray-curtis 相异度获得 Bray-curtis 相似度
comm_sim <- 1 - as.matrix(vegan::vegdist(spe, method = 'bray'))

#将矩阵转换为两两群落对应数值的数据框结构
diag(comm_sim) <- 0  #去除群落相似度矩阵中的对角线值，它们是样本的自相似度
comm_sim[upper.tri(comm_sim)] <- 0  #群落相似度矩阵是对称的，因此只选择半三角（如下三角）区域的数值即可
comm_sim <- reshape2::melt(comm_sim)
comm_sim <- subset(comm_sim, value != 0)
head(comm_sim)

##采样点距离和群落相似度数据合并
comm_dis <- merge(comm_sim, site_dis, by = c('Var1', 'Var2'))
names(comm_dis) <- c('site1', 'site2', 'comm_sim', 'site_dis')

####输出
write.table(comm_dis, 'dis-bray(rare).txt', sep = '\t', row.names = FALSE, quote = FALSE)
####一元一次回归
##首先是北半球的
#读取上述输出文件，首先以北半球的为例
comm_dis <- read.delim('dis-bray(rare).txt', sep = '\t')

#注：site_dis 列的单位是米（m），在这种大尺度范围下，以千米（km）作为度量会更好一些
comm_dis$site_dis_km <- comm_dis$site_dis/1000

#lm() 拟合群落组成相似度（comm_sim）地理距离（site_dis_km）的一元线性关系
fit <- lm(comm_sim~site_dis_km, data = comm_dis)
summary(fit)  #展示拟合模型的简单统计

#提取重要数值，例如
coefficients(fit)[1]  #获取截距
coefficients(fit)[2]  #获取 site_dis_km 的斜率

#也可以 names(summary(fit)) 后查看主要的内容项，然后从中提取，例如
summary(fit)$adj.r.squared  #校正后 R2
