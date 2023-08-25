library(vegan)

##读入文件
#OTU 丰度表
otu <- read.delim('regional_local_abundance.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#样本分组文件
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)
##ANOSIM 分析（使用循环处理，进行小分组间比较，如两组间）
#推荐使用 OTU 丰度表作为输入数据，每次筛选分组后重新计算样本距离，避免由于样本数减少可能导致的距离变动而造成误差
group_name <- unique(group$site)

dir.create('anosim_two', recursive = TRUE)
anosim_result_two <- NULL
for (i in 1:(length(group_name) - 1)) {
  for (j in (i + 1):length(group_name)) {
    group_ij <- subset(group, site %in% c(group_name[i], group_name[j]))
    otu_ij <- otu[group_ij$names, ]
    anosim_result_otu_ij <- anosim(otu_ij, group_ij$site, permutations = 999, distance = 'bray')     #Bray-Curtis 距离测度，基于 999 次置换
    anosim_result_two <- rbind(anosim_result_two, c(paste(group_name[i], group_name[j], sep = '/'), 'Bray-Curtis', anosim_result_otu_ij$statistic, anosim_result_otu_ij$signif))
    
    #每次循环输出图片
    #pdf(paste('anosim_two/anosim.', group_name[i], '_', group_name[j], '.pdf', sep = ''), width = 7, height = 5)
    png(paste('anosim_two/anosim.', group_name[i], '_', group_name[j], '.png', sep = ''), width = 600, height = 400)
    plot(anosim_result_otu_ij, col = c('gray', 'red', 'blue'))
    dev.off()
  }
}

#带 R 值和 p 值的表格
anosim_result_two <- data.frame(anosim_result_two, stringsAsFactors = FALSE)
names(anosim_result_two) <- c('group', 'distance', 'R', 'P_value')

#可选添加 p 值校正过程，例如 Benjamini 校正
anosim_result_two$P_value <- as.numeric(anosim_result_two$P_value)
anosim_result_two$P_adj_BH <- p.adjust(anosim_result_two$P_value, method = 'BH')

write.table(anosim_result_two, 'anosim_two/ANOSIM.result_two.txt', row.names = FALSE, sep = '\t', quote = FALSE, na = '')
