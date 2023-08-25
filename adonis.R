library(vegan)

##读入文件
#OTU 丰度表
otu <- read.delim('regional_local_abundance.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#样本分组文件
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)
##PERMANOVA 分析（使用循环处理，进行小分组间比较，如两组间）
#推荐使用 OTU 丰度表作为输入数据，每次筛选分组后重新计算样本距离，避免由于样本数减少可能导致的距离变动而造成误差
group_name <- unique(group$site)

adonis_result_two <- NULL
for (i in 1:(length(group_name) - 1)) {
  for (j in (i + 1):length(group_name)) {
    group_ij <- subset(group, site %in% c(group_name[i], group_name[j]))
    otu_ij <- otu[group_ij$names, ]
    adonis_result_otu_ij <- adonis(otu_ij~site, group_ij, permutations = 999, distance = 'bray')     #Bray-Curtis 距离测度，基于 999 次置换
    adonis_result_two <- rbind(adonis_result_two, c(paste(group_name[i], group_name[j], sep = '/'), 'Bray-Curtis', unlist(data.frame(adonis_result_otu_ij$aov.tab, check.names = FALSE)[1, ])))
  }
}
adonis_result_two <- data.frame(adonis_result_two, stringsAsFactors = FALSE)
names(adonis_result_two) <- c('group', 'distance', 'Df', 'Sums of squares', 'Mean squares', 'F.Model', 'Variation (R2)', 'Pr (>F)')

#可选添加 p 值校正，例如 Benjamini 校正
adonis_result_two$'Pr (>F)' <- as.numeric(adonis_result_two$'Pr (>F)')
adonis_result_two$P_adj_BH <- p.adjust(adonis_result_two$'Pr (>F)', method = 'BH')

#输出
write.table(adonis_result_two, 'PERMANOVA.result_two.txt', row.names = FALSE, sep = '\t', quote = FALSE, na = '')

