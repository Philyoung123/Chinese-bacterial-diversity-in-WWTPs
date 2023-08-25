library(vegan)

##读入文件
#OTU 丰度表
otu <- read.delim('regional_local_abundance.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#样本分组文件
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)
##MRPP 分析（使用循环处理，进行小分组间比较，如两组间）
#推荐使用 OTU 丰度表作为输入数据，每次筛选分组后重新计算样本距离，避免由于样本数减少可能导致的距离变动而造成误差
group_name <- unique(group$site)

mrpp_result_two <- NULL
for (i in 1:(length(group_name) - 1)) {
  for (j in (i + 1):length(group_name)) {
    group_ij <- subset(group, site %in% c(group_name[i], group_name[j]))
    otu_ij <- otu[group_ij$names, ]
    mrpp_result_otu_ij <- mrpp(otu_ij, group_ij$site, permutations = 999, distance = 'bray')     #Bray-Curtis 距离测度，基于 999 次置换
    mrpp_result_two <- rbind(mrpp_result_two, c(paste(group_name[i], group_name[j], sep = '/'), 'Bray-Curtis', mrpp_result_otu_ij$A, mrpp_result_otu_ij$delta, mrpp_result_otu_ij$E.delta, mrpp_result_otu_ij$Pvalue))
  }
}
mrpp_result_two <- data.frame(mrpp_result_two, stringsAsFactors = FALSE)
names(mrpp_result_two) <- c('group', 'distance', 'A', 'Observe_delta', 'Expect_delta', 'P_value')

#可选添加 p 值校正过程，例如 Benjamini 校正
mrpp_result_two$P_value <- as.numeric(mrpp_result_two$P_value)
mrpp_result_two$P_adj_BH <- p.adjust(mrpp_result_two$P_value, method = 'BH')

#输出
write.table(mrpp_result_two, 'MRPP.result_two.txt', row.names = FALSE, sep = '\t', quote = FALSE, na = '')
