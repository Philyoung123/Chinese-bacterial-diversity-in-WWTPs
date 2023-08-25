library(vegan)

##�����ļ�
#OTU ��ȱ�
otu <- read.delim('regional_local_abundance.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#���������ļ�
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)
##PERMANOVA ������ʹ��ѭ������������С�����Ƚϣ�������䣩
#�Ƽ�ʹ�� OTU ��ȱ���Ϊ�������ݣ�ÿ��ɸѡ��������¼����������룬�����������������ٿ��ܵ��µľ���䶯��������
group_name <- unique(group$site)

adonis_result_two <- NULL
for (i in 1:(length(group_name) - 1)) {
  for (j in (i + 1):length(group_name)) {
    group_ij <- subset(group, site %in% c(group_name[i], group_name[j]))
    otu_ij <- otu[group_ij$names, ]
    adonis_result_otu_ij <- adonis(otu_ij~site, group_ij, permutations = 999, distance = 'bray')     #Bray-Curtis �����ȣ����� 999 ���û�
    adonis_result_two <- rbind(adonis_result_two, c(paste(group_name[i], group_name[j], sep = '/'), 'Bray-Curtis', unlist(data.frame(adonis_result_otu_ij$aov.tab, check.names = FALSE)[1, ])))
  }
}
adonis_result_two <- data.frame(adonis_result_two, stringsAsFactors = FALSE)
names(adonis_result_two) <- c('group', 'distance', 'Df', 'Sums of squares', 'Mean squares', 'F.Model', 'Variation (R2)', 'Pr (>F)')

#��ѡ���� p ֵУ�������� Benjamini У��
adonis_result_two$'Pr (>F)' <- as.numeric(adonis_result_two$'Pr (>F)')
adonis_result_two$P_adj_BH <- p.adjust(adonis_result_two$'Pr (>F)', method = 'BH')

#���
write.table(adonis_result_two, 'PERMANOVA.result_two.txt', row.names = FALSE, sep = '\t', quote = FALSE, na = '')
