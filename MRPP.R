library(vegan)

##�����ļ�
#OTU ��ȱ�
otu <- read.delim('regional_local_abundance.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#���������ļ�
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)
##MRPP ������ʹ��ѭ������������С�����Ƚϣ�������䣩
#�Ƽ�ʹ�� OTU ��ȱ���Ϊ�������ݣ�ÿ��ɸѡ��������¼����������룬�����������������ٿ��ܵ��µľ���䶯��������
group_name <- unique(group$site)

mrpp_result_two <- NULL
for (i in 1:(length(group_name) - 1)) {
  for (j in (i + 1):length(group_name)) {
    group_ij <- subset(group, site %in% c(group_name[i], group_name[j]))
    otu_ij <- otu[group_ij$names, ]
    mrpp_result_otu_ij <- mrpp(otu_ij, group_ij$site, permutations = 999, distance = 'bray')     #Bray-Curtis �����ȣ����� 999 ���û�
    mrpp_result_two <- rbind(mrpp_result_two, c(paste(group_name[i], group_name[j], sep = '/'), 'Bray-Curtis', mrpp_result_otu_ij$A, mrpp_result_otu_ij$delta, mrpp_result_otu_ij$E.delta, mrpp_result_otu_ij$Pvalue))
  }
}
mrpp_result_two <- data.frame(mrpp_result_two, stringsAsFactors = FALSE)
names(mrpp_result_two) <- c('group', 'distance', 'A', 'Observe_delta', 'Expect_delta', 'P_value')

#��ѡ���� p ֵУ�����̣����� Benjamini У��
mrpp_result_two$P_value <- as.numeric(mrpp_result_two$P_value)
mrpp_result_two$P_adj_BH <- p.adjust(mrpp_result_two$P_value, method = 'BH')

#���
write.table(mrpp_result_two, 'MRPP.result_two.txt', row.names = FALSE, sep = '\t', quote = FALSE, na = '')