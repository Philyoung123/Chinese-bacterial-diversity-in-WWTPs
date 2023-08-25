library(vegan)

##�����ļ�
#OTU ��ȱ�
otu <- read.delim('regional_local_abundance.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- data.frame(t(otu))

#���������ļ�
group <- read.delim('group.txt', sep = '\t', stringsAsFactors = FALSE)
##ANOSIM ������ʹ��ѭ������������С�����Ƚϣ�������䣩
#�Ƽ�ʹ�� OTU ��ȱ���Ϊ�������ݣ�ÿ��ɸѡ��������¼����������룬�����������������ٿ��ܵ��µľ���䶯��������
group_name <- unique(group$site)

dir.create('anosim_two', recursive = TRUE)
anosim_result_two <- NULL
for (i in 1:(length(group_name) - 1)) {
  for (j in (i + 1):length(group_name)) {
    group_ij <- subset(group, site %in% c(group_name[i], group_name[j]))
    otu_ij <- otu[group_ij$names, ]
    anosim_result_otu_ij <- anosim(otu_ij, group_ij$site, permutations = 999, distance = 'bray')     #Bray-Curtis �����ȣ����� 999 ���û�
    anosim_result_two <- rbind(anosim_result_two, c(paste(group_name[i], group_name[j], sep = '/'), 'Bray-Curtis', anosim_result_otu_ij$statistic, anosim_result_otu_ij$signif))
    
    #ÿ��ѭ�����ͼƬ
    #pdf(paste('anosim_two/anosim.', group_name[i], '_', group_name[j], '.pdf', sep = ''), width = 7, height = 5)
    png(paste('anosim_two/anosim.', group_name[i], '_', group_name[j], '.png', sep = ''), width = 600, height = 400)
    plot(anosim_result_otu_ij, col = c('gray', 'red', 'blue'))
    dev.off()
  }
}

#�� R ֵ�� p ֵ�ı���
anosim_result_two <- data.frame(anosim_result_two, stringsAsFactors = FALSE)
names(anosim_result_two) <- c('group', 'distance', 'R', 'P_value')

#��ѡ���� p ֵУ�����̣����� Benjamini У��
anosim_result_two$P_value <- as.numeric(anosim_result_two$P_value)
anosim_result_two$P_adj_BH <- p.adjust(anosim_result_two$P_value, method = 'BH')

write.table(anosim_result_two, 'anosim_two/ANOSIM.result_two.txt', row.names = FALSE, sep = '\t', quote = FALSE, na = '')