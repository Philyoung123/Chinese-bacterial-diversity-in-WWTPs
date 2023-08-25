save.wd="C:/Users/yangkuo/Desktop/�й���ˮ�������зḻ��ϡ����Ⱥ����̬/3.2Biogeography principles of different microbial sub-communities/rare"
setwd(save.wd)
####����Ԥ����
##���ݾ�γ�ȼ��������ĵ�������
#��ȡ������ĵ���λ������
site <- read.csv('Sample.coordinates.csv', row.names = 1)
#����������ĵ�������
#geosphere �� distm() ���ݾ�γ�ȼ���������루Ĭ�Ͼ��뵥λ���ף�
#distm() Ҫ���������ݣ���һ���Ǿ��ȣ��ڶ�����γ��
site_dis <- geosphere::distm(site[c('Lon', 'Lat')]) 
rownames(site_dis) <- rownames(site)
colnames(site_dis) <- rownames(site)

#������������������ת��Ϊ������Ӧ��ֵ�����ݿ�ṹ
site_dis <- reshape2::melt(site_dis)
site_dis <- subset(site_dis, value != 0)
head(site_dis)

##����Ⱥ�������������ƶȣ�����ԭ��������ʹ�� Bray-curtis ���ƶ�
#Ⱥ�������������
spe <- read.csv('rare.csv',row.names = 1)
spe <- data.frame(t(spe))
spe <- spe[rownames(site), ]  #�� site ��Ԥ��ѡ������ݶ�Ӧ

#vegan �� vegdist() ����Ⱥ���������� Bray-curtis ����Ⱦ���
#��ͨ�� 1-Bray-curtis ����Ȼ�� Bray-curtis ���ƶ�
comm_sim <- 1 - as.matrix(vegan::vegdist(spe, method = 'bray'))

#������ת��Ϊ����Ⱥ���Ӧ��ֵ�����ݿ�ṹ
diag(comm_sim) <- 0  #ȥ��Ⱥ�����ƶȾ����еĶԽ���ֵ�������������������ƶ�
comm_sim[upper.tri(comm_sim)] <- 0  #Ⱥ�����ƶȾ����ǶԳƵģ����ֻѡ������ǣ��������ǣ��������ֵ����
comm_sim <- reshape2::melt(comm_sim)
comm_sim <- subset(comm_sim, value != 0)
head(comm_sim)

##����������Ⱥ�����ƶ����ݺϲ�
comm_dis <- merge(comm_sim, site_dis, by = c('Var1', 'Var2'))
names(comm_dis) <- c('site1', 'site2', 'comm_sim', 'site_dis')

####���
write.table(comm_dis, 'dis-bray(rare).txt', sep = '\t', row.names = FALSE, quote = FALSE)
####һԪһ�λع�
##�����Ǳ������
#��ȡ��������ļ��������Ա������Ϊ��
comm_dis <- read.delim('dis-bray(rare).txt', sep = '\t')

#ע��site_dis �еĵ�λ���ף�m���������ִ�߶ȷ�Χ�£���ǧ�ף�km����Ϊ���������һЩ
comm_dis$site_dis_km <- comm_dis$site_dis/1000

#lm() ���Ⱥ��������ƶȣ�comm_sim���������루site_dis_km����һԪ���Թ�ϵ
fit <- lm(comm_sim~site_dis_km, data = comm_dis)
summary(fit)  #չʾ���ģ�͵ļ�ͳ��

#��ȡ��Ҫ��ֵ������
coefficients(fit)[1]  #��ȡ�ؾ�
coefficients(fit)[2]  #��ȡ site_dis_km ��б��

#Ҳ���� names(summary(fit)) ��鿴��Ҫ�������Ȼ�������ȡ������
summary(fit)$adj.r.squared  #У���� R2