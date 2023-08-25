library(plspm)

#��ȡ����
dat<-read.csv('coreenv.csv',row.names=1)
dat=na.omit(dat[,])
#dat <- read.delim('env.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
#ָ��Ǳ�������� R �����б���list���洢������Ǳ�����Ĺ�ϵ
#������ֱ��ָ�������ƣ�����ָ���е��±궼���ԣ��Ҹ���ϰ��ָ��������
dat_blocks <- list(
  space = c('latitude', 'longitude', 'MAT'),
  influent_quality = c('InfBOD', 'InfCOD', 'InfNH4', 'InfTN','InfTP'),
  operating_parameter = c('MLSS', 'SRT', 'HRT','DO','pH','Cond'),
  core =  c('Community1', 'Community2', 'Community3','Community4','Community5','Community6','Community7','Community8','Community9','Community10','Community11','Community12','Community13','Community14','Community15','Community16','Community17','Community18','Community19','Community20',
            'Community21', 'Community22', 'Community23','Community24','Community25','Community26','Community27','Community28','Community29','Community30','Community31','Community32','Community33','Community34','Community35','Community36','Community37','Community38','Community39','Community40')
)

dat_blocks

#ͨ�� 0-1 ��������Ǳ����֮��Ĺ��������� 0 ����������û�й�����1 �����й���
space<- c(0,0,0,0)
influent_quality<- c(1,0,0,0)
operating_parameter<- c(0,1,0,0)
core<- c(1,1,1,0)
dat_path <- rbind(space,influent_quality,operating_parameter,core)
colnames(dat_path) <- rownames(dat_path)
dat_path

#ָ�������ϵ����ѡ A�����������е��� �� B�����������е���
dat_modes <- rep('A', 4)
dat_modes

##һ���򵥵� PLS-PM������������� ?plspm
dat_pls <- plspm(dat, dat_path, dat_blocks, modes = dat_modes)
dat_pls
summary(dat_pls)

#������ݱȽ϶࣬ϸ�ڲ��ֻ������в��� plspm �����û��ֲ᣺
#�������ֲᣬ235ҳ��https://www.gastonsanchez.com/PLS_Path_Modeling_with_R.pdf
#����ֲᣬ10ҳ��https://rdrr.io/cran/plspm/f/inst/doc/plspm_introduction.pdf

#���½�չʾ��һ���������Ҫ������

#�鿴·��ϵ���Ĳ�������ֵ���Լ���ص�ͳ����Ϣ
dat_pls$path_coefs
dat_pls$inner_model


#�鿴�����ϵ��·��ͼ������ ?innerplot
innerplot(dat_pls, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'black', box.lwd = 0)

#�鿴��Ϊ��ԴǱ��������ԴǱ������״̬
dat_pls$inner_summary

#�鿴�������Ӱ��״̬
dat_pls$effects

#�鿴�۲������Ǳ������ϵ����ͨ�� outerplot() ��ͼչʾ����·��ͼ�Ľṹ������ ?outerplot
dat_pls$outer_model
outerplot(dat_pls, what = 'loadings', arr.width = 0.1, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray')
outerplot(dat_pls, what = 'weights', arr.width = 0.1, colpos = 'red', colneg = 'blue', show.values = TRUE, lcol = 'gray')

#goodness-of-fit ֵ���԰�������ģ���Ŷ�
dat_pls$gof

#�鿴Ǳ�����÷֣���������Ϊ��׼�����Ǳ������ֵ
dat_pls$scores

#���Ǳ������ֵ
#latent <- data.frame(dat_pls$scores)
#latent <- cbind(dat$site, latent)
#write.csv(latent, 'latent.csv')
write.table(dat_pls$inner_model, 'sem���ϵ��1.txt', sep = '\t')