library(linkET)
library(ggplot2)
library(dplyr)
library(vegan)
rm(list=ls())
com <- read.csv("rareandcore.csv",row.names=1,header=T)
comm <- t(com)
env<-read.csv("env.csv",header=T,row.names=1)
size = dim(comm)

env1=na.omit(env[,])
dim(env1)
comm=as.data.frame(comm[match(rownames(env1),rownames(comm)),])
row.names(comm)




cor = correlate(env1,
                method = "pearson",
                adjust = TRUE, 
                adjust_method = "fdr") %>% 
  as_md_tbl() # תΪ���ݿ�����
cor
cor.p1 <- correlate(env1,
                    method = "pearson",
                    adjust = TRUE, 
                    adjust_method = "fdr") %>% 
  qcorrplot()+
  geom_square() # �Ծ��δ�С����ɫ��ǳ��ʾ����Դ�С
cor.p1
library(ggtext) # �����������ǩ
library(ggsci) # ϰ��ʹ�õ���ɫ����
##basic markdown/html��ʽ���ñ�ǩ��ֱ����ͼ��չʾ��ѧ���� ������ǳ���Ҫ Ҫ��
colnames(env1) = c("Lati","Longi","MAT","InfBOD","InfCOD",
                  "InfNH4","InfTN","InfTP","DO","pH","Cond","MLSS",
                  "SRT","HRT")


cor.p2.1 = correlate(
  env1,
  method = "pearson",
  adjust = TRUE, 
  adjust_method = "fdr") %>% 
  #as_md_tbl() %>%
  qcorrplot(
    type = "lower",
    diag = FALSE,
    grid_col = "white", # ��������ɫ
    grid_size = 0.25, # �����С
    use_md = TRUE # ����ʹ��ggtext::element_markdown()�������ǩ
  ) +
  geom_tile() + # ֻ����ɫ��ʾ����Դ�С
  geom_mark(# ����rֵ�������Ա��
    sep = '\n', 
    size = 3, 
    sig_level = c(0.05, 0.01, 0.001), # ������ˮƽ����
    sig_thres = 0.05 # ��������ֵ��pֵ������ֵ�������ϵ�����ᱻ���ơ�
  ) +
  geom_diag_label(# basic markdown/html��ʽ���ñ�ǩ
    geom = "richtext",
    angle=45
    #nudge_x = 0.1,# �ɵ�����ǩλ��
    #nudge_y = 0.1
  ) + # ���ӶԽ��߱�ǩ
  theme(## element_black()δ�����ã�Ϊ��ֻ�����Խ��߱�ǩ����������Ϊ��ɫ��
    axis.text.x = element_text(color = "white"),
    axis.text.y = element_text(color = "white"),
    legend.position = c(-0.1,0.5)
  ) 
cor.p2.1
cor.p2.2 = cor.p2.1 +
  scale_fill_gsea(limits = c(-1, 1),
                  breaks = seq(-1,1,0.5)) # ʹ��ggsci�е���ɫ��

cor.p2.3 = cor.p2.1 +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu"),
                       limits = c(-1, 1),
                       breaks = seq(-1,1,0.2)) # RColorBrewer��ɫ��

cor.p2.4 = cor.p2.1 +
  scale_fill_gradientn(
    colours = colorRampPalette(colors =c("darkgreen","white","red"),space="Lab")(10),
    limits = c(-1, 1),
    breaks = seq(-1,1,0.5)) # ����������ɫ����
cor.p2.4
ggsave("cor.p2.4.png", width = 30, height = 20, units = "cm")


mantel <- mantel_test(spec = comm, env = env1, spec_select = list(rare = 1:30221,abundant = 30222:30406), mantel_fun = 'mantel')



mantel
mantel <- mutate(mantel, 
                 rd = cut(r, right = TRUE,# ��ʾ�ָ�������ʽΪ(b1,b2]��
                          breaks = c(-Inf, 0.3, 0.6, 0.8, Inf),
                          labels = c('[0, 0.3]', '(0.3, 0.6]', '(0.6, 0.8]', '(0.8, 1]')
                 ),
                 pd = cut(p, right = FALSE,# ��ʾ�ָ�������ʽΪ[b1,b2)��
                          breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                          labels = c('***', '**', '*', 'ns'),
                 )
)
mantel


com= correlate(
  env1,
  method = "pearson",
  adjust = TRUE, 
  adjust_method = "fdr") %>% 
  qcorrplot(
    type = "upper",
    diag = FALSE, 
    grid_col = "black", # ��������ɫ
    grid_size = 0.25, # �����С
    use_md = TRUE # ����ʹ��ggtext::element_markdown()�������ǩ
  ) +
  geom_square() + # ����ɫ�;��������ʾ����Դ�С
  geom_mark(# ����rֵ�������Ա��
    sep = '\n', 
    size = 3, 
    sig_level = c(0.05, 0.01, 0.001), # ������ˮƽ����
    sig_thres = 0.05 # ��������ֵ��pֵ������ֵ�������ϵ�����ᱻ���ơ�
  ) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu"),
                       limits = c(-1, 1),
                       breaks = seq(-1,1,0.5)) +
  geom_couple(aes(color = pd, size = rd), data = mantel, 
              label.size = 4,
              drop = TRUE,
              label.colour = "black",
              label.fontface = 1,
              nudge_x = 0.5,
              curvature = nice_curvature()) +
  scale_size_manual(values = c(0.5, 1, 1.5, 2)) +  #�������õ�mantel�����rֵ���������ߴ�ϸ
  scale_color_manual(values = c("#D62728FF","#E0E0E0")) +  #�������õ�mantel�����pֵ������������ɫ,��Ϊp���ֻ���������䣬����ֻ����������ɫ��
  guides(color = guide_legend(title = "Mantel's p", order = 1), #ͼ�����������
         size = guide_legend(title = "Mantel's r", order = 2), 
         fill = guide_colorbar(title = "pearson's r", order = 3)) +
  theme(legend.key = element_blank())
com
ggsave("com.pdf", width = 30, height = 20, units = "cm")
write.table(mantel,'mantelresult.csv')