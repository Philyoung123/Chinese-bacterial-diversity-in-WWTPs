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
  as_md_tbl() # 转为数据框类型
cor
cor.p1 <- correlate(env1,
                    method = "pearson",
                    adjust = TRUE, 
                    adjust_method = "fdr") %>% 
  qcorrplot()+
  geom_square() # 以矩形大小和颜色深浅表示相关性大小
cor.p1
library(ggtext) # 用于设置轴标签
library(ggsci) # 习惯使用的颜色设置
##basic markdown/html形式设置标签，直接在图中展示化学符号 这下面非常重要 要改
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
    grid_col = "white", # 网格线颜色
    grid_size = 0.25, # 网格大小
    use_md = TRUE # 设置使用ggtext::element_markdown()设置轴标签
  ) +
  geom_tile() + # 只以颜色表示相关性大小
  geom_mark(# 添加r值与显著性标记
    sep = '\n', 
    size = 3, 
    sig_level = c(0.05, 0.01, 0.001), # 显著性水平设置
    sig_thres = 0.05 # 显著性阈值，p值大于阈值的相关性系数不会被绘制。
  ) +
  geom_diag_label(# basic markdown/html形式设置标签
    geom = "richtext",
    angle=45
    #nudge_x = 0.1,# 可调整标签位置
    #nudge_y = 0.1
  ) + # 添加对角线标签
  theme(## element_black()未起作用，为了只保留对角线标签，设置字体为白色。
    axis.text.x = element_text(color = "white"),
    axis.text.y = element_text(color = "white"),
    legend.position = c(-0.1,0.5)
  ) 
cor.p2.1
cor.p2.2 = cor.p2.1 +
  scale_fill_gsea(limits = c(-1, 1),
                  breaks = seq(-1,1,0.5)) # 使用ggsci中的颜色集

cor.p2.3 = cor.p2.1 +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu"),
                       limits = c(-1, 1),
                       breaks = seq(-1,1,0.2)) # RColorBrewer颜色集

cor.p2.4 = cor.p2.1 +
  scale_fill_gradientn(
    colours = colorRampPalette(colors =c("darkgreen","white","red"),space="Lab")(10),
    limits = c(-1, 1),
    breaks = seq(-1,1,0.5)) # 自行设置颜色区间
cor.p2.4
ggsave("cor.p2.4.png", width = 30, height = 20, units = "cm")


mantel <- mantel_test(spec = comm, env = env1, spec_select = list(rare = 1:30221,abundant = 30222:30406), mantel_fun = 'mantel')



mantel
mantel <- mutate(mantel, 
                 rd = cut(r, right = TRUE,# 表示分割区间形式为(b1,b2]。
                          breaks = c(-Inf, 0.3, 0.6, 0.8, Inf),
                          labels = c('[0, 0.3]', '(0.3, 0.6]', '(0.6, 0.8]', '(0.8, 1]')
                 ),
                 pd = cut(p, right = FALSE,# 表示分割区间形式为[b1,b2)。
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
    grid_col = "black", # 网格线颜色
    grid_size = 0.25, # 网格大小
    use_md = TRUE # 设置使用ggtext::element_markdown()设置轴标签
  ) +
  geom_square() + # 以颜色和矩形面积表示相关性大小
  geom_mark(# 添加r值与显著性标记
    sep = '\n', 
    size = 3, 
    sig_level = c(0.05, 0.01, 0.001), # 显著性水平设置
    sig_thres = 0.05 # 显著性阈值，p值大于阈值的相关性系数不会被绘制。
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
  scale_size_manual(values = c(0.5, 1, 1.5, 2)) +  #根据设置的mantel相关性r值区间设置线粗细
  scale_color_manual(values = c("#D62728FF","#E0E0E0")) +  #根据设置的mantel相关性p值区间设置线颜色,因为p结果只有两个区间，可以只设置两个颜色。
  guides(color = guide_legend(title = "Mantel's p", order = 1), #图例标题和排序
         size = guide_legend(title = "Mantel's r", order = 2), 
         fill = guide_colorbar(title = "pearson's r", order = 3)) +
  theme(legend.key = element_blank())
com
ggsave("com.pdf", width = 30, height = 20, units = "cm")
write.table(mantel,'mantelresult.csv')
