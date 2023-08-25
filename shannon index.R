library(vegan) 
library("picante")
alpha <- read.csv('α多样性系数.csv',row.names = 1)
library(ggplot2)
p <- ggplot(alpha, aes(x = group, y = Shannon, fill = group))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.title=element_blank(),axis.title.x=element_blank(),
        strip.background=element_rect(fill="black",size=0.1),
        strip.text=element_text(colour="white"))+
  ylim(2,9)
p
ggsave("YBC_chao1.PDF", p, width = 5.5, height = 5.5)
