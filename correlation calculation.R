library(geosphere)
save.wd="C:/Users/yangkuo/Desktop/中国污水处理厂中丰富及稀有子群落生态/3.2Biogeography principles of different microbial sub-communities/core"
setwd(save.wd)
bray <- read.csv("bray-dis.csv",header=T)
cor.test(bray$geo.dis,bray$dissim)
