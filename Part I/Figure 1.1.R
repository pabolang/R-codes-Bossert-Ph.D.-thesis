library(ggplot2)
library(ggpubr)

range <- seq(10000,10059,2)
m <- 500
n <- 250000
fontSize <- 14

cases <- c("C11_m10_kappa0_sigma1","C12_m10_kappa1_sigma1","C13_m10_kappa6_sigma1",
           "C21_m100_kappa0_sigma1","C22_m100_kappa1_sigma1","C23_m100_kappa6_sigma1",
           "C41_m10_kappa0_sigma05","C42_m10_kappa1_sigma05","C43_m10_kappa6_sigma05",
           "C51_m100_kappa0_sigma05","C52_m100_kappa1_sigma05","C53_m100_kappa6_sigma05",
           "C71_m10_kappa0_sigma2","C72_m10_kappa1_sigma2","C73_m10_kappa6_sigma2",
           "C81_m100_kappa0_sigma2","C82_m100_kappa1_sigma2","C83_m100_kappa6_sigma2")

dat1 <- readRDS(paste("Case Study/",cases[19],"/dat1.rds",sep=""))
dat2 <- readRDS(paste("Case Study/",cases[20],"/dat1.rds",sep=""))
dat3 <- readRDS(paste("Case Study/",cases[21],"/dat1.rds",sep=""))
l1 <- lapply(range,function(i){
  dat1[i,]
})
l2 <- lapply(range,function(i){
  dat2[i,]
})
l3 <- lapply(range,function(i){
  dat3[i,]
})


dat4 <- data.frame(x = rep(seq(0,1,1/m),length(range)),
                   y = unlist(l1),
                   group = rep(1:length(range),each = m+1))
dat5 <- data.frame(x = rep(seq(0,1,1/m),length(range)),
                   y = unlist(l2),
                   group = rep(1:length(range),each = m+1))
dat6 <- data.frame(x = rep(seq(0,1,1/m),length(range)),
                   y = unlist(l3),
                   group = rep(1:length(range),each = m+1))
dat <- rbind(dat4,dat5,dat6)
dat$group2 <- as.factor(rep(1:3,each=length(unlist(l1))))



(g <- ggplot(dat,aes(x=x,y=y,group=group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    geom_vline(xintercept = 0.99,linetype="dashed",lwd=0.7)+
    geom_vline(xintercept = 0.01,linetype="dashed",lwd=0.7)+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    theme(legend.position = "none",
          axis.text=element_text(size=fontSize),
          axis.title.x = element_text(size=fontSize),
          axis.title.y = element_text(size=fontSize))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("1"="","2"="","3"="")),ncol=1))