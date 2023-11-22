# eta plot
library(SecondOrderSPDEMulti)

library(ggplot2)
library(ggpubr)
library(Cairo)


dat <- readRDS("eta_1.RDS")
dat2 <- readRDS("eta_2.RDS")
df1 <- dat$data
SG <- dat$SG
param <- dat$param
m <- param["numberSP"]
n <- param["numberTP"]
theta0 <- param["theta0"]
nu <- c(param["nu1"],param["nu2"],param["nu3"])
eta1 <- param["eta"]
eta2 <- dat2$param["eta"]
sigma <- param["sigma"]
alphaDash <- param["alphaDash"]
df2 <- dat2$data

ts <- 50
# Fixierung immer bei 0.5


index1 <- which(SG$V2 == 0.5 & SG$V3 == 0.5)
index2 <- which(SG$V1 == 0.5 & SG$V3 == 0.5)
index3 <- which(SG$V1 == 0.5 & SG$V2 == 0.5)




range <- 1000:1050
l1 <- lapply(range,function(i){
  df1[index1,i]
})
l2 <- lapply(range,function(i){
  df2[index1,i]
})
dat_paired_space <- data.frame(x= rep(seq(0,1,1/m),length(range)*2),
                               y=c(unlist(l1),unlist(l2)),
                               group = rep(1:length(range),each = (m+1)),
                               group2 = as.factor(rep(rep(1:2,each=m+1),each =length(range))))
dat_paired_space1 <- dat_paired_space


(g1 <- ggplot(dat_paired_space,aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0))+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("1"="","2"="")
    ),ncol=1,nrow=2)+
    ggtitle("Spatial process"))





range <- 1000:1050
l1 <- lapply(range,function(i){
  df1[index2,i]
})
l2 <- lapply(range,function(i){
  df2[index2,i]
})
dat_paired_space <- data.frame(x= rep(seq(0,1,1/m),length(range)*2),
                               y=c(unlist(l1),unlist(l2)),
                               group = rep(1:length(range),each = (m+1)),
                               group2 = as.factor(rep(rep(3:4,each=m+1),each =length(range))))
dat_paired_space2 <- dat_paired_space


(g2 <- ggplot(dat_paired_space,aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0))+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("3"="","4"="")
    ),ncol=1,nrow=2)+
    ggtitle("Spatial process"))



range <- 1000:1050
l1 <- lapply(range,function(i){
  df1[index3,i]
})
l2 <- lapply(range,function(i){
  df2[index3,i]
})
dat_paired_space <- data.frame(x= rep(seq(0,1,1/m),length(range)*2),
                               y=c(unlist(l1),unlist(l2)),
                               group = rep(1:length(range),each = (m+1)),
                               group2 = as.factor(rep(rep(5:6,each=m+1),each =length(range))))
dat_paired_space3 <- dat_paired_space


(g3 <- ggplot(dat_paired_space,aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0))+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("5"="","6"="")
    ),ncol=1,nrow=2)+
    ggtitle("Spatial process"))




range2 <- which(SG$V1 == 0.1 & SG$V2 == 0.1 & SG$V3 == 0.1)

dat_paired_time <- data.frame(x=rep(seq(0,1,1/n),length(range2)*2),
                              y = c(df1[range2,],df2[range2,]),
                              group = rep(1:length(range2),each = (n+1)),
                              group2 = as.factor(rep(rep(3:4,each=n+1),length(range2))))

(g4 <- ggplot(dat_paired_time,aes(x=x,y=y,group = group))+geom_line(alpha = 1,color=darkmint[7])+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0),
          axis.text=element_text(size=18),
          axis.title=element_text(size=24))+
    labs(x = bquote(t),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("3"="","4"="")),ncol=1,nrow=2
    )+
    ggtitle("Temporal process"))


gg <- ggarrange(g1,g2,g3,g4, ncol = 4)



dat_paired_space <- rbind(dat_paired_space1,dat_paired_space2,dat_paired_space3)


(g5 <- ggplot(transform(dat_paired_space,
                        group2=factor(group2,levels=c("1","3","5","2","4","6"))),
              aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0),
          axis.text=element_text(size=18),
          axis.title=element_text(size=24))+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("1"="","2"="","3"="","4"="","5"="","6"="")
    ),ncol=3,nrow=2)+
    ggtitle("Spatial process"))


(gg2 <- ggarrange(g5,g4,ncol = 2,widths=c(2,1)))



cairo_ps(file = "psm1.eps", onefile = T, fallback_resolution = 600,width = 40, height = 12,family = "serif")

annotate_figure(gg2,top = text_grob(bquote(phantom(0)[vartheta[0]==.(theta0)~","~nu=="("*.(nu[1]) *","*.(nu[2])*","~.(nu[3])*")"~","~ eta==.(eta1)~","~ alpha^"'" ==.(alphaDash)~","~sigma^2==.(sigma^2)~~"vs."~~
                                                        vartheta[0]==.(theta0)~","~nu=="("*.(nu[1]) *","*.(nu[2])*","~.(nu[3])*")"~","~ eta==.(eta2)~","~alpha^"'"==.(alphaDash)~","~sigma^2==.(sigma^2)]),size = ts))


dev.off()



# sigma plot
dat <- readRDS("sigma_1.RDS")
dat2 <- readRDS("sigma_2.RDS")
df1 <- dat$data
SG <- dat$SG
param <- dat$param
m <- param["numberSP"]
n <- param["numberTP"]
theta0 <- param["theta0"]
nu <- c(param["nu1"],param["nu2"],param["nu3"])
eta1 <- param["eta"]
eta2 <- dat2$param["eta"]
sigma1 <- param["sigma"]
sigma2 <- dat2$param["sigma"]
alphaDash <- param["alphaDash"]
df2 <- dat2$data

ts <- 50


index1 <- which(SG$V2 == 0.5 & SG$V3 == 0.5)
index2 <- which(SG$V1 == 0.5 & SG$V3 == 0.5)
index3 <- which(SG$V1 == 0.5 & SG$V2 == 0.5)




range <- 1000:1050
l1 <- lapply(range,function(i){
  df1[index1,i]
})
l2 <- lapply(range,function(i){
  df2[index1,i]
})
dat_paired_space <- data.frame(x= rep(seq(0,1,1/m),length(range)*2),
                               y=c(unlist(l1),unlist(l2)),
                               group = rep(1:length(range),each = (m+1)),
                               group2 = as.factor(rep(rep(1:2,each=m+1),each =length(range))))
dat_paired_space1 <- dat_paired_space


(g1 <- ggplot(dat_paired_space,aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0))+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("1"="","2"="")
    ),ncol=1,nrow=2)+
    ggtitle("Spatial process"))


range <- 1000:1050
l1 <- lapply(range,function(i){
  df1[index2,i]
})
l2 <- lapply(range,function(i){
  df2[index2,i]
})
dat_paired_space <- data.frame(x= rep(seq(0,1,1/m),length(range)*2),
                               y=c(unlist(l1),unlist(l2)),
                               group = rep(1:length(range),each = (m+1)),
                               group2 = as.factor(rep(rep(3:4,each=m+1),each =length(range))))
dat_paired_space2 <- dat_paired_space


(g2 <- ggplot(dat_paired_space,aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0))+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("3"="","4"="")
    ),ncol=1,nrow=2)+
    ggtitle("Spatial process"))



range <- 1000:1050
l1 <- lapply(range,function(i){
  df1[index3,i]
})
l2 <- lapply(range,function(i){
  df2[index3,i]
})
dat_paired_space <- data.frame(x= rep(seq(0,1,1/m),length(range)*2),
                               y=c(unlist(l1),unlist(l2)),
                               group = rep(1:length(range),each = (m+1)),
                               group2 = as.factor(rep(rep(5:6,each=m+1),each =length(range))))
dat_paired_space3 <- dat_paired_space


(g3 <- ggplot(dat_paired_space,aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0))+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("5"="","6"="")
    ),ncol=1,nrow=2)+
    ggtitle("Spatial process"))




range2 <- which(SG$V1 == 0.1 & SG$V2 == 0.1 & SG$V3 == 0.1)

dat_paired_time <- data.frame(x=rep(seq(0,1,1/n),length(range2)*2),
                              y = c(df1[range2,],df2[range2,]),
                              group = rep(1:length(range2),each = (n+1)),
                              group2 = as.factor(rep(rep(3:4,each=n+1),length(range2))))

(g4 <- ggplot(dat_paired_time,aes(x=x,y=y,group = group))+geom_line(alpha = 1,color=darkmint[7])+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0),
          axis.text=element_text(size=18),
          axis.title=element_text(size=24))+
    labs(x = bquote(t),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,labeller = as_labeller(
      c("3"="","4"="")),ncol=1,nrow=2
    )+
    ggtitle("Temporal process"))


gg <- ggarrange(g1,g2,g3,g4, ncol = 4)



dat_paired_space <- rbind(dat_paired_space1,dat_paired_space2,dat_paired_space3)


(g5 <- ggplot(transform(dat_paired_space,
                        group2=factor(group2,levels=c("1","3","5","2","4","6"))),
              aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0),
          axis.text=element_text(size=18),
          axis.title=element_text(size=24))+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("1"="","2"="","3"="","4"="","5"="","6"="")
    ),ncol=3,nrow=2)+
    ggtitle("Spatial process"))


(gg2 <- ggarrange(g5,g4,ncol = 2,widths=c(2,1)))



cairo_ps(file = "psm2.eps", onefile = T, fallback_resolution = 600,width = 40, height = 12,family = "serif")

annotate_figure(gg2,top = text_grob(bquote(phantom(0)[vartheta[0]==.(theta0)~","~nu=="("*.(nu[1]) *","*.(nu[2])*","~.(nu[3])*")"~","~ eta==.(eta1)~","~ alpha^"'" ==.(alphaDash)~","~sigma^2==.(sigma1^2)~~"vs."~~
                                                        vartheta[0]==.(theta0)~","~nu=="("*.(nu[1]) *","*.(nu[2])*","~.(nu[3])*")"~","~ eta==.(eta2)~","~alpha^"'"==.(alphaDash)~","~sigma^2==.(sigma2^2)]),size = ts))


dev.off()


# alpha plots
dat <- readRDS("alpha_1.RDS")
dat2 <- readRDS("alpha_2.RDS")
dat3 <- readRDS("alpha_3.RDS")
df1 <- dat$data
SG <- dat$SG
param <- dat$param
m <- param["numberSP"]
n <- param["numberTP"]
theta0 <- param["theta0"]
nu <- c(param["nu1"],param["nu2"],param["nu3"])
eta1 <- param["eta"]
eta2 <- dat2$param["eta"]
sigma <- param["sigma"]
alphaDash1 <- param["alphaDash"]
alphaDash2 <- dat2$param["alphaDash"]
alphaDash3 <- dat3$param["alphaDash"]

df2 <- dat2$data
df3 <- dat3$data

ts <- 50

index1 <- which(SG$V2 == 0.5 & SG$V3 == 0.5)
index2 <- which(SG$V1 == 0.5 & SG$V3 == 0.5)
index3 <- which(SG$V1 == 0.5 & SG$V2 == 0.5)




range <- 1000:1050
l1 <- lapply(range,function(i){
  df1[index1,i]
})
l2 <- lapply(range,function(i){
  df2[index1,i]
})
l3 <- lapply(range,function(i){
  df3[index1,i]
})
dat_paired_space <- data.frame(x= rep(seq(0,1,1/m),length(range)*3),
                               y=c(unlist(l1),unlist(l2),unlist(l3)),
                               group = rep(rep(1:length(range),each = (m+1),3)),
                               group2 = as.factor(rep(rep(1:3,each=m+1),each =length(range))))
dat_paired_space1 <- dat_paired_space


(g1 <- ggplot(dat_paired_space,aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0))+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("1"="","2"="","3"="")
    ),ncol=1,nrow=3)+
    ggtitle("Spatial process"))





range <- 1000:1050
l1 <- lapply(range,function(i){
  df1[index2,i]
})
l2 <- lapply(range,function(i){
  df2[index2,i]
})
l3 <- lapply(range,function(i){
  df3[index2,i]
})
dat_paired_space <- data.frame(x= rep(seq(0,1,1/m),length(range)*3),
                               y=c(unlist(l1),unlist(l2),unlist(l3)),
                               group = rep(rep(1:length(range),each = (m+1),3)),
                               group2 = as.factor(rep(rep(4:6,each=m+1),each =length(range))))
dat_paired_space2 <- dat_paired_space


(g2 <- ggplot(dat_paired_space,aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0))+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("4"="","5"="","6"="")
    ),ncol=1,nrow=3)+
    ggtitle("Spatial process"))



range <- 1000:1050
l1 <- lapply(range,function(i){
  df1[index3,i]
})
l2 <- lapply(range,function(i){
  df2[index3,i]
})
l3 <- lapply(range,function(i){
  df3[index3,i]
})
dat_paired_space <- data.frame(x= rep(seq(0,1,1/m),length(range)*3),
                               y=c(unlist(l1),unlist(l2),unlist(l3)),
                               group = rep(rep(1:length(range),each = (m+1),3)),
                               group2 = as.factor(rep(rep(7:9,each=m+1),each =length(range))))
dat_paired_space3 <- dat_paired_space


(g3 <- ggplot(dat_paired_space,aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0))+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("7"="","8"="","9"="")
    ),ncol=1,nrow=3)+
    ggtitle("Spatial process"))




range2 <- which(SG$V1 == 0.1 & SG$V2 == 0.1 & SG$V3 == 0.1)

dat_paired_time <- data.frame(x=rep(seq(0,1,1/n),length(range2)*3),
                              y = c(df1[range2,],df2[range2,],df3[range2,]),
                              group = rep(rep(1:length(range2),each = (n+1),3)),
                              group2 = as.factor(rep(rep(10:12,each=n+1),length(range2))))

(g4 <- ggplot(dat_paired_time,aes(x=x,y=y,group = group))+geom_line(alpha = 1,color=darkmint[7])+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0),
          axis.text=element_text(size=18),
          axis.title=element_text(size=24))+
    labs(x = bquote(t),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,labeller = as_labeller(
      c("10"="","11"="","12"="")),ncol=1,nrow=3
    )+
    ggtitle("Temporal process"))


gg <- ggarrange(g1,g2,g3,g4, ncol = 4)



dat_paired_space <- rbind(dat_paired_space1,dat_paired_space2,dat_paired_space3)


(g5 <- ggplot(transform(dat_paired_space,
                        group2=factor(group2,levels=c("1","4","7","2","5","8","3","6","9"))),
              aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0),
          axis.text=element_text(size=18),
          axis.title=element_text(size=24))+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)+
    facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
      c("1"="","2"="","3"="","4"="","5"="","6"="","7"="","8"="","9"="")
    ),ncol=3,nrow=3)+
    ggtitle("Spatial process"))


(gg2 <- ggarrange(g5,g4,ncol = 2,widths=c(2,1)))



cairo_ps(file = "psm3.eps", onefile = T, fallback_resolution = 600,width = 40, height = 12,family = "serif")

annotate_figure(gg2,top = text_grob(bquote(phantom(0)[vartheta[0]==.(theta0)~","~nu=="("*.(nu[1]) *","*.(nu[2])*","~.(nu[3])*")"~","~ eta==.(eta1)~","~ alpha^"'" ==.(alphaDash1)~","~sigma^2==.(sigma^2)~~"vs."~~
                                                        vartheta[0]==.(theta0)~","~nu=="("*.(nu[1]) *","*.(nu[2])*","~.(nu[3])*")"~","~ eta==.(eta2)~","~alpha^"'"==.(alphaDash2)~","~sigma^2==.(sigma^2)~~"vs."~~
                                                        vartheta[0]==.(theta0)~","~nu=="("*.(nu[1]) *","*.(nu[2])*","~.(nu[3])*")"~","~ eta==.(eta2)~","~alpha^"'"==.(alphaDash3)~","~sigma^2==.(sigma^2)]),size = ts))
dev.off()