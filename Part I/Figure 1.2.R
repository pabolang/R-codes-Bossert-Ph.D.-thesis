# Parameter Study
library(parallel)
library(ggplot2)
library(ggpubr)
library(plotly)
library(pbapply)
library(plotly)
library(beepr)
library(pbmcapply)
library(reshape2)
options(rgl.useNULL=TRUE)
library(rayshader)
library(latex2exp)
library(Cairo)
library(ParabolicSPDEs)


source("Color Palette.R")



xi <- function(y){0*y}
spatialPoints <- 100
temporalPoints <- 10000
L <- 20
theta0 <- 0

theta1_1 <- -10
theta2_1 <- 1
sigma_1 <- 1
ts <- 50

dat1 <- simulateSPDEmodel(theta0=theta0,theta1=theta1_1,theta2=theta2_1,sigma=sigma_1,
                          xi=xi,numberTemporalPoints=temporalPoints,
                          numberSpatialPoints=spatialPoints,L=L)



theta1_2 <- 10
theta2_2 <- 1
sigma_2 <- 1

dat2 <- simulateSPDEmodel(theta0=theta0,theta1=theta1_2,theta2=theta2_2,sigma=sigma_2,
                          xi=xi,numberTemporalPoints=temporalPoints,
                          numberSpatialPoints=spatialPoints,L=L)



theta1_3 <- 0
theta2_3 <- 1
sigma_3 <- 1

dat3 <- simulateSPDEmodel(theta0=theta0,theta1=theta1_3,theta2=theta2_3,sigma=sigma_3,
                          xi=xi,numberTemporalPoints=temporalPoints,
                          numberSpatialPoints=spatialPoints,L=L)

theta1_4 <- 0
theta2_4 <- 1
sigma_4 <- 2

dat4 <- simulateSPDEmodel(theta0=theta0,theta1=theta1_4,theta2=theta2_4,sigma=sigma_4,
                          xi=xi,numberTemporalPoints=temporalPoints,
                          numberSpatialPoints=spatialPoints,L=L)

theta1_5 <- 10
theta2_5 <- 1
sigma_5 <- 1

dat5 <- simulateSPDEmodel(theta0=theta0,theta1=theta1_5,theta2=theta2_5,sigma=sigma_5,
                          xi=xi,numberTemporalPoints=temporalPoints,
                          numberSpatialPoints=spatialPoints,L=L)


theta1_6 <- 10
theta2_6 <- 10
sigma_6 <- 1

dat6 <- simulateSPDEmodel(theta0=theta0,theta1=theta1_6,theta2=theta2_6,sigma=sigma_6,
                          xi=xi,numberTemporalPoints=temporalPoints,
                          numberSpatialPoints=spatialPoints,L=L)



dat_list <- list(dat1,dat2,dat3,dat4,dat5,dat6)
m <- dim(dat_list[[1]])[2]-1


plot_list <- lapply(1:6, function(i){
  dat <- dat_list[[i]]
  
  range <- 1000:1020
  l <- lapply(range,function(i){
    dat[i,]
  })
  dat_plot <- data.frame(x = rep(seq(0,1,1/m),length(range)),
                         y = unlist(l),
                         group = rep(1:length(range),each = (m+1)))
  
  ggplot(dat_plot,aes(x=x,y=y,group=group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
    scale_x_continuous(breaks = seq(0,1,0.1))+
    theme(legend.position = "none")+
    labs(x = bquote(y),y=bquote(X[t](y)))+
    scale_color_gradientn(colours = darkmint)
  
})
ggarrange(plotlist = plot_list,ncol = 2, nrow = 3)



m <- dim(dat_list[[1]])[2]-1
n <- dim(dat_list[[1]])[1]-1
range <- 1000:1020
range2 <- which(seq(0,1,1/m) %in% c(0.1))


j <- 1
l <- lapply(range,function(i){
  c(dat_list[[2*j-1]][i,],dat_list[[2*j]][i,])
})
l2 <- lapply(range2,function(i){
  c(dat_list[[2*j-1]][,i],dat_list[[2*j]][,i])
})

dat_paired_space <- data.frame(x= rep(seq(0,1,1/m),length(range)*2),
                               y=unlist(l),
                               group = rep(1:length(range),each = (m+1)),
                               group2 = as.factor(rep(rep(1:2,each=m+1),length(range))))

dat_paired_time <- data.frame(x=rep(seq(0,1,1/n),length(range2)*2),
                              y = unlist(l2),
                              group = rep(1:length(range2),each = (n+1)),
                              group2 = as.factor(rep(rep(3:4,each=n+1),length(range2))))



g1 <- ggplot(dat_paired_space,aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
  scale_x_continuous(breaks = seq(0,1,0.1))+
  theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0),
        axis.text=element_text(size=18),
        axis.title=element_text(size=24))+
  labs(x = bquote(y),y=bquote(X[t](y)))+
  scale_color_gradientn(colours = darkmint)+
  facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
    c("1"="","2"="")
  ))+
  ggtitle("Spatial process")

g2 <- ggplot(dat_paired_time,aes(x=x,y=y,group = group))+geom_line(alpha = 1,color=darkmint[7])+theme_minimal()+
  scale_x_continuous(breaks = seq(0,1,0.1))+
  theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0),
        axis.text=element_text(size=18),
        axis.title=element_text(size=24))+
  labs(x = bquote(t),y=bquote(X[t](y)))+
  scale_color_gradientn(colours = darkmint)+
  facet_wrap(~group2,scales = "free_y",labeller = as_labeller(
    c("3"="","4"=""))
  )+
  ggtitle("Temporal process")

(gg <- ggarrange(g1,g2,ncol=2))

cairo_ps(file = "Plots/ps1.eps", onefile = T, fallback_resolution = 600,width = 40, height = 12,family="serif")
annotate_figure(gg,top = text_grob(bquote(vartheta[0]==.(theta0)~","~vartheta[1]==.(theta1_1)~","~vartheta[2]==.(theta2_1)~","~sigma^2==.(sigma_1)~~"vs."~~
                                            vartheta[0]==.(theta0)~","~vartheta[1]==.(theta1_2)~","~vartheta[2]==.(theta2_2)~","~sigma^2==.(sigma_2)),size = ts))
dev.off()

j <- 2
l <- lapply(range,function(i){
  c(dat_list[[2*j-1]][i,],dat_list[[2*j]][i,])
})
l2 <- lapply(range2,function(i){
  c(dat_list[[2*j-1]][,i],dat_list[[2*j]][,i])
})

dat_paired_space <- data.frame(x= rep(seq(0,1,1/m),length(range)*2),
                               y=unlist(l),
                               group = rep(1:length(range),each = (m+1)),
                               group2 = as.factor(rep(rep(1:2,each=m+1),length(range))))

dat_paired_time <- data.frame(x=rep(seq(0,1,1/n),length(range2)*2),
                              y = unlist(l2),
                              group = rep(1:length(range2),each = (n+1)),
                              group2 = as.factor(rep(rep(3:4,each=n+1),length(range2))))
g1 <- ggplot(dat_paired_space,aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
  scale_x_continuous(breaks = seq(0,1,0.1))+
  theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0),
        axis.text=element_text(size=18),
        axis.title=element_text(size=24))+
  labs(x = bquote(y),y=bquote(X[t](y)))+
  scale_color_gradientn(colours = darkmint)+
  facet_wrap(~group2,labeller = as_labeller(
    c("1"="","2"="")
  ))+
  ggtitle("Spatial process")

g2 <- ggplot(dat_paired_time,aes(x=x,y=y,group = group))+geom_line(alpha = 1,color=darkmint[7])+theme_minimal()+
  scale_x_continuous(breaks = seq(0,1,0.1))+
  theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0),
        axis.text=element_text(size=18),
        axis.title=element_text(size=24))+
  labs(x = bquote(t),y=bquote(X[t](y)))+
  scale_color_gradientn(colours = darkmint)+
  facet_wrap(~group2,labeller = as_labeller(
    c("3"="","4"=""))
  )+
  ggtitle("Temporal process")

(gg <- ggarrange(g1,g2,ncol=2))

cairo_ps(file = "Plots/ps2.eps", onefile = T, fallback_resolution = 600,width = 40, height = 12,family="serif")
annotate_figure(gg,top = text_grob(bquote(vartheta[0]==.(theta0)~","~vartheta[1]==.(theta1_3)~","~vartheta[2]==.(theta2_3)~","~sigma^2==.(sigma_3)~~"vs."~~
                                            vartheta[0]==.(theta0)~","~vartheta[1]==.(theta1_4)~","~vartheta[2]==.(theta2_4)~","~sigma^2==.(sigma_4^2)),size = ts))
dev.off()





j <- 3
l <- lapply(range,function(i){
  c(dat_list[[2*j-1]][i,],dat_list[[2*j]][i,])
})
l2 <- lapply(range2,function(i){
  c(dat_list[[2*j-1]][,i],dat_list[[2*j]][,i])
})

dat_paired_space <- data.frame(x= rep(seq(0,1,1/m),length(range)*2),
                               y=unlist(l),
                               group = rep(1:length(range),each = (m+1)),
                               group2 = as.factor(rep(rep(1:2,each=m+1),length(range))))

dat_paired_time <- data.frame(x=rep(seq(0,1,1/n),length(range2)*2),
                              y = unlist(l2),
                              group = rep(1:length(range2),each = (n+1)),
                              group2 = as.factor(rep(rep(3:4,each=n+1),length(range2))))
g1 <- ggplot(dat_paired_space,aes(x=x,y=y,group = group,color=group))+geom_line(alpha = 0.5)+theme_minimal()+
  scale_x_continuous(breaks = seq(0,1,0.1))+
  theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0),
        axis.text=element_text(size=18),
        axis.title=element_text(size=24))+
  labs(x = bquote(y),y=bquote(X[t](y)))+
  scale_color_gradientn(colours = darkmint)+
  facet_wrap(~group2,labeller = as_labeller(
    c("1"="","2"="")
  ))+
  ggtitle("Spatial process")

g2 <- ggplot(dat_paired_time,aes(x=x,y=y,group = group))+geom_line(alpha = 1,color=darkmint[7])+theme_minimal()+
  scale_x_continuous(breaks = seq(0,1,0.1))+
  theme(legend.position = "none",plot.title = element_text(size = ts,hjust = 0),
        axis.text=element_text(size=18),
        axis.title=element_text(size=24))+
  labs(x = bquote(t),y=bquote(X[t](y)))+
  scale_color_gradientn(colours = darkmint)+
  facet_wrap(~group2,labeller = as_labeller(
    c("3"="","4"=""))
  )+
  ggtitle("Temporal process")

(gg <- ggarrange(g1,g2,ncol=2))

cairo_ps(file = "Plots/ps3.eps", onefile = T, fallback_resolution = 600,width = 40, height = 12,family="serif")

annotate_figure(gg,top = text_grob(bquote(vartheta[0]==.(theta0)~","~vartheta[1]==.(theta1_5)~","~vartheta[2]==.(theta2_5)~","~sigma^2==.(sigma_5)~~"vs."~~
                                            vartheta[0]==.(theta0)~","~vartheta[1]==.(theta1_6)~","~vartheta[2]==.(theta2_6)~","~sigma^2==.(sigma_6)),size = ts))
dev.off()