library(latex2exp)
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
library(processx)
library(reticulate)


source("Help_Functions.r")
source("Color Palette.R")

spatial_cases <- rep(rep(c(10,100),each=3),3)
timeHorizon <- 1
temporal_cases <- rep(rep(c(1000,10000),each=3),3)

sigma_cases <- rep(c(1,0.5,2),each=6)
theta0 <- 0

theta1_cases <- rep(c(0,1,6),6)
theta2 <- 1
cases <- c("C11_m10_kappa0_sigma1","C12_m10_kappa1_sigma1","C13_m10_kappa6_sigma1",
           "C21_m100_kappa0_sigma1","C22_m100_kappa1_sigma1","C23_m100_kappa6_sigma1",
           "C41_m10_kappa0_sigma05","C42_m10_kappa1_sigma05","C43_m10_kappa6_sigma05",
           "C51_m100_kappa0_sigma05","C52_m100_kappa1_sigma05","C53_m100_kappa6_sigma05",
           "C71_m10_kappa0_sigma2","C72_m10_kappa1_sigma2","C73_m10_kappa6_sigma2",
           "C81_m100_kappa0_sigma2","C82_m100_kappa1_sigma2","C83_m100_kappa6_sigma2")
to <- 1000
spatialDelta <- 0.05

select1 <- c(2,3,8,9)
select2 <- c(5,6,11,12)

colorPair1 <- adobeColorsDiscrete[c(1,3)]
colorPair2 <- adobeColorsDiscrete[c(1,2)]
alpha <- 0.5
fontSize <- 10


list_densityPlot_kappa1 <- lapply(select1, function(i){
  n <- temporal_cases[i]
  y <- seq(0,1,1/spatial_cases[i])
  yWithoutBounds <- y[which(y >= spatialDelta)[1] : (which( round(y,2) > round((timeHorizon - spatialDelta),2))[1]-1 )]
  m <- length(y)
  kappa_true <- theta1_cases[i]/theta2
  sigma_true <- (sigma_cases[i]/sqrt(theta2))^2
  dat <- readRDS(paste("Case Study Results Data/result_kappa_",cases[i],".RDS",sep=""))
  
  asympVar_LSE <- asympVariance_etaLSE(kappa_true,sigma_true,spatialDelta)[2,2]
  asympVar_MLE <- asympVariance_etaMLE(kappa_true,sigma_true,spatialDelta)[2,2]
  x <- seq(min(sqrt(m*n)*(dat$y-kappa_true)),max(sqrt(m*n)*(dat$y-kappa_true)),length.out = 80)
  dat_asympVar <- data.frame(x = rep(x,3),
                             y = c(dnorm(x,0,sqrt(asympVar_LSE)),
                                   dnorm(x,0,sqrt(asympVar_MLE)),
                                   dnorm(x,0,sqrt(3*pi*GammaCalc(10000)/((1-spatialDelta)^3-spatialDelta^3)))),
                             group = rep(c("Kappa LSE", "Kappa Non Oracle Estimator", "Kappa Oracle Estimator"),each = length(x))
  )
  
  x2 <- seq(min(sqrt(m*n)*(subset(dat, group %in% c("Kappa Non Oracle Estimator", "Kappa Oracle Estimator"))$y-kappa_true)),
            max(sqrt(m*n)*(subset(dat, group %in% c("Kappa Non Oracle Estimator", "Kappa Oracle Estimator"))$y-kappa_true)),
            length.out = 80)
  dat_asympVar2 <- data.frame(x = rep(x2,3),
                              y = c(dnorm(x2,0,sqrt(asympVar_LSE)),
                                    dnorm(x2,0,sqrt(asympVar_MLE)),
                                    dnorm(x2,0,sqrt(3*pi*GammaCalc(10000)/(1-spatialDelta+spatialDelta^2)))),
                              group = rep(c("Kappa LSE", "Kappa Non Oracle Estimator", 
                                            "Kappa Oracle Estimator"),
                                          each = length(x2))
  )
  
  
  
  g1 <- ggplot(subset(dat,group %in% c("Kappa Non Oracle Estimator")), aes(x=sqrt(m*n)*(y-kappa_true),group=group,color = group,fill = group))+
    geom_density(alpha = alpha)+
    geom_density(data = subset(dat, group %in% c("Kappa LSE")),aes(x=sqrt(m*n)*(y-kappa_true),group=group,color = group,fill = group),alpha = alpha)+
    theme_minimal()+
    labs(y="",x="",fill="",color="",title= bquote("n" ==.(n)~", m" ==.(m)~", " ~kappa == .(kappa_true)~", " ~sigma[0]^2 == .(sigma_true)~", " ~delta == .(spatialDelta)))+
    theme(plot.title = element_text(size = fontSize),
          legend.position = "none",
          axis.text=element_text(size=fontSize))+
    scale_fill_manual(breaks =c("Kappa Non Oracle Estimator","Kappa LSE")  ,values = colorPair1)+
    scale_color_manual(breaks=c("Kappa Non Oracle Estimator","Kappa LSE") ,values = colorPair1)+
    geom_line(data = subset(dat_asympVar,group %in% c("Kappa LSE","Kappa Non Oracle Estimator")),aes(x=x,y=y,group = group,color=group),linetype = "dashed")
  
  g2 <- ggplot(subset(dat,group %in% c("Kappa Oracle Estimator","Kappa Non Oracle Estimator")), aes(x=sqrt(m*n)*(y-kappa_true),group=group,color = group,fill = group))+
    geom_density(alpha = alpha)+
    theme_minimal()+
    labs(y="",x="",fill="",color="",title= bquote("n" ==.(n)~", m" ==.(m)~", " ~kappa == .(kappa_true)~", " ~sigma[0]^2 == .(sigma_true)~", " ~delta == .(spatialDelta)))+
    theme(plot.title = element_text(size = fontSize),
          legend.position = "none",
          axis.text=element_text(size=fontSize))+
    scale_fill_manual(breaks =c("Kappa Non Oracle Estimator","Kappa Oracle Estimator")  ,values = colorPair2)+
    scale_color_manual(breaks=c("Kappa Non Oracle Estimator","Kappa Oracle Estimator") ,values = colorPair2)+
    geom_line(data = subset(dat_asympVar2,group %in% c("Kappa Oracle Estimator","Kappa Non Oracle Estimator")),aes(x=x,y=y,group = group,color=group),linetype="dashed")
  
  ggarrange(g1,g2,ncol=2,nrow=1)
})
ggarrange(plotlist = list_densityPlot_kappa1,ncol = 2,nrow = 2,common.legend = T,legend = "bottom")
ggarrange(plotlist = list_densityPlot_kappa1[1:2],ncol = 2,nrow = 1,common.legend = T,legend = "bottom")
ggarrange(plotlist = list_densityPlot_kappa1[3:4],ncol = 2,nrow = 1,common.legend = T,legend = "bottom")

cairo_ps(file = "EPS/dens+Asymp_kappa1.eps",width=20,height=11, fallback_resolution = 600,bg="transparent")
ggarrange(plotlist = list_densityPlot_kappa1,ncol = 2,nrow = 2,common.legend = T,legend = "bottom")
dev.off()

cairo_ps(file = "EPS/dens+Asymp_kappa2.eps",width=20,height=11, fallback_resolution = 600,bg="transparent")
ggarrange(plotlist = list_densityPlot_kappa1[1:2],ncol = 2,nrow = 1,common.legend = T,legend = "bottom")
dev.off()

cairo_ps(file = "EPS/dens+Asymp_kappa3.eps",width=20,height=11, fallback_resolution = 600,bg="transparent")
ggarrange(plotlist = list_densityPlot_kappa1[1:2],ncol = 2,nrow = 1,common.legend = T,legend = "bottom")
dev.off()



list_densityPlot_kappa2 <- lapply(select2, function(i){
  n <- temporal_cases[i]
  y <- seq(0,1,1/spatial_cases[i])
  yWithoutBounds <- y[which(y >= spatialDelta)[1] : (which( round(y,2) > round((timeHorizon - spatialDelta),2))[1]-1 )]
  m <- length(y)
  kappa_true <- theta1_cases[i]/theta2
  sigma_true <- (sigma_cases[i]/sqrt(theta2))^2
  dat <- readRDS(paste("Case Study Results Data/result_kappa_",cases[i],".RDS",sep=""))
  
  asympVar_LSE <- asympVariance_etaLSE(kappa_true,sigma_true,spatialDelta)[2,2]
  asympVar_MLE <- asympVariance_etaMLE(kappa_true,sigma_true,spatialDelta)[2,2]
  #min(sqrt(m*n)*(dat$y-kappa_true)) subset(dat, group %in% c("Kappa Non Oracle Estimator", "Kappa Oracle Estimator"))$y
  x <- seq(min(sqrt(m*n)*(subset(dat, group %in% c("Kappa LSE", "Kappa Non Oracle Estimator"))$y-kappa_true)),max(sqrt(m*n)*(subset(dat, group %in% c("Kappa LSE", "Kappa Non Oracle Estimator"))$y-kappa_true)),length.out = 80)
  dat_asympVar <- data.frame(x = rep(x,3),
                             y = c(dnorm(x,0,sqrt(asympVar_LSE)),
                                   dnorm(x,0,sqrt(asympVar_MLE)),
                                   dnorm(x,0,sqrt(3*pi*GammaCalc(10000)/((1-spatialDelta)^3-spatialDelta^3)))),
                             group = rep(c("Kappa LSE", "Kappa Non Oracle Estimator", "Kappa Oracle Estimator"),each = length(x))
                             
  )
  
  x2 <- seq(min(sqrt(m*n)*(subset(dat, group %in% c("Kappa Non Oracle Estimator", "Kappa Oracle Estimator"))$y-kappa_true)),max(sqrt(m*n)*(subset(dat, group %in% c("Kappa Non Oracle Estimator", "Kappa Oracle Estimator"))$y-kappa_true)),length.out = 80)
  dat_asympVar2 <- data.frame(x = rep(x2,3),
                              y = c(dnorm(x2,0,sqrt(asympVar_LSE)),
                                    dnorm(x2,0,sqrt(asympVar_MLE)),
                                    dnorm(x2,0,sqrt(3*pi*GammaCalc(10000)/(1-spatialDelta+spatialDelta^2)))),
                              group = rep(c("Kappa LSE", "Kappa Non Oracle Estimator", "Kappa Oracle Estimator"),each = length(x2))
  )
  
  g1 <- ggplot(subset(dat,group %in% c("Kappa Non Oracle Estimator")), aes(x=sqrt(m*n)*(y-kappa_true),group=group,color = group,fill = group))+
    geom_density(alpha = alpha)+
    geom_density(data = subset(dat, group %in% c("Kappa LSE")),aes(x=sqrt(m*n)*(y-kappa_true),group=group,color = group,fill = group),alpha = alpha)+
    theme_minimal()+
    labs(y="",x="",fill="",color="",title= bquote("n" ==.(n)~", m" ==.(m)~", " ~kappa == .(kappa_true)~", " ~sigma[0]^2 == .(sigma_true)~", " ~delta == .(spatialDelta)))+
    theme(plot.title = element_text(size = fontSize),
          legend.position = "none",
          axis.text=element_text(size=fontSize))+
    scale_fill_manual(breaks =c("Kappa Non Oracle Estimator","Kappa LSE")  ,values = colorPair1)+
    scale_color_manual(breaks=c("Kappa Non Oracle Estimator","Kappa LSE") ,values = colorPair1)+
    geom_line(data = subset(dat_asympVar,group %in% c("Kappa LSE","Kappa Non Oracle Estimator")),aes(x=x,y=y,group = group,color=group),linetype = "dashed")
  
  g2 <- ggplot(subset(dat,group %in% c("Kappa Oracle Estimator","Kappa Non Oracle Estimator")), aes(x=sqrt(m*n)*(y-kappa_true),group=group,color = group,fill = group))+
    geom_density(alpha = alpha)+
    theme_minimal()+
    labs(y="",x="",fill="",color="",title= bquote("n" ==.(n)~", m" ==.(m)~", " ~kappa == .(kappa_true)~", " ~sigma[0]^2 == .(sigma_true)~", " ~delta == .(spatialDelta)))+
    theme(plot.title = element_text(size = fontSize),
          legend.position = "none",
          axis.text=element_text(size=fontSize))+
    scale_fill_manual(breaks =c("Kappa Non Oracle Estimator","Kappa Oracle Estimator")  ,values = colorPair2)+
    scale_color_manual(breaks=c("Kappa Non Oracle Estimator","Kappa Oracle Estimator") ,values = colorPair2)+
    geom_line(data = subset(dat_asympVar2,group %in% c("Kappa Oracle Estimator","Kappa Non Oracle Estimator")),aes(x=x,y=y,group = group,color=group),linetype="dashed")
  
  ggarrange(g1,g2,ncol=2,nrow=1)
})
ggarrange(plotlist = list_densityPlot_kappa2,ncol = 2,nrow = 2,common.legend = T,legend = "bottom")
ggarrange(plotlist = list_densityPlot_kappa2[1:2],ncol = 2,nrow = 1,common.legend = T,legend = "bottom")
ggarrange(plotlist = list_densityPlot_kappa2[3:4],ncol = 2,nrow = 1,common.legend = T,legend = "bottom")

cairo_ps(file = "EPS/dens+Asymp_kappa4.eps",width=20,height=11, fallback_resolution = 600,bg="transparent")
ggarrange(plotlist = list_densityPlot_kappa2,ncol = 2,nrow = 2,common.legend = T,legend = "bottom")
dev.off()



cairo_ps(file = "EPS/dens+Asymp_kappa5.eps",width=20,height=11, fallback_resolution = 600,bg="transparent")
ggarrange(plotlist = list_densityPlot_kappa2[1:2],ncol = 2,nrow = 1,common.legend = T,legend = "bottom")
dev.off()



cairo_ps(file = "EPS/dens+Asymp_kappa6.eps",width=20,height=11, fallback_resolution = 600,bg="transparent")
ggarrange(plotlist = list_densityPlot_kappa2[3:4],ncol = 2,nrow = 1,common.legend = T,legend = "bottom")
dev.off()