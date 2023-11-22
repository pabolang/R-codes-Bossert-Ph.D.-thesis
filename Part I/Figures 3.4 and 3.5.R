library(parallel)
library(ggplot2)
library(qqplotr)
library(ggpubr)
library(plotly)
library(pbapply)
library(plotly)
library(beepr)
library(pbmcapply)
library(reshape2)
library(qqplotr)
library(dplyr)

source("Help_Functions.r")
source("Color Palette.R")


iChoices <- c(2,3,5,6,8,9,11,12,14,15,17,18)
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

l11 <- lapply(select1, function(i){
  n <- temporal_cases[i]
  y <- seq(0,1,1/spatial_cases[i])
  yWithoutBounds <- y[which(y >= spatialDelta)[1] : (which( round(y,2) > round((timeHorizon - spatialDelta),2))[1]-1 )]
  m <- length(y)
  kappa_true <- theta1_cases[i]/theta2
  sigma_true <- (sigma_cases[i]/sqrt(theta2))^2
  delta <- 0.05
  
  dat <- readRDS(paste("Case Study Results Data/result_eta_",cases[i],".RDS",sep=""))
  dat_kappa <- subset(dat,parameter == "kappa")
  kappaLSE <- mean(subset(dat_kappa,group == "LSE")$y)
  kappaMLE <- mean(subset(dat_kappa,group == "MLE")$y)
  
  dat <- subset(dat,parameter == "sigma")
  sigmaLSE <- mean(subset(dat,group == "LSE")$y)
  sigmaMLE <- mean(subset(dat,group == "MLE")$y)
  val1 <- subset(dat, group == "MLE")$y
  val2 <- subset(dat, group == "LSE")$y
  
  
  
  s1 <- asympVariance_etaMLE(kappaMLE,sigmaMLE,delta)[1,1]
  s2 <- asympVariance_etaLSE(kappa = kappaLSE,sigmaSquared = sigmaLSE, delta)[1,1]
  
  rescaled1 <- sqrt(n*m)*(val1-sigmaMLE)/sqrt(s1)
  rescaled2 <- sqrt(n*m)*(val2-sigmaLSE)/sqrt(s2)
  
  
  dat$rescaled <- c(rescaled1,rescaled2)
  
  
  
  ggplot(dat,aes(sample=rescaled,group = group,color=group))+
    ggplot2::stat_qq() + 
    ggplot2::stat_qq_line()+
    theme_minimal()+
    scale_color_manual(breaks = c("LSE", "MLE"),values = adobeColorsDiscrete[c(3,1)])+
    # facet_grid(rows = vars(group),labeller = labeller(a = "Aa", b = "Bb", c = "Cc"))+
    labs(x="",y="",title= bquote("n" ==.(n)~", m" ==.(m)~", " ~kappa == .(kappa_true)~", " ~sigma[0]^2 == .(sigma_true)~", " ~delta == .(spatialDelta)))+
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text.y = element_blank(),
          plot.title = element_text(size = fontSize),
          axis.text=element_text(size=fontSize))+
    facet_grid(rows = vars(group),scales = "free")
  
  
})

ggarrange(plotlist = l11,ncol = 2,nrow = 2,common.legend = T,legend = "none")

cairo_ps(file = "EPS/qq_sigma1.eps",width=20,height=11, fallback_resolution = 600,bg="transparent")
ggarrange(plotlist = l11,ncol = 2,nrow = 2,common.legend = T,legend = "none")
dev.off()


l22 <- lapply(select2, function(i){
  n <- temporal_cases[i]
  y <- seq(0,1,1/spatial_cases[i])
  yWithoutBounds <- y[which(y >= spatialDelta)[1] : (which( round(y,2) > round((timeHorizon - spatialDelta),2))[1]-1 )]
  m <- length(y)
  kappa_true <- theta1_cases[i]/theta2
  sigma_true <- (sigma_cases[i]/sqrt(theta2))^2
  delta <- 0.05
  
  dat <- readRDS(paste("Case Study Results Data/result_eta_",cases[i],".RDS",sep=""))
  dat_kappa <- subset(dat,parameter == "kappa")
  kappaLSE <- mean(subset(dat_kappa,group == "LSE")$y)
  kappaMLE <- mean(subset(dat_kappa,group == "MLE")$y)
  
  dat <- subset(dat,parameter == "sigma")
  sigmaLSE <- mean(subset(dat,group == "LSE")$y)
  sigmaMLE <- mean(subset(dat,group == "MLE")$y)
  val1 <- subset(dat, group == "MLE")$y
  val2 <- subset(dat, group == "LSE")$y
  
  s1 <- asympVariance_etaMLE(kappaMLE,sigmaMLE,delta)[1,1]
  s2 <- asympVariance_etaLSE(kappa = kappaLSE,sigmaSquared = sigmaLSE, delta)[1,1]
  
  rescaled1 <- sqrt(n*m)*(val1-sigmaMLE)/sqrt(s1)
  rescaled2 <- sqrt(n*m)*(val2-sigmaLSE)/sqrt(s2)
  
  
  dat$rescaled <- c(rescaled1,rescaled2)
  
  
  
  ggplot(dat,aes(sample=rescaled,group = group,color=group))+
    ggplot2::stat_qq() + 
    ggplot2::stat_qq_line()+
    theme_minimal()+
    scale_color_manual(breaks = c("LSE", "MLE"),values = adobeColorsDiscrete[c(3,1)])+
    labs(x="",y="",title= bquote("n" ==.(n)~", m" ==.(m)~", " ~kappa == .(kappa_true)~", " ~sigma[0]^2 == .(sigma_true)~", " ~delta == .(spatialDelta)))+
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text.y = element_blank(),
          plot.title = element_text(size = fontSize),
          axis.text=element_text(size=fontSize))+
    facet_grid(rows = vars(group),scales = "free")
  
})

ggarrange(plotlist = l22,ncol = 2,nrow = 2,common.legend = T,legend = "none")



cairo_ps(file = "EPS/qq_sigma2.eps",width=20,height=11, fallback_resolution = 600,bg="transparent")
ggarrange(plotlist = l22,ncol = 2,nrow = 2,common.legend = T,legend = "none")
dev.off()