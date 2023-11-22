library(SecondOrderSPDEMulti)
library(pbmcapply)
library(ggplot2)
library(ggpubr)

fontSize <- 14

path1 <- "path_low_data"
end <- 1000
numCores <- detectCores()-1
dat <- readRDS(paste(path1,"/dat1.RDS",sep=""))
SG <- dat$SG
param <- dat$param
d <- param["d"]
theta0 <- param["theta0"]
nu <- param[3:(3+d-1)]
eta <- param["eta"]
sigma <- param["sigma"]
sigma_0_squared <- sigma^2/(eta^(d/2))
alphaDash <- param["alphaDash"]
M <- param["numberSP"]
N <- param["numberTP"]
kappa <- nu/eta
Upsilon <- Upsilon(alphaDash = alphaDash,d = d)
trans <- 0.5
indexset <- c(15,47,83)
Mf <- length(indexset)

det(as.matrix(cbind(c(1,1,1),SG[indexset,])))

l1 <- pbmclapply(1:end,function(i){
  dat <- readRDS(paste(path1,"/dat",i,".RDS",sep=""))
  estimateParametersSPDEMulti(dat,estimationMethod="SigmaAndKappa",alphaDash = alphaDash,indexset = indexset)
},mc.cores = numCores)
res <- do.call(rbind,l1)
res1 <- res[,1]
res2 <- res[,2]
res3 <- res[,3]
param_true <- c(mean(res1),mean(res2),mean(res3))

x1_1 <- sqrt(N*Mf)*(res1-param_true[1])
x1_2 <- sqrt(N*Mf)*(res2-param_true[2])
x1_3 <- sqrt(N*Mf)*(res3-param_true[3])

x2_1 <- seq(min(x1_1),max(x1_1),1/1000)
x2_2 <- seq(min(x1_2),max(x1_2),1/1000)
x2_3 <- seq(min(x1_3),max(x1_3),1/1000)

dat_L_1 <- data.frame(x=c(x1_1,x1_2,x1_3),group=as.factor(c(rep(1,length(x1_1)),rep(2,length(x1_2)),rep(3,length(x1_3)))))

av <- asymp_var("SigmaAndKappa",d,alphaDash=alphaDash,sigma_0_squared=sigma_0_squared,indexset = indexset,SG = SG)
av1 <- sqrt(av[1,1])
av2 <- sqrt(av[2,2])
av3 <- sqrt(av[3,3])
dat_L2_1 <- data.frame(x=c(x2_1,x2_2,x2_3),y=c(dnorm(x2_1,0,av1),dnorm(x2_2,0,av2),dnorm(x2_3,0,av3)),
                       group= as.factor(c(rep(1,length(x2_1)),rep(2,length(x2_2)),rep(3,length(x2_3)))))

dat_qq_1 <- data.frame(x=c(x1_1,x1_2,x1_3)/c(av1,av2,av3),group = as.factor(rep(1:3,each = length(x1_1))))
dat_qq_1$group2 <- as.factor(1)

(g1 <- ggplot(dat_L_1,aes(x=x,group=group,fill=group,color=group))+geom_density(alpha=0.5)+
    scale_fill_manual(values = adobeColorsDiscrete[1:3],labels = c(bquote(sigma[0]^2),bquote(kappa[1]),bquote(kappa[2])))+
    geom_line(data = dat_L2_1, aes(x=x,y=y,group=group,color=group),linetype = "dashed")+
    scale_color_manual(values = adobeColorsDiscrete[1:3],labels = c(bquote(sigma[0]^2),bquote(kappa[1]),bquote(kappa[2])) )+
    facet_wrap(~group,scales = "free_x",labeller = as_labeller(
      c("1"="","2"="","3"="")))+
    theme_minimal()+
    labs(x="",y="",fill="",color="",title = bquote(alpha == .(alphaDash)))+
    theme(legend.position = "bottom",
          plot.title = element_text(size = fontSize,hjust = 0.5),
          axis.text=element_text(size=fontSize),
          legend.text=element_text(size=fontSize)))




path1 <- "path_mid_data"
dat <- readRDS(paste(path1,"/dat1.RDS",sep=""))
SG <- dat$SG
param <- dat$param
d <- param["d"]
theta0 <- param["theta0"]
nu <- param[3:(3+d-1)]
eta <- param["eta"]
sigma <- param["sigma"]
sigma_0_squared <- sigma^2/(eta^(d/2))
alphaDash <- param["alphaDash"]
M <- param["numberSP"]
N <- param["numberTP"]
kappa <- nu/eta
Upsilon <- Upsilon(alphaDash = alphaDash,d = d)
trans <- 0.5
indexset <- c(15,47,83)
Mf <- length(indexset)

l1 <- pbmclapply(1:end,function(i){
  dat <- readRDS(paste(path1,"/dat",i,".RDS",sep=""))
  estimateParametersSPDEMulti(dat,estimationMethod="SigmaAndKappa",alphaDash = alphaDash,indexset = indexset)
},mc.cores = numCores)
res <- do.call(rbind,l1)
res1 <- res[,1]
res2 <- res[,2]
res3 <- res[,3]
param_true <- c(mean(res1),mean(res2),mean(res3))

x1_1 <- sqrt(N*Mf)*(res1-param_true[1])
x1_2 <- sqrt(N*Mf)*(res2-param_true[2])
x1_3 <- sqrt(N*Mf)*(res3-param_true[3])

x2_1 <- seq(min(x1_1),max(x1_1),1/1000)
x2_2 <- seq(min(x1_2),max(x1_2),1/1000)
x2_3 <- seq(min(x1_3),max(x1_3),1/1000)

dat_L_2 <- data.frame(x=c(x1_1,x1_2,x1_3),group=as.factor(c(rep(1,length(x1_1)),rep(2,length(x1_2)),rep(3,length(x1_3)))))

av <- asymp_var("SigmaAndKappa",d,alphaDash=alphaDash,sigma_0_squared=sigma_0_squared,indexset = indexset,SG = SG)
av1 <- sqrt(av[1,1])
av2 <- sqrt(av[2,2])
av3 <- sqrt(av[3,3])
dat_L2_2 <- data.frame(x=c(x2_1,x2_2,x2_3),y=c(dnorm(x2_1,0,av1),dnorm(x2_2,0,av2),dnorm(x2_3,0,av3)),
                       group= as.factor(c(rep(1,length(x2_1)),rep(2,length(x2_2)),rep(3,length(x2_3)))))

dat_qq_2 <- data.frame(x=c(x1_1,x1_2,x1_3)/c(av1,av2,av3),group = as.factor(rep(1:3,each = length(x1_1))))
dat_qq_2$group2 <- as.factor(2)

(g2 <- ggplot(dat_L_2,aes(x=x,group=group,fill=group,color=group))+geom_density(alpha=0.5)+
    scale_fill_manual(values = adobeColorsDiscrete[1:3],labels = c(bquote(sigma[0]^2),bquote(kappa[1]),bquote(kappa[2])))+
    geom_line(data = dat_L2_2, aes(x=x,y=y,group=group,color=group),linetype = "dashed")+
    scale_color_manual(values = adobeColorsDiscrete[1:3],labels = c(bquote(sigma[0]^2),bquote(kappa[1]),bquote(kappa[2])) )+
    facet_wrap(~group,scales = "free_x",labeller = as_labeller(
      c("1"="","2"="","3"="")))+
    theme_minimal()+
    labs(x="",y="",fill="",color="",title = bquote(alpha == .(alphaDash)))+
    theme(legend.position = "bottom",
          plot.title = element_text(size = fontSize,hjust = 0.5),
          axis.text=element_text(size=fontSize),
          legend.text=element_text(size=fontSize)))


path1 <- "path_high_data"
dat <- readRDS(paste(path1,"/dat1.RDS",sep=""))
SG <- dat$SG
param <- dat$param
d <- param["d"]
theta0 <- param["theta0"]
nu <- param[3:(3+d-1)]
eta <- param["eta"]
sigma <- param["sigma"]
sigma_0_squared <- sigma^2/(eta^(d/2))
alphaDash <- param["alphaDash"]
M <- param["numberSP"]
N <- param["numberTP"]
kappa <- nu/eta
Upsilon <- Upsilon(alphaDash = alphaDash,d = d)
trans <- 0.5
indexset <- c(15,47,83)
Mf <- length(indexset)

l1 <- pbmclapply(1:end,function(i){
  dat <- readRDS(paste(path1,"/dat",i,".RDS",sep=""))
  estimateParametersSPDEMulti(dat,estimationMethod="SigmaAndKappa",alphaDash = alphaDash,indexset = indexset,ignoreWarnings = T)
},mc.cores = numCores)
res <- do.call(rbind,l1)
res1 <- res[,1]
res2 <- res[,2]
res3 <- res[,3]
param_true <- c(mean(res1),mean(res2),mean(res3))

x1_1 <- sqrt(N*Mf)*(res1-param_true[1])
x1_2 <- sqrt(N*Mf)*(res2-param_true[2])
x1_3 <- sqrt(N*Mf)*(res3-param_true[3])

x2_1 <- seq(min(x1_1),max(x1_1),1/1000)
x2_2 <- seq(min(x1_2),max(x1_2),1/1000)
x2_3 <- seq(min(x1_3),max(x1_3),1/1000)

dat_L_3 <- data.frame(x=c(x1_1,x1_2,x1_3),group=as.factor(c(rep(1,length(x1_1)),rep(2,length(x1_2)),rep(3,length(x1_3)))))

av <- asymp_var("SigmaAndKappa",d,alphaDash=alphaDash,sigma_0_squared=sigma_0_squared,indexset = indexset,SG = SG)
av1 <- sqrt(av[1,1])
av2 <- sqrt(av[2,2])
av3 <- sqrt(av[3,3])
dat_L2_3 <- data.frame(x=c(x2_1,x2_2,x2_3),y=c(dnorm(x2_1,0,av1),dnorm(x2_2,0,av2),dnorm(x2_3,0,av3)),
                       group= as.factor(c(rep(1,length(x2_1)),rep(2,length(x2_2)),rep(3,length(x2_3)))))

dat_qq_3 <- data.frame(x=c(x1_1,x1_2,x1_3)/c(av1,av2,av3),group = as.factor(rep(1:3,each = length(x1_1))))
dat_qq_3$group2 <- as.factor(3)


(g3 <- ggplot(dat_L_3,aes(x=x,group=group,fill=group,color=group))+geom_density(alpha=0.5)+
    scale_fill_manual(values = adobeColorsDiscrete[1:3],labels = c(bquote(sigma[0]^2),bquote(kappa[1]),bquote(kappa[2])))+
    geom_line(data = dat_L2_3, aes(x=x,y=y,group=group,color=group),linetype = "dashed")+
    scale_color_manual(values = adobeColorsDiscrete[1:3],labels = c(bquote(sigma[0]^2),bquote(kappa[1]),bquote(kappa[2])) )+
    facet_wrap(~group,scales = "free_x",labeller = as_labeller(
      c("1"="","2"="","3"="")))+
    theme_minimal()+
    labs(x="",y="",fill="",color="",title = bquote(alpha == .(alphaDash)))+
    theme(legend.position = "bottom",
          plot.title = element_text(size = fontSize,hjust = 0.5),
          axis.text=element_text(size=fontSize),
          legend.text=element_text(size=fontSize)))

ggarrange(g1,g2,g3, nrow = 3,common.legend = T,legend = "bottom")




# qq-Plot
dat_qq <- rbind(dat_qq_1,dat_qq_2,dat_qq_3)

(g11 <- ggplot(dat_qq_1,aes(sample=x,group = group,color=group))+
    ggplot2::stat_qq() + 
    ggplot2::stat_qq_line()+
    theme_minimal()+
    facet_wrap(~group,labeller = as_labeller(
      c("1"="","2"="","3"="")))+
    scale_color_manual(values = adobeColorsDiscrete[1:3],labels = c(bquote(sigma[0]^2),bquote(kappa[1]),bquote(kappa[2])) )+
    labs(x="",y="",fill="",color="",title = bquote(alpha == 0.4))+
    theme(legend.position = "bottom",
          plot.title = element_text(size = fontSize,hjust = 0.5),
          axis.text=element_text(size=fontSize),
          legend.text=element_text(size=fontSize)))

(g22 <- ggplot(dat_qq_2,aes(sample=x,group = group,color=group))+
    ggplot2::stat_qq() + 
    ggplot2::stat_qq_line()+
    theme_minimal()+
    facet_wrap(~group,labeller = as_labeller(
      c("1"="","2"="","3"="")))+
    scale_color_manual(values = adobeColorsDiscrete[1:3],labels = c(bquote(sigma[0]^2),bquote(kappa[1]),bquote(kappa[2])) )+
    labs(x="",y="",fill="",color="",title = bquote(alpha == 0.5))+
    theme(legend.position = "bottom",
          plot.title = element_text(size = fontSize,hjust = 0.5),
          axis.text=element_text(size=fontSize),
          legend.text=element_text(size=fontSize)))

(g33 <- ggplot(dat_qq_1,aes(sample=x,group = group,color=group))+
    ggplot2::stat_qq() + 
    ggplot2::stat_qq_line()+
    theme_minimal()+
    facet_wrap(~group,labeller = as_labeller(
      c("1"="","2"="","3"="")))+
    scale_color_manual(values = adobeColorsDiscrete[1:3],labels = c(bquote(sigma[0]^2),bquote(kappa[1]),bquote(kappa[2])) )+
    labs(x="",y="",fill="",color="",title = bquote(alpha == 0.6))+
    theme(legend.position = "bottom",
          plot.title = element_text(size = fontSize,hjust = 0.5),
          axis.text=element_text(size=fontSize),
          legend.text=element_text(size=fontSize)))

ggarrange(g11,g22,g33, nrow = 3,common.legend = T,legend = "bottom")