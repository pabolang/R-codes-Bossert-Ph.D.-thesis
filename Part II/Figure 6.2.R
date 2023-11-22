library(SecondOrderSPDEMulti)
library(pbmcapply)
library(ggplot2)


path1 <- "path_low_data"
end <- 1000
numCores <- detectCores()-1
dat <- readRDS(paste(path1,"/dat1.RDS",sep=""))
param <- dat$param
d <- param["d"]
theta0 <- param["theta0"]
nu <- param[3:(3+d-1)]
eta <- param["eta"]
sigma <- param["sigma"]
alphaDash <- param["alphaDash"]
M <- param["numberSP"]
N <- param["numberTP"]
kappa <- nu/eta
Upsilon <- Upsilon(alphaDash = alphaDash,d = d)
trans <- 0.5
Mf <- length(estimateParametersSPDEMulti(dat,estimationMethod="alphaDash")$filteredSGIndices)

l1 <- pbmclapply(1:end,function(i){
  dat <- readRDS(paste(path1,"/dat",i,".RDS",sep=""))
  estimateParametersSPDEMulti(dat,estimationMethod="alphaDash")$estimate
},mc.cores = numCores)
res <- unlist(l1)
alpha_true <- mean(res)
x2 <- seq(min(sqrt(N*Mf)*(res-alpha_true)),max(sqrt(N*Mf)*(res-alpha_true)),1/1000)
dat_L <- data.frame(x=sqrt(N*Mf)*(res-alpha_true))
v <- 1/(log(2)^2)*(Upsilon*(3-2^(2-alphaDash))-2^(2-alphaDash)*Lambda(alphaDash))
dat_L2 <- data.frame(x=x2,y=dnorm(x2,0,sqrt(v)))



path1 <- "path_mid_data"
dat <- readRDS(paste(path1,"/dat1.RDS",sep=""))
param <- dat$param
d <- param["d"]
theta0 <- param["theta0"]
nu <- param[3:(3+d-1)]
eta <- param["eta"]
sigma <- param["sigma"]
alphaDash <- param["alphaDash"]
M <- param["numberSP"]
N <- param["numberTP"]
kappa <- nu/eta
Upsilon <- Upsilon(alphaDash = alphaDash,d = d)
trans <- 0.5
Mf <- length(estimateParametersSPDEMulti(dat,estimationMethod="alphaDash")$filteredSGIndices)


l1 <- pbmclapply(1:end,function(i){
  dat <- readRDS(paste(path1,"/dat",i,".RDS",sep=""))
  estimateParametersSPDEMulti(dat,estimationMethod="alphaDash")$estimate
},mc.cores = numCores)
res <- unlist(l1)
alpha_true <- mean(res)
x2 <- seq(min(sqrt(N*Mf)*(res-alpha_true)),max(sqrt(N*Mf)*(res-alpha_true)),1/1000)
dat_M <- data.frame(x=sqrt(N*Mf)*(res-alpha_true))
v <- 1/(log(2)^2)*(Upsilon*(3-2^(2-alphaDash))-2^(2-alphaDash)*Lambda(alphaDash))
dat_M2 <- data.frame(x=x2,y=dnorm(x2,0,sqrt(v)))




path1 <- "path_high_data"
dat <- readRDS(paste(path1,"/dat1.RDS",sep=""))
param <- dat$param
d <- param["d"]
theta0 <- param["theta0"]
nu <- param[3:(3+d-1)]
eta <- param["eta"]
sigma <- param["sigma"]
alphaDash <- param["alphaDash"]
M <- param["numberSP"]
N <- param["numberTP"]
kappa <- nu/eta
Upsilon <- Upsilon(alphaDash = alphaDash,d = d)
trans <- 0.5
Mf <- length(estimateParametersSPDEMulti(dat,estimationMethod="alphaDash")$filteredSGIndices)


l1 <- pbmclapply(1:end,function(i){
  dat <- readRDS(paste(path1,"/dat",i,".RDS",sep=""))
  estimateParametersSPDEMulti(dat,estimationMethod="alphaDash")$estimate
},mc.cores = numCores)
res <- unlist(l1)
alpha_true <- mean(res)
x2 <- seq(min(sqrt(N*Mf)*(res-alpha_true)),max(sqrt(N*Mf)*(res-alpha_true)),1/1000)
dat_H <- data.frame(x=sqrt(N*Mf)*(res-alpha_true))
v <- 1/(log(2)^2)*(Upsilon*(3-2^(2-alphaDash))-2^(2-alphaDash)*Lambda(alphaDash))
dat_H2 <- data.frame(x=x2,y=dnorm(x2,0,sqrt(v)))


dat1 <- rbind(dat_L,dat_M,dat_H)
dat1$group <- as.factor(c(rep(1,length(dat_L$x)),rep(2,length(dat_M$x)),rep(3,length(dat_H$x))))
dat2 <- rbind(dat_L2,dat_M2,dat_H2)
dat2$group <- as.factor(c(rep(1,length(dat_L2$x)),rep(2,length(dat_M2$x)),rep(3,length(dat_H2$x))))

ggplot(dat1,aes(x=x,group=group,fill=group,color=group))+geom_density(alpha=0.5)+
  scale_fill_manual(values = adobeColorsDiscrete[1:3],labels = c(bquote(alpha == 0.4),bquote(alpha == 0.5),bquote(alpha == 0.6)))+
  geom_line(data = dat2, aes(x=x,y=y,group=group,color=group),linetype = "dashed")+
  scale_color_manual(values = adobeColorsDiscrete[1:3],labels = c(bquote(alpha == 0.4),bquote(alpha == 0.5),bquote(alpha == 0.6)) )+
  facet_wrap(~group,labeller = as_labeller(
    c("1"="","2"="","3"="")))+
  theme_minimal()+
  labs(x="",y="",fill="",color="")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = fontSize,hjust = 0.5),
        axis.text=element_text(size=fontSize),
        legend.text=element_text(size=fontSize))


# qq-Plot
Upsilon1 <- Upsilon(alphaDash = 0.4,d = d)
Lambda1 <- Lambda(alphaDash = 0.4)
Upsilon2 <- Upsilon(alphaDash = 0.5,d = d)
Lambda2 <- Lambda(alphaDash = 0.5)
Upsilon3 <- Upsilon(alphaDash = 0.6,d = d)
Lambda3 <- Lambda(alphaDash = 0.6)
rescale <- c(rep((3*Upsilon1-2^(2-0.4)*(Upsilon1+Lambda1))/(log(2)^2),1000),
             rep((3*Upsilon2-2^(2-0.4)*(Upsilon2+Lambda2))/(log(2)^2),1000),
             rep((3*Upsilon3-2^(2-0.4)*(Upsilon3+Lambda3))/(log(2)^2),1000))
dat_qq <- dat1
dat_qq$x <- dat1$x/sqrt(rescale)

ggplot(dat_qq,aes(sample=x,group = group,color=group))+
  ggplot2::stat_qq() + 
  ggplot2::stat_qq_line()+
  theme_minimal()+
  facet_wrap(~group,labeller = as_labeller(
    c("1"="","2"="","3"="")))+
  scale_color_manual(values = adobeColorsDiscrete[1:3],labels = c(bquote(alpha == 0.4),bquote(alpha == 0.5),bquote(alpha == 0.6)) )+
  labs(x="",y="",fill="",color="")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = fontSize,hjust = 0.5),
        axis.text=element_text(size=fontSize),
        legend.text=element_text(size=fontSize))