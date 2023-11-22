library(SecondOrderSPDEMulti)
library(ggpubr)

#Generate Data

d <- 2
theta0 <- 0
nu <- c(5,0)
eta <- 1
sigma <- 1
alphaDash <- 0.6
numberOfTemporalPoints <- 10^4
numberOfSpatialPoints <- 10
L <- 10
K <- 20


res20 <- simulateSPDEmodelMulti(
  d,theta0,nu,eta,sigma,alphaDash,
  numberOfSpatialPoints,numberOfTemporalPoints,
  L,K
)



K <- 100
res100 <- simulateSPDEmodelMulti(
  d,theta0,nu,eta,sigma,alphaDash,
  numberOfSpatialPoints,numberOfTemporalPoints,
  L,K
)

K <- 1500
res1000 <- simulateSPDEmodelMulti(
  d,theta0,nu,eta,sigma,alphaDash,
  numberOfSpatialPoints,numberOfTemporalPoints,
  L,K
)


# Plot
dat20 <- readRDS("K20_1.RDS")
dat100 <- readRDS("K100_1.RDS")
dat1000 <- readRDS("K1000_1.RDS")

res20 <- RV_plot(dat20)
res100 <- RV_plot(dat100)
res1000 <- RV_plot(dat1000)



mean(res20$deviation[,1])
mean(res100$deviation[,1])
mean(res1000$deviation[,1])


rgb2 <- function(r,g,b){
  return(rgb(r/255,g/255,b/255))
}
darkmint <- c( rgb2(123, 188, 176), rgb2(85, 156, 158), rgb2(58, 124, 137), rgb2(35, 93, 114), rgb2(18, 63, 90))
adobeColorsDiscrete <- c("#323E40","#F2AB27","#BF6B04","#732002","#D95323","#254021","#2E5902","#024873")



dat1 <- res20$dat
dat2 <- res100$data
dat3 <- res1000$data
dat <- rbind(dat1,dat2,dat3)
dat$index <- as.factor(rep(1:3,each=dim(dat1)[1]))
(p1 <- ggplot(dat,aes(x=x,y=y,group=group,color = group))+geom_line()+
    theme_minimal()+labs(x="Index of filtered spatial grid",y="Rescaled realized volatility",color=NULL)+
    theme(legend.position = "bottom")+
    scale_colour_manual(values=adobeColorsDiscrete)+
    facet_wrap(~index,labeller = as_labeller(
      c("1"="K=20","2"="K=100","3"="K=1500")),ncol=3,nrow=1))


dat1 <- res20$deviation
dat2 <- res100$deviation
dat3 <- res1000$deviation
dat <- rbind(dat1,dat2,dat3)
dat$index2 <- as.factor(rep(1:3,each=dim(dat1)[1]))

(p2 <- ggplot(dat,aes(x=index,y=deviation,color=deviation))+geom_line()+
    theme_minimal()+labs(x="Index of filtered spatial grid",y="Deviation of theoretical and empirical results")+
    theme(legend.position = "none")+
    scale_colour_gradientn(colours = darkmint)+
    facet_wrap(~index2,labeller = as_labeller(
      c("1"="K=20","2"="K=100","3"="K=1500")),ncol=3,nrow=1))

ggarrange(p1,p2,ncol=1)