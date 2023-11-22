library(ggplot2)
library(ggpubr)
# read df1
ownColor2 <- tinaCol2 <- c("#323E40","#F2AB27","#BF6B04","#732002","#D95323","#2E5902","#4C6C73",
                           "#D9B88F","#BF9169","#735236","#D96262","#D9CCC1")
dat <- df1$data
datY <- df1$SG
param <- df1$param
alphaDash <- param["alphaDash"]
maxLag <- 15
yPoint <- c(0.2,0.5)


acfSPDEMulti <- function(dat,yIndex,maxLag){
  
  ind <- yIndex
  n <- dim(dat)[1]
  
  tf <- function(lag,alphaDash){
    return(-lag^alphaDash+0.5*(lag-1)^alphaDash+0.5*(lag+1)^alphaDash)
  }
  
  tf1 <- function(lag,alphaDash){
    a <- -lag^alphaDash+0.5*(lag-1)^alphaDash+0.5*(lag+1)^alphaDash
    b <- sigma^2/(n-1)^(alphaDash)*gamma(1-alphaDash)/(2^d*(pi*eta)^(d/2)*alphaDash*gamma(d/2))*exp(-(kappa[1]*yPoint[1]+kappa[2]*yPoint[2])/2)
    return(a*b)
  }
  
  
  x <- dat[ind,]
  x1 <- x[-1]
  x2 <- x[-n]
  inc <- x1-x2
  datlags <- stats::acf(inc,plot=F,lag.max = maxLag)
  
  df <- data.frame(x=1:maxLag,yTheo = tf(1:maxLag,alphaDash),y = datlags$acf[-1])
  return(df)
}

yp1 <- 13
yp2 <- 37
yp3 <- 61
yp4 <- 97
yp5 <- 109
acfSPDEMulti(dat,yp1,maxLag)

yACF <- c(acfSPDEMulti(dat,yIndex = yp1,maxLag = maxLag)$yTheo,
          acfSPDEMulti(dat,yIndex = yp1,maxLag = maxLag)$y,
          acfSPDEMulti(dat,yIndex = yp2,maxLag = maxLag)$y,
          acfSPDEMulti(dat,yIndex = yp3,maxLag = maxLag)$y,
          acfSPDEMulti(dat,yIndex = yp4,maxLag = maxLag)$y,
          acfSPDEMulti(dat,yIndex = yp5,maxLag = maxLag)$y)
acfDat <- data.frame(y = yACF,
                     group = as.factor(rep(1:6,each=maxLag)), x= 1:maxLag)


(acf1 <- ggplot(acfDat,aes(x=x,y=y,group=group,color=group))+
    geom_line(data=subset(acfDat,group==1),linetype = "dashed")+
    geom_line(data=subset(acfDat,group!=1))+
    theme_minimal()+
    labs(y="",x="Lags")+
    scale_x_continuous(breaks=seq(0,maxLag,by=2))+
    scale_color_manual(name = "", labels = c("Theoretical ACF","y=(0.1,0.1)", "y=(0.3,0.3)", "y=(0.5,0.5)","y=(0.7,0.7)","y=(0.9,0.9)"),
                       values = ownColor2[c(1:5,6)])+
    theme(legend.position="bottom",legend.text = element_text(size=11))
)

l <- lapply(31:50, function(i){
  df1 <- readRDS(paste("/Users/patrickbossert/Desktop/Dissertation/Multidim. SPDE/2D Data/Data Alpha Mid/dat",i,".RDS",sep=""))
  dat <- df1$data
  datY <- df1$SG
  param <- df1$param
  alphaDash <- param["alphaDash"]
  
  yACF <- c(acfSPDEMulti(dat,yIndex = yp1,maxLag = maxLag)$yTheo,
            acfSPDEMulti(dat,yIndex = yp1,maxLag = maxLag)$y,
            acfSPDEMulti(dat,yIndex = yp2,maxLag = maxLag)$y,
            acfSPDEMulti(dat,yIndex = yp3,maxLag = maxLag)$y,
            acfSPDEMulti(dat,yIndex = yp4,maxLag = maxLag)$y,
            acfSPDEMulti(dat,yIndex = yp5,maxLag = maxLag)$y)
  yACF
})

t <- do.call(rbind,l)
yACF <- colMeans(t)

acfDat <- data.frame(y = yACF,
                     group = as.factor(rep(1:6,each=maxLag)), x= 1:maxLag)


(acf2 <- ggplot(acfDat,aes(x=x,y=y,group=group,color=group))+
    geom_line(data=subset(acfDat,group==1),linetype = "dashed")+
    geom_line(data=subset(acfDat,group!=1))+
    theme_minimal()+
    labs(y="",x="Lags")+
    scale_x_continuous(breaks=seq(0,maxLag,by=2))+
    scale_color_manual(name = "", labels = c("Theoretical ACF","y=(0.1,0.1)", "y=(0.3,0.3)", "y=(0.5,0.5)","y=(0.7,0.7)","y=(0.9,0.9)"),
                       values = ownColor2[c(1:5,6)])+
    theme(legend.position="bottom",legend.text = element_text(size=11))
)
ggarrange(acf1,acf2,ncol = 2,common.legend = T,legend="bottom")