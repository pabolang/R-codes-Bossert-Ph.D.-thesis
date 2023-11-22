fontSize <- 10


kappa_grid <- seq(-5,5,1/1000)
sigma_grid <- 1
delta <- 0.05

l <- pbmclapply(kappa_grid, function(kappa){
  asympVariance_etaLSE(kappa = kappa,sigmaSquared = sigma_grid,spatialDelta = delta)[2,2]
},mc.cores = numCores)
erg <- unlist(l)
s1 <- rep(asympVariance_etaMLE(kappa = kappa,sigmaSquared = sigma_grid,spatialDelta = delta)[2,2],length(kappa_grid))
s2 <- rep(3*GammaCalc(10000)*pi/(1-delta+delta^2),length(kappa_grid))
dat <- data.frame(x=rep(kappa_grid,3),y=c(erg,s1,s2),group=rep(c("LSE","MLE","Oracle"),each=length(kappa_grid)))
g1 <- ggplot(dat,aes(x=x,y=y,group=group,color=group))+
  geom_line()+
  theme_minimal()+
  labs(x=bquote(kappa),y="")+
  scale_color_manual(breaks = c("LSE","MLE","Oracle"),values = adobeColorsDiscrete[c(3,1,2)])+
  theme(legend.position = "none",
        #axis.text=element_text(size=fontSize),
        #axis.title = element_text(size=fontSize),
        #plot.title = element_text(size=fontSize)
  )

erg2 <- s1/erg
dat2 <- data.frame(x=kappa_grid,y=erg2)
g2 <- ggplot(dat2,aes(x=x,y=y,color=y))+
  geom_line()+
  geom_hline(yintercept = 1,linetype="dotted")+
  expand_limits(y=0)+
  theme_minimal()+
  labs(x=bquote(kappa),y="")+
  scale_color_gradientn(colours = darkmint[5:7])+
  theme(legend.position = "none",
        #axis.text=element_text(size=fontSize),
        #axis.title = element_text(size=fontSize),
        #plot.title = element_text(size=fontSize)
  )

ggarrange(g1,g2,ncol=2)