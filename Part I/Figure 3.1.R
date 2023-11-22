kappa_grid <- seq(-5,5,1)
sigma_grid <- seq(0.1,2,1/100)
delta <- 0.05

erg <- c()
for(kappa in kappa_grid){
  
  
  l <- pbmclapply(sigma_grid, function(sigma){
    # print(paste("kappa:",kappa," sigma^2:",sigma^2))
    asympVariance_etaLSE(kappa = kappa,sigmaSquared = sigma^2,spatialDelta = delta)[1,1]
  },mc.cores = numCores)
  s1 <- unlist(l)
  l <- pbmclapply(sigma_grid, function(sigma){
    asympVariance_etaMLE(kappa = kappa,sigmaSquared = sigma^2,spatialDelta = delta)[1,1]
  },mc.cores = numCores)
  s2 <- unlist(l)
  erg <- c(erg,s1,s2)
}


dat <- data.frame(x=rep(sigma_grid,2*length(kappa_grid)),y=erg,group=as.factor(rep(1:(length(kappa_grid)),each=2*length(sigma_grid))),est = rep(c("LSE","MLE"),each=length(sigma_grid)))
cap <- 200
dat2 <- dat
dat2$y <- ifelse(dat$y <= cap,dat$y,NA) 
g1 <- ggplot(dat,aes(x=x,y=y,group=interaction(group,est),color=est))+
  geom_line()+
  theme_minimal()+
  labs(x=bquote(sigma^2),y="")+
  scale_color_manual(breaks = c("LSE","MLE"),values = adobeColorsDiscrete[c(3,1)])+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  facet_wrap(.~group,scales="free",ncol = 11)+
  scale_x_continuous(breaks = seq(0,2,1))+
  expand_limits(x = 0)+
  facetted_pos_scales(y=list(group == "3" ~ scale_y_continuous(breaks = c(300,900)),
                             group == "4" ~ scale_y_continuous(breaks = c(200,400)),
                             group == "5" ~ scale_y_continuous(breaks = c(100,250)),
                             group == "6" ~ scale_y_continuous(breaks = c(100,300)),
                             group == "7" ~ scale_y_continuous(breaks = c(100,200)),
                             group == "8" ~ scale_y_continuous(breaks = c(100,300)),
                             group == "9" ~ scale_y_continuous(breaks = c(200,400)),
                             group == "10" ~ scale_y_continuous(breaks = c(200,500)),
                             group == "11" ~ scale_y_continuous(breaks = c(200,600))))


g1
kappa_grid <- seq(-5,5,1/100)
sigma = 1


l <- pbmclapply(kappa_grid, function(kappa){
  asympVariance_etaMLE(kappa = kappa,sigmaSquared = sigma^2,spatialDelta = delta)[1,1]/asympVariance_etaLSE(kappa = kappa,sigmaSquared = sigma^2,spatialDelta = delta)[1,1]
},mc.cores = numCores)
s1 <- unlist(l)




dat2 <- data.frame(x=kappa_grid,y=s1)

g2 <- ggplot(dat2,aes(x=x,y=y,color=y))+
  geom_line()+
  geom_hline(yintercept = 1,linetype="dotted")+
  expand_limits(y=0)+
  theme_minimal()+
  labs(x=bquote(kappa),y="")+
  scale_color_gradientn(colours = darkmint[5:7])+
  theme(legend.position = "none")
g2


ggarrange(g1,NULL,g2,ncol=3,nrow = 1,widths = c(1.2,0.01, 0.5))