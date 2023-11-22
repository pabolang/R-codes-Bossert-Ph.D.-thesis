library(SecondOrderSPDEMulti)
dat <- readRDS("sigma_1.RDS")
scale <- 0.6
cp <- c(-2.8*scale, -1*scale, 1.7*scale)
plot_SPDEMulti(dat,1,spatialCoordsRemainingAxes = c(0.5,0.5), camerPosition = cp)
plot_SPDEMulti(dat,2,spatialCoordsRemainingAxes = c(0.5,0.5), camerPosition = cp)
plot_SPDEMulti(dat,3,spatialCoordsRemainingAxes = c(0.5,0.5), camerPosition = cp)