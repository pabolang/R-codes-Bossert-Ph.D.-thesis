# deterministic version
det_HK <- function(K,M,N,xi){
  y <- seq(0,1,1/M)
  t <- seq(0,1,1/N)
  e_k <- function(k,y){
    return(sqrt(2)*sin(pi*k*y))
  }
  l <- pbmclapply(1:K,function(k){
    integrate(function(y){xi(y)*e_k(k,y)},0,1,subdivisions=2000)$value
  },mc.cores = numCores)
  x0_data <- unlist(l)
  
  l1 <- pbmclapply(1:(M+1), function(j){
    l2 <- lapply(1:(N+1), function(i){
      l3 <- lapply(1:K, function(k){
        if(round(e_k(k,y[j]),14)==0){0}
        else {
          exp(-pi^2*k^2*t[i])*e_k(k,y[j])*x0_data[k]
        }
      })
      sum(unlist(l3))
    })
    unlist(l2)
  },mc.cores = numCores)
  return(list(data=do.call(rbind,l1),y=y,t=t))
}

K <- 1000
M <- 200
N <- 200
xi <- function(y){4*(-y^2+y)}
HK <- det_HK(K,M,N,xi)
y <- HK$t[1:61]
x <- HK$y
z <- HK$data[,1:61]
alpha <- 0.85

rgb2 <- function(r,g,b){
  return(rgb(r/255,g/255,b/255))
}

darkmint <- c(rgb2(210, 251, 212), rgb2(165, 219, 194), rgb2(123, 188, 176), rgb2(85, 156, 158), rgb2(58, 124, 137), rgb2(35, 93, 114), rgb2(18, 63, 90))
reverseColorSheme <- function(colors){
  n <- length(colors)
  return(colors[n:1])
}
darkmint2 <- reverseColorSheme(darkmint)

scale <- 1.25
(p <- plot_ly(x = ~y, y = ~x, z = ~z,width = 1000, height = 1000) %>% 
    add_surface(contours = list(z = list(
      show=TRUE,
      usecolormap=T,
      project=list(z=TRUE)
    )
    ),
    colorscale = list(seq(0,1,length.out = length(darkmint2)),darkmint2),
    lighting = list(diffuse = 3)) %>% 
    layout(scene = list(aspectmode='cube',
                        camera = list(eye = list(x = -1.3*scale, y = -1.3*scale, z = 1.3/2.5*scale)),
                        xaxis = list(title = 'Time'),
                        yaxis = list(title = 'Space'),
                        zaxis = list(title = 'Temperature')
    )
    )%>% 
    hide_colorbar()
)

# Stochastic version
theta0 = 0
theta1 = 0
theta2 = 1
sigma = 0.25
numberSpatialPoints = 200
numberTemporalPoints = 200
xi <- function(y){4*(-y^2+y)}

SHK <- simulateSPDEmodel(theta0,theta1,theta2,sigma,numberSpatialPoints,numberTemporalPoints,xi=xi,method = "cutoff")



(p2 <- plot_ly(x = ~x, y = ~y, z = ~SHK,width = 1000, height = 1000) %>% 
    add_surface(contours = list(z = list(
      show=TRUE,
      usecolormap=T,
      project=list(z=TRUE)
    )
    ),
    colorscale = list(seq(0,1,length.out = length(darkmint2)),darkmint2),
    lighting = list(diffuse = 3)) %>% 
    layout(scene = list(aspectmode='cube',
                        camera = list(eye = list(x = 1.3*scale, y = -1.3*scale, z = 1.3*scale/2.5)),
                        xaxis = list(title = 'Space'),
                        yaxis = list(title = 'Time'),
                        zaxis = list(title = 'Temperature')
    ))%>% 
    hide_colorbar()
)