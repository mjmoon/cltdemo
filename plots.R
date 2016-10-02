#############################################################
# cltdemo - visualize central limit theorem by michael moon #
#                                                           #
# Sample generation and plotting                            #
#############################################################
library(RColorBrewer)
palette(brewer.pal(8, "Set2"))
fillcol <- apply(col2rgb(palette()[1])/255, 2, function(x) rgb(x[1], x[2], x[3], 0.5))

gendt <- function(n, niter, parm1 = 0, parm2 = 1, dist = 1){
  if(dist == 1)
    dt <- matrix(rnorm(niter*n, parm1, parm2), nrow = niter)
  else if(dist == 2)
    dt <- matrix(rbeta(niter*n, parm1, parm2), nrow = niter)
  else if(dist == 3)
    dt <- matrix(rbinom(niter*n, parm1, parm2), nrow = niter)
  apply(dt, 1, mean)
}

plotdt <- function(dt, parms, n, dist = 1){
  par(mar = c(4,0,0,0))
  if(dist == 1) {
    mu <- parms[1]
    sigma <- parms[2]
    ats <- c(mu - 5*sigma/sqrt(n), mu, mu + 5*sigma/sqrt(n))
  } else if(dist == 2) {
    mu <- parms[1]/sum(parms)
    sigma <- sqrt(prod(parms)/(sum(parms) + 1))/sum(parms)
  } else if(dist == 3) {
    mu <- prod(parms)
    sigma <- sqrt(mu*(1 - parms[2]))
  }
  ats <- c(mu - 6*sigma/sqrt(n), mu, mu + 6*sigma/sqrt(n))
  x <- seq(ats[1], ats[3], sigma/(sqrt(n)*1000))
  dena <- dnorm(x, mu, sigma/sqrt(n))
  dendt <- density(dt)
  ylims <- c(0, max(dena)*1.2)
  
  # plot asymptotic density
  plot(x, dena, col = 2, lty = 3, type = "l",
       ylim = ylims, axes = FALSE, ylab = "", xlab = "", main = "")
  # plot sample kernel density
  lines(dendt, col = 3)
  segments(mu, 0, mu, dnorm(mu, mu, sigma/sqrt(n)), col = 4, lty = 2)
  # plot sample points
  points(dt, jitter(rep(ylims[2]/3, length(dt)), factor = 15), 
         col = fillcol, pch = 16)
  legend("topleft", 
         legend = c(expression(paste("Sample Means, ", bold(bar(x)))), 
                    "Kernel Density", 
                    expression(paste("Asymptotic Density, N(", mu, ", ", sigma^2, "/n)")),
                    expression(paste("Asymptotic Mean, ", mu))), 
         col = c(fillcol, 3, 2, 4), lwd = 2,
         lty = c(NA, 1, 3, 2), pch = c(16, NA, NA, NA), bty = 'n')
  axis(1, at = ats, labels = round(ats, 2))
}

plotqq <- function(dt, parms, dist = 1){
  par(mar = c(4,4,0,0), las = 1)
  if(dist == 1) {
    mu <- parms[1]
  } else if(dist == 2) {
    mu <- parms[1]/sum(parms)
  } else if(dist == 3) {
    mu <- prod(parms)
  }
  qplot <- qqnorm(dt, col = fillcol, pch = 16, 
         axes = FALSE, main = "" )
  qqline(dt, col = 2)
  axis(1, at = c(min(qplot$x), 0, max(qplot$x)),
       labels = c(NA, 0, NA))
  axis(2, at = c(min(qplot$y), mu, max(qplot$y)), 
       labels = c(NA, expression(mu), NA))
}

plotdist <- function(dist, parms){
  par(mar = c(4,0,0,0))
  if(dist == 1) {
    x <- seq(-3, 3, 0.01)
    plot(x, dnorm(x, parms[1], parms[2]), type = "l",
         col = 2, xlab = "", ylab = "", axes = FALSE)
    axis(1, at = c(-3, parms[1], 3))  
    legend("topleft", expression(paste("N(", mu, ", ", sigma^2, ")")), bty = 'n')
  } else if(dist == 2) {
    x <- seq(0, 1, 0.001)
    plot(x, dbeta(x, parms[1], parms[2]), type = "l",
         col = 2, xlab = "", ylab = "", axes = FALSE)
    axis(1, at = c(0, parms[1]/sum(parms), 1), labels = round(c(0, parms[1]/sum(parms), 1), 2))  
    legend("topleft", expression(paste("Beta(", alpha, ", ", beta, ")")), bty = 'n')
  } else if(dist == 3) {
    x <- c(0:parms[1])
    d <- dbinom(x, parms[1], parms[2])
    ylims <- c(0, max(d)*1.2)
    plot(x, d, type = "h", ylim = ylims, xlim = c(0,50),
         col = 2, xlab = "", ylab = "", axes = FALSE)
    points(x, dbinom(x, parms[1], parms[2]), pch = 16, cex = 1.2, col = 2)
    axis(1)
    legend("topleft", "Bin(n, p)", bty = 'n')
  }
}