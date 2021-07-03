##  plot life table by age over time and age patterns

# List of packages for session
.packages = c("openxlsx", "readxl", "writexl", "data.table", "lubridate", "cwhmisc", "devtools", "MortCast", "signal", "RColorBrewer", "Hmisc", "magicaxis")  ## "MortalityLaws", 

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst],dependencies=TRUE)

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

workdir <- 'd:/United Nations/DESA-POP - DemoData/LifeTable/plots/'
setwd(workdir)


## LT1 <- fread("Spectrum_2016_LT1x1_v2.csv")	
LT1 <- fread("LT1_test.csv")	

 # define transparency function for named colors:
	colalpha <- function(color,alpha){
	 colalphai <- function(color,alpha){
	      paste(rgb(t(col2rgb(color)/255)),alpha,sep="")
		  }
	sapply(color,colalphai,alpha=alpha)
	}


options(digits=6)

DT <- dcast(LT1, Variant + country + country_code + ISO3 + Year + Age ~ Sex, value.var=c("nqx"))
DT <- DT[Year < 2030 & Age < 110]

xrange <- range(DT$Year)
yrange <- range(c(DT$Male, DT$Female), na.rm = TRUE)

subPlot <- unique(DT$country)
maxsubPlot <- length(subPlot)
## subPlotID = paste(LETTERS[1:maxsubPlot],'.', sep = "")


pdf(file = "qx-time.pdf", width=10*2, height=6.5*2, useDingbats=FALSE)
par(oma=c(0, 0, 0, 0)) # no margins

for (i in 1:maxsubPlot) 
{  
  linegr <- subset(DT, DT$country==subPlot[i]) 
  uxAge <- sort(unique(linegr$Age))
  ## uxVar <- unique(linegr$Variant)
  maxtype <- length(uxAge)
  plotchar <- c(rep(seq(1,25,1),maxtype))[1:maxtype]
  ## linetype <- c(rep(c(1:6),maxtype))[1:maxtype]
  linetype <- c(2, 1)

## "Greens", "Greys", "Oranges", "OrRd", "PuBuGn", "Purples", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
## The sequential palettes names are
## Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
## All the sequential palettes are available in variations from 3 different values up to 9 different values.
## 
## The diverging palettes are
## BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral

  colors <- c(rep(rainbow(8),maxtype))[1:maxtype]
  ## colors <- c(rev(colorRampPalette(brewer.pal(9,"YlOrRd")[3:9])(length(uxYear[uxYear <= WPPRev]))),
  ##		  rev(colorRampPalette(brewer.pal(9,"YlGnBu")[4:9])(length(uxYear[uxYear > WPPRev]))))
  ## colors <- rep("black", maxtype)
  ## colors <- c("blue", "red")
    

 
  yLowerBound <- 0
  yUpperBound <- max(linegr$Female, linegr$Male, na.rm = TRUE)

  ## xlim=xrange, ylim=c(yLowerBound, yUpperBound), 
  par(mfrow = c(1,2))
  
  ticks <- seq(-6, 0, by=1)
  labels <- sapply(ticks, function(i) as.expression(bquote(10^ .(i))))

  ## Panel 1: Male
  curves <- vector('list', maxtype)
  plot(linegr$Year, log10(linegr$Male), type='n', lab=c(11,8,4), xlab = "", ylab = "Qx", main = paste(subPlot[i], "Male Qx"), col = "black", cex.main = 1.5, yaxt='n') 
  for (j in 1:maxtype) { 
    linegr2 <- subset(linegr, Age==uxAge[j]) 
    lines(sortedXyData(linegr2$Year, log10(linegr2$Male)), type="l", lwd=2, lty=1,  col=colors[j])     
    curves[[j]] <- list(x=linegr2$Year, y=log10(linegr2$Male))
  } 
  axis(2, at=ticks, labels=c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1))
  grid(col = "gray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
  abline(v = 2020, type="l", lwd=3, lty=1,  col="black")
  labels <- uxAge
  names(curves) <- labels
  labcurve(curves, labels, tilt=TRUE, method="offset", adj="auto", type=linetype, col="black", cex=.5) 

  ## Panel 2: Female
  curves <- vector('list', maxtype)
  plot(linegr$Year, log10(linegr$Female), type='n', lab=c(11,8,4), xlab = "", ylab = "Qx", main = paste(subPlot[i], "Female Qx"), col = "black", cex.main = 1.5, yaxt='n') 
  for (j in 1:maxtype) { 
    linegr2 <- subset(linegr, Age==uxAge[j]) 
    lines(sortedXyData(linegr2$Year, log10(linegr2$Female)), type="l", lwd=2, lty=1,  col=colors[j])     
    curves[[j]] <- list(x=linegr2$Year, y=log10(linegr2$Female))
  } 
  axis(2, at=ticks, labels=c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1))
  grid(col = "gray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
  abline(v = 2020, type="l", lwd=3, lty=1,  col="black")
  labels <- uxAge
  names(curves) <- labels
  labcurve(curves, labels, tilt=TRUE, method="offset", adj="auto", type=linetype, col="black", cex=.5) 

} 
dev.off()  


WPPRev <- 2020
pdf(file = "qx-age.pdf", width=10*2, height=6.5*2, useDingbats=FALSE)
par(oma=c(0, 0, 0, 0)) # no margins

for (i in 1:maxsubPlot) 
{  
  linegr <- subset(DT, DT$country==subPlot[i]) 
  uxYear <- sort(unique(linegr$Year))
  maxtype <- length(uxYear)
  plotchar <- c(rep(seq(1,25,1),maxtype))[1:maxtype]
  ## linetype <- c(rep(c(1:6),maxtype))[1:maxtype]
  linetype <- c(2, 1)

## "Greens", "Greys", "Oranges", "OrRd", "PuBuGn", "Purples", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
## The sequential palettes names are
## Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
## All the sequential palettes are available in variations from 3 different values up to 9 different values.
## 
## The diverging palettes are
## BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral

  length(uxYear[uxYear <= WPPRev])
  ## colors <- c(rep(rainbow(8),maxtype))[1:maxtype]
  colors <- c(rev(colorRampPalette(brewer.pal(9,"YlGnBu")[4:9])(length(uxYear[uxYear <= WPPRev]))),
			  rev(colorRampPalette(brewer.pal(9,"YlOrRd")[3:9])(length(uxYear[uxYear >  WPPRev]))))
  ## colors <- rep("black", maxtype)
  ## colors <- c("blue", "red")
    
  curves <- vector('list', maxtype)

  yLowerBound <- 0
  yUpperBound <- max(c(linegr$Female, linegr$Male), na.rm = TRUE)

  ## xlim=xrange, ylim=c(yLowerBound, yUpperBound), 
  par(mfrow = c(1,2))
  
  ticks <- seq(-6, 0, by=1)
  # labels <- sapply(ticks, function(i) as.expression(bquote(10^ .(i))))

  ## Panel 1: Male
  plot(linegr$Age, log10(linegr$Male), type='n', lab=c(11,8,4), xlab = "", ylab = "Qx", main = paste(subPlot[i], "Male Qx"), col = "black", cex.main = 1.5, yaxt='n')
  for (j in 1:maxtype) { 
    linegr2 <- subset(linegr, Year==uxYear[j]) 
    lines(sortedXyData(linegr2$Age, log10(linegr2$Male)), type="l", lwd=1, lty=1,  col=colors[j])     
	if (linegr2$Year==2018) {
       lines(sortedXyData(linegr2$Age, log10(linegr2$Male)), type="l", lwd=3, lty=1,  col="black")     
	}
    curves[[j]] <- list(x=linegr2$Age, y=log10(linegr2$Male))
  } 
  axis(2, at=ticks, labels=c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1))
  labels <- uxYear
  names(curves) <- labels
  grid(col = "gray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
  labcurve(curves, labels, tilt=TRUE, method="offset", adj="auto", type=linetype, col="black", cex=0.5) 

  ## Panel 2: Female
  plot(linegr$Age, log10(linegr$Female), type='n', lab=c(11,8,4), xlab = "", ylab = "Qx", main = paste(subPlot[i], "Female Qx"), col = "black", cex.main = 1.5, yaxt='n')
  for (j in 1:maxtype) { 
    linegr2 <- subset(linegr, Year==uxYear[j]) 
    lines(sortedXyData(linegr2$Age, log10(linegr2$Female)), type="l", lwd=1, lty=1,  col=colors[j])     
	if (linegr2$Year==2018) {
       lines(sortedXyData(linegr2$Age, log10(linegr2$Female)), type="l", lwd=3, lty=1,  col="black")     
	}
    curves[[j]] <- list(x=linegr2$Age, y=log10(linegr2$Female))
  } 
  axis(2, at=ticks, labels=c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1))
  labels <- uxYear
  names(curves) <- labels
  grid(col = "gray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
  labcurve(curves, labels, tilt=TRUE, method="offset", adj="auto", type=linetype, col="black", cex=0.5) 
  
} 
dev.off()  


## compute sex ratios
DT[, SR := Male / Female]

## repeat this time with age profiles
pdf(file = "qxSR-age.pdf", width=10*2, height=6.5*2, useDingbats=FALSE)
par(oma=c(0, 0, 0, 0)) # no margins

for (i in 1:maxsubPlot) 
{  
  linegr <- subset(DT, DT$country==subPlot[i]) 
  uxYear <- sort(unique(linegr$Year))
  maxtype <- length(uxYear)
  plotchar <- c(rep(seq(1,25,1),maxtype))[1:maxtype]
  ## linetype <- c(rep(c(1:6),maxtype))[1:maxtype]
  linetype <- c(2, 1)

  length(uxYear[uxYear <= WPPRev])
  ## colors <- c(rep(rainbow(8),maxtype))[1:maxtype]
  colors <- c(rev(colorRampPalette(brewer.pal(9,"YlGnBu")[4:9])(length(uxYear[uxYear <= WPPRev]))),
			  rev(colorRampPalette(brewer.pal(9,"YlOrRd")[3:9])(length(uxYear[uxYear >  WPPRev]))))
  ## colors <- rep("black", maxtype)
  ## colors <- c("blue", "red")
    
  curves <- vector('list', maxtype)

  yLowerBound <- min(linegr$SR, na.rm = TRUE)
  yUpperBound <- max(linegr$SR, na.rm = TRUE)

  ## xlim=xrange, ylim=c(yLowerBound, yUpperBound), 
  par(mfrow = c(1,1))
  
  ticks <- seq(-6, 0, by=1)
  # labels <- sapply(ticks, function(i) as.expression(bquote(10^ .(i))))

  plot(linegr$Age, linegr$SR, type='n', lab=c(11,8,4), xlab = "Age", ylab = "Sex Ratio: Qx Male/Female", main = paste(subPlot[i], "Qx sex ratio (M/F)"), col = "black", cex.main = 1.5)
  for (j in 1:maxtype) { 
    linegr2 <- subset(linegr, Year==uxYear[j]) 
    lines(sortedXyData(linegr2$Age, linegr2$SR), type="l", lwd=1, lty=1,  col=colors[j])     
	if (linegr2$Year==2018) {
       lines(sortedXyData(linegr2$Age, linegr2$SR), type="l", lwd=3, lty=1,  col="black")     
	}
    curves[[j]] <- list(x=linegr2$Age, y=linegr2$SR)
  } 
  labels <- uxYear
  names(curves) <- labels
  grid(col = "gray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
  abline(h = 1, type="l", lwd=5, lty=1,  col=colalpha("black", 50))
  labcurve(curves, labels, tilt=TRUE, method="offset", adj="auto", type=linetype, col="black", cex=0.5) 
  
} 
  dev.off()  




pdf(file = "qxSR-Time.pdf", width=10*2, height=6.5*2, useDingbats=FALSE)
par(oma=c(0, 0, 0, 0)) # no margins

for (i in 1:maxsubPlot) 
{  
  linegr <- subset(DT, DT$country==subPlot[i]) 
  uxAge <- sort(unique(linegr$Age))
  ## uxVar <- unique(linegr$Variant)
  maxtype <- length(uxAge)
  plotchar <- c(rep(seq(1,25,1),maxtype))[1:maxtype]
  ## linetype <- c(rep(c(1:6),maxtype))[1:maxtype]
  linetype <- c(2, 1)

  # colors <- c(rep(rainbow(8),maxtype))[1:maxtype]   
  colors <- c(rev(colorRampPalette(brewer.pal(9,"YlOrRd")[3:9])(length(uxAge[uxAge < 60]))),
			  rev(colorRampPalette(brewer.pal(9,"YlGnBu")[4:9])(length(uxAge[uxAge >= 60]))))

  yLowerBound <- 0
  yUpperBound <- max(linegr$Total, na.rm = TRUE)

  ## xlim=xrange, ylim=c(yLowerBound, yUpperBound), 
  par(mfrow = c(1,1))
  
  ticks <- seq(-6, 0, by=1)
  labels <- sapply(ticks, function(i) as.expression(bquote(10^ .(i))))

  curves <- vector('list', maxtype)
  plot(linegr$Year, linegr$SR, type='n', lab=c(11,8,4), xlab = "Period", ylab = "Sex Ratio: Qx Male/Female", main = paste(subPlot[i], "Qx sex ratio (M/F)"), col = "black", cex.main = 1.5) 
  for (j in 1:maxtype) { 
    linegr2 <- subset(linegr, Age==uxAge[j]) 
    lines(sortedXyData(linegr2$Year, linegr2$SR), type="l", lwd=2, lty=1,  col=colors[j])     
    curves[[j]] <- list(x=linegr2$Year, y=linegr2$SR)
  } 
  grid(col = "gray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
  abline(h = 1, type="l", lwd=5, lty=1,  col=colalpha("black", 50))
  abline(v = 2020, type="l", lwd=5, lty=1,  col=colalpha("black", 50))
  labels <- uxAge
  names(curves) <- labels
  labcurve(curves, labels, tilt=TRUE, method="offset", adj="auto", type=linetype, col="black", cex=.5) 

} 
  dev.off()  
