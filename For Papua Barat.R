#install.packages(c("xlsx", "sp", "sf", "spdep", "tmap"))

library(xlsx)
library(sp)
library(sf)
library(spdep)
library(tmap)

ina.shp   <- st_read("D:/SHAPEFILES/INA_GADM/gadm41_IDN_2.shp")
pdrb_20    <- read.xlsx("E:/Workshop Spatial Econometrics/2020 ADHK PDRB.xlsx", sheetName = "Sheet1")
pdrb_19    <- read.xlsx("E:/Workshop Spatial Econometrics/2019 ADHK PDRB.xlsx", sheetName = "Sheet1")
ina.shp$pdrb_2019     <- as.numeric(pdrb_19$X2019) 
ina.shp$pdrb_2020    <- as.numeric(pdrb_20$X2022) 
ina.shp$growth_2020  <- ((ina.shp$pdrb_2020 - ina.shp$pdrb_2019)/ina.shp$pdrb_2019)*100

papua.id   <- which(ina.shp$NAME_1 == "Papua")
wpapua.id  <- which(ina.shp$NAME_1 == "Papua Barat")
papua.poly <- (ina.shp[c(papua.id, wpapua.id), ])

#mycol <- colorRampPalette(c("lightblue", "darkblue"))
#plot(papua.poly[,16], col=mycol(nrow(papua.poly)))

png("E:/BAPPENAS/Eco Growth/Eco Growth_papua.png", width = 1400, height = 1000, units = "px")
plot(papua.poly[,17])
dev.off()

X     <- papua.poly$growth_2020
x.na  <- which(is.na(X), arr.ind = T)
x.max <- which(X == max(X))

if(identical(x.na, integer(0))){
  X <- X
} else {
  X <- X[-x.na]
}

papuanb     <- poly2nb(papua.poly$geometry, queen = T)
papualistw  <- nb2listw(papuanb, style = "W", zero.policy = T)
papuamat    <- listw2mat(papualistw)
papua.m_test<- moran.test(c(na.omit(X)), papualistw, zero.policy = T)
geary.test(X, papualistw, zero.policy = T)

# ----------------------------

papua.poly.rm   <- papua.poly[-x.max,]
papuanb.rm      <- poly2nb(papua.poly.rm$geometry, queen = T)
papualistw.rm   <- nb2listw(papuanb.rm, style = "W", zero.policy = T)
papuamat.rm     <- listw2mat(papualistw.rm)
papua.m_test.rm <- moran.test(c(X[-x.max]), papualistw.rm, zero.policy = T)

#-------------------------------


png("E:/BAPPENAS/Eco Growth/Eco Growth_MoranPlot_papua.png", width = 800, height = 600, units = "px")
moran.plot(c(na.omit(X)), papualistw, zero.policy = T, 
           main="Global Moran P0 Jawa Tengah 2020", pch=16, col="black", cex=3.5)
dev.off()

sink("E:/BAPPENAS/Eco Growth/Eco Growth_MoranTest_papua.txt")
moran.test(c(na.omit(X)), papualistw, zero.policy = T)
sink()

sink("E:/BAPPENAS/Eco Growth/Eco Growth_LocalMoranTest_papua.txt")
papua.l_test<- localmoran(c(na.omit(X)), papualistw, zero.policy = T)
papua.l_test
sink()

localmoran_map <- cbind(papua.poly, papua.l_test)

# Map For P-value Local Moran's
png("E:/BAPPENAS/Eco Growth/Eco Growth_pval_LocalMoran_papua.png", width = 1400, height = 1000, units = "px")
tm_shape(localmoran_map) +
  tm_fill(col = "Pr.z....E.Ii..",
          palette = "RdBu",
          style = "quantile",
          title = "P-value local moran statistic") 
dev.off()

# Map For Ii-value Local Moran's
png("E:/BAPPENAS/Eco Growth/Eco Growth_Ival_LocalMoran_papua.png", width = 1400, height = 1000, units = "px")
tm_shape(localmoran_map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "I-values of local moran statistic") 
dev.off()

# Clusters Map

local_g <- localG(X, papualistw)   # Local Geary's C statistics
local_g <- cbind(papua.poly, as.matrix(local_g))

png("E:/BAPPENAS/Eco Growth/Eco Growth_ClusterLocal_papua.png", width = 1400, height = 1000, units = "px")
tm_shape(local_g) + 
  tm_fill("as.matrix.local_g.", 
          palette = "RdBu",
          style = "pretty") +
  tm_borders(alpha=.4)
dev.off()


plot(X - mean(X), pch=16, cex=3, col="red")
abline(a=0, b=mean(X - mean(X)), lwd=4)
hist(X - mean(X), freq = F)
lines(density(X - mean(X)), lwd=4, col="red")

plot(papuamat %*% X, pch=16, cex=3, col="red")
abline(a=mean(papuamat %*% X), b=0, lwd=4)

 