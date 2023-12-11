#install.packages(c("xlsx", "sp", "sf", "spdep", "tmap"))

library(xlsx)
library(sp)
library(sf)
library(spdep)
library(tmap)

ina.shp   <- st_read("D:/SHAPEFILES/INA_GADM/gadm41_IDN_2.shp")
ipm       <- read.xlsx("E:/Workshop Spatial Econometrics/2020 IPM Data.xlsx", sheetName = "Sheet1")
ina.shp$ipm  <- as.numeric(ipm$IPM)

papua.id   <- which(ina.shp$NAME_1 == "Papua")
wpapua.id  <- which(ina.shp$NAME_1 == "Papua Barat")
papua.poly <- (ina.shp[c(papua.id, wpapua.id), ])

#mycol <- colorRampPalette(c("lightblue", "darkblue"))
#plot(papua.poly[,16], col=mycol(nrow(papua.poly)))

png("E:/BAPPENAS/IPM/2020_IPM_papua.png", width = 900, height = 700, units = "px")
plot(papua.poly[,ncol(papua.poly)], main = "PAPUA IPM 2020")
dev.off()

X     <- papua.poly$ipm
x.na  <- which(is.na(X), arr.ind = T)

if(identical(x.na, integer(0))){
  X <- X
} else {
  X <- X[-x.na]
}

papuanb     <- poly2nb(papua.poly$geometry, queen = T)
papualistw  <- nb2listw(papuanb, style = "W", zero.policy = T)
papuamat    <- listw2mat(papualistw)
papua.m_test<- moran.test(c(na.omit(X)), papualistw, zero.policy = T)

png("E:/BAPPENAS/IPM/2020_IPM_MoranPlot_papua.png", width = 800, height = 600, units = "px")
moran.plot(c(na.omit(X)), papualistw, zero.policy = T, 
           main="Global Moran IPM 2020", pch=16, col="black", cex=3.5)
dev.off()

sink("E:/BAPPENAS/IPM/2020_IPM_MoranTest_papua.txt")
moran.test(c(na.omit(X)), papualistw, zero.policy = T)
sink()

sink("E:/BAPPENAS/IPM/2020_IPM_LocalMoranTest_papua.txt")
papua.l_test<- localmoran(c(na.omit(X)), papualistw, zero.policy = T)
papua.l_test
sink()

localmoran_map <- cbind(papua.poly, papua.l_test)

# Map For P-value Local Moran's
png("E:/BAPPENAS/IPM/2020_IPM_pval_LocalMoran_papua.png", width = 800, height = 600, units = "px")
tm_shape(localmoran_map) +
  tm_fill(col = "Pr.z....E.Ii..",
          palette = "RdBu",
          style = "quantile",
          title = "P-value local moran statistic") 
dev.off()

# Map For Ii-value Local Moran's
png("E:/BAPPENAS/IPM/2020_IPM_Ival_LocalMoran_papua.png", width = 1400, height = 1000, units = "px")
tm_shape(localmoran_map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "I-values of local moran statistic") 
dev.off()

# Clusters Map

local_g <- localG(X, papualistw)   # Local Geary's C statistics
local_g <- cbind(papua.poly, as.matrix(local_g))

png("E:/BAPPENAS/IPM/2020_IPM_ClusterLocal_papua.png", width = 800, height = 600, units = "px")
tm_shape(local_g) + 
  tm_fill("as.matrix.local_g.", 
          palette = "RdBu",
          style = "pretty") +
  tm_borders(alpha=.4)
dev.off()

