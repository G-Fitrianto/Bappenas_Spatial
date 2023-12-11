#install.packages(c("xlsx", "sp", "sf", "spdep", "tmap"))

library(xlsx)
library(sp)
library(sf)
library(spdep)
library(tmap)

ina.shp   <- st_read("D:/SHAPEFILES/INA_GADM/gadm41_IDN_2.shp")
IPM       <- read.xlsx("E:/Workshop Spatial Econometrics/2020 IPM Data.xlsx", sheetName = "Sheet1")
ina.shp$IPM  <- as.numeric(IPM$IPM)

kepri.poly <- (ina.shp[ina.shp$NAME_1 == "Kepulauan Riau", ])
kepri.centroid <- st_centroid(kepri.poly)


png("E:/BAPPENAS/IPM/2020_IPM_kepri.png", width = 1400, height = 1000, units = "px")
plot(kepri.poly[,ncol(kepri.poly)])
#text(st_coordinates(kepri.centroid$geometry), labels = kepri.poly$NAME_2)
dev.off()


X     <- kepri.poly$IPM
x.na  <- which(is.na(X), arr.ind = T)

if(identical(x.na, integer(0))){
  X <- X
} else {
  X <- X[-x.na]
}

keprinb     <- poly2nb(kepri.poly$geometry, queen = T)
keprilistw  <- nb2listw(keprinb, style = "W", zero.policy = T)
keprimat    <- listw2mat(keprilistw)
kepri.m_test<- moran.test(c(na.omit(X)), keprilistw, zero.policy = T)

png("E:/BAPPENAS/IPM/2020_IPM_MoranPlot_kepri.png", width = 800, height = 600, units = "px")
moran.plot(c(na.omit(X)), keprilistw, zero.policy = T, 
           main="Global Moran Kepulauan Riau 2020", pch=16, col="black", cex=3.5)
dev.off()

sink("E:/BAPPENAS/IPM/2020_IPM_MoranTest_kepri.txt")
moran.test(c(na.omit(X)), keprilistw, zero.policy = T)
sink()

sink("E:/BAPPENAS/IPM/2020_IPM_LocalMoranTest_kepri.txt")
kepri.l_test<- localmoran(c(na.omit(X)), keprilistw, zero.policy = T)
kepri.l_test
sink()

localmoran_map <- cbind(kepri.poly, kepri.l_test)

# Map For P-value Local Moran's
png("E:/BAPPENAS/IPM/2020_IPM_pval_LocalMoran_kepri.png", width = 800, height = 600, units = "px")
tm_shape(localmoran_map) +
  tm_fill(col = "Pr.z....E.Ii..",
          palette = "RdBu",
          style = "quantile",
          title = "P-value local moran statistic") 
dev.off()

# Map For Ii-value Local Moran's
png("E:/BAPPENAS/IPM/2020_IPM_Ival_LocalMoran_kepri.png", width = 800, height = 600, units = "px")
tm_shape(localmoran_map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "I-values of local moran statistic") 
dev.off()

# Clusters Map

local_g <- localG(X, keprilistw)   # Local Geary's C statistics
local_g <- cbind(kepri.poly, as.matrix(local_g))

png("E:/BAPPENAS/IPM/2020_IPM_ClusterLocal_papua.png", width = 800, height = 600, units = "px")
tm_shape(local_g) + 
  tm_fill("as.matrix.local_g.", 
          palette = "RdBu",
          style = "pretty") +
  tm_borders(alpha=.4)
dev.off()