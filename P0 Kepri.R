#install.packages(c("xlsx", "sp", "sf", "spdep", "tmap"))

library(xlsx)
library(sp)
library(sf)
library(spdep)
library(tmap)

ina.shp   <- st_read("D:/SHAPEFILES/INA_GADM/gadm41_IDN_2.shp")
miskin    <- read.xlsx("E:/Workshop Spatial Econometrics/P0.xlsx", sheetName = "Sheet1")
ina.shp$miskin_2020  <- as.numeric(miskin$X2020) 

kepri.id     <- which(ina.shp$NAME_1 == "Kepulauan Riau")
kepri.poly   <- ina.shp[kepri.id,]
plot(kepri.poly[,ncol(kepri.poly)])

wsumatra.id  <- which(ina.shp$NAME_1 == "sumatra Barat")
sumatra.poly <- (ina.shp[c(sumatra.id, wsumatra.id), ])

#mycol <- colorRampPalette(c("lightblue", "darkblue"))
#plot(sumatra.poly[,16], col=mycol(nrow(sumatra.poly)))

png("E:/BAPPENAS/2020_P0/2020_P0_Kepri.png", width = 800, height = 600, units = "px")
plot(kepri.poly[,ncol(kepri.poly)])
dev.off()

X     <- kepri.poly$miskin_2020
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

png("E:/BAPPENAS/2020_P0/2020_P0_MoranPlot_kepri.png", width = 800, height = 600, units = "px")
moran.plot(c(na.omit(X)), keprilistw, zero.policy = T, 
           main="Global Moran P0 2020", pch=16, col="black", cex=3.5)
dev.off()

sink("E:/BAPPENAS/2020_P0/2020_P0_MoranTest_kepri.txt")
moran.test(c(na.omit(X)), keprilistw, zero.policy = T)
sink()

sink("E:/BAPPENAS/2020_P0/2020_P0_LocalMoranTest_kepri.txt")
kepri.l_test<- localmoran(c(na.omit(X)), keprilistw, zero.policy = T)
kepri.l_test
sink()

localmoran_map <- cbind(kepri.poly, kepri.l_test)

# Map For P-value Local Moran's
png("E:/BAPPENAS/2020_P0/2020_P0_pval_LocalMoran_kepri.png", width = 1400, height = 1000, units = "px")
tm_shape(localmoran_map) +
  tm_fill(col = "Pr.z....E.Ii..",
          palette = "RdBu",
          style = "quantile",
          title = "P-value local moran statistic") 
dev.off()

# Map For Ii-value Local Moran's
png("E:/BAPPENAS/2020_P0/2020_P0_Ival_LocalMoran_kepri.png", width = 1400, height = 1000, units = "px")
tm_shape(localmoran_map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "I-values of local moran statistic") 
dev.off()

# Clusters Map

local_g <- localG(X, keprilistw)   # Local Geary's C statistics
local_g <- cbind(kepri.poly, as.matrix(local_g))

png("E:/BAPPENAS/2020_P0/2020_P0_ClusterLocal_kepri.png", width = 1400, height = 1000, units = "px")
tm_shape(local_g) + 
  tm_fill("as.matrix.local_g.", 
          palette = "RdBu",
          style = "pretty") +
  tm_borders(alpha=.4)
dev.off()

