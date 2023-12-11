#install.packages(c("xlsx", "sp", "sf", "spdep", "tmap"))

library(xlsx)
library(sp)
library(sf)
library(spdep)
library(tmap)

ina.shp   <- st_read("D:/SHAPEFILES/INA_GADM/gadm41_IDN_2.shp")
tpt       <- read.xlsx("E:/Workshop Spatial Econometrics/2020 TPT Data.xlsx", sheetName = "Sheet1")
ina.shp$tpt  <- as.numeric(tpt$TPT)

sulsel.poly <- (ina.shp[ina.shp$NAME_1 == "Sulawesi Selatan", ])

png("E:/BAPPENAS/TPT/2020_TPT_sulsel.png", width = 800, height = 600, units = "px")
plot(sulsel.poly[,ncol(sulsel.poly)])
dev.off()


X     <- sulsel.poly$tpt
x.na  <- which(is.na(X), arr.ind = T)

if(identical(x.na, integer(0))){
  X <- X
} else {
  X <- X[-x.na]
}

sulselnb     <- poly2nb(sulsel.poly$geometry, queen = T)
sulsellistw  <- nb2listw(sulselnb, style = "W", zero.policy = T)
sulselmat    <- listw2mat(sulsellistw)
sulsel.m_test<- moran.test(c(na.omit(X)), sulsellistw, zero.policy = T)

png("E:/BAPPENAS/TPT/2020_TPT_MoranPlot_Sulsel.png", width = 800, height = 600, units = "px")
moran.plot(c(na.omit(X)), sulsellistw, zero.policy = T, 
           main="Global Moran Sulawesi Selatan 2020", pch=16, col="black", cex=3.5)
dev.off()

sink("E:/BAPPENAS/TPT/2020_TPT_MoranTest_Sulsel.txt")
moran.test(c(na.omit(X)), sulsellistw, zero.policy = T)
sink()

sink("E:/BAPPENAS/TPT/2020_TPT_LocalMoranTest_Sulsel.txt")
sulsel.l_test<- localmoran(c(na.omit(X)), sulsellistw, zero.policy = T)
sulsel.l_test
sink()

localmoran_map <- cbind(sulsel.poly, sulsel.l_test)

# Map For P-value Local Moran's
png("E:/BAPPENAS/TPT/2020_TPT_pval_LocalMoran_Sulsel.png", width = 800, height = 600, units = "px")
tm_shape(localmoran_map) +
  tm_fill(col = "Pr.z....E.Ii..",
          palette = "RdBu",
          style = "quantile",
          title = "P-value local moran statistic") 
dev.off()

# Map For Ii-value Local Moran's
png("E:/BAPPENAS/TPT/2020_TPT_Ival_LocalMoran_Sulsel.png", width = 800, height = 600, units = "px")
tm_shape(localmoran_map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "I-values of local moran statistic") 
dev.off()

# Clusters Map

local_g <- localG(X, sulsellistw)   # Local Geary's C statistics
local_g <- cbind(sulsel.poly, as.matrix(local_g))

png("E:/BAPPENAS/TPT/2020_TPT_ClusterLocal_papua.png", width = 800, height = 600, units = "px")
tm_shape(local_g) + 
  tm_fill("as.matrix.local_g.", 
          palette = "RdBu",
          style = "pretty") +
  tm_borders(alpha=.4)
dev.off()