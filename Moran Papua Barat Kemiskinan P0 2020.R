install.packages(c("xlsx", "sp", "sf", "spdep", "tmap"))

library(xlsx)
library(sp)
library(sf)
library(spdep)
library(tmap)

ina.shp   <- st_read("D:/SHAPEFILES/INA_GADM/gadm41_IDN_2.shp")
miskin    <- read.xlsx("E:/Workshop Spatial Econometrics/P0.xlsx", sheetName = "Sheet1")
ina.shp$miskin_2019  <- as.numeric(miskin$X2019)
ina.shp$miskin_2020  <- as.numeric(miskin$X2020) 
ina.shp$miskin_2021  <- as.numeric(miskin$X2021)
ina.shp$miskin_2022  <- as.numeric(miskin$X2022)

sulsel.poly <- (ina.shp[ina.shp$NAME_1 == "Sulawesi Selatan", ])

#mycol <- colorRampPalette(c("lightblue", "darkblue"))
#plot(sulsel.poly[,16], col=mycol(nrow(sulsel.poly)))

png("E:/Workshop Spatial Econometrics/2020_P0/2020_P0_Sulsel.png", width = 1400, height = 1000, units = "px")
plot(sulsel.poly[,16])
dev.off()

X     <- sulsel.poly$miskin_2020
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

png("E:/Workshop Spatial Econometrics/2020_P0/2020_P0_MoranPlot_Sulsel.png", width = 800, height = 600, units = "px")
moran.plot(c(na.omit(X)), sulsellistw, zero.policy = T, 
           main="Global Moran P0 Jawa Tengah 2020", pch=16, col="black", cex=3.5)
dev.off()

sink("E:/Workshop Spatial Econometrics/2020_P0/2020_P0_MoranTest_Sulsel.txt")
moran.test(c(na.omit(X)), sulsellistw, zero.policy = T)
sink()

sink("E:/Workshop Spatial Econometrics/2020_P0/2020_P0_LocalMoranTest_Sulsel.txt")
sulsel.l_test<- localmoran(c(na.omit(X)), sulsellistw, zero.policy = T)
sulsel.l_test
sink()

localmoran_map <- cbind(sulsel.poly, sulsel.l_test)

# Map For P-value Local Moran's
png("E:/Workshop Spatial Econometrics/2020_P0/2020_P0_pval_LocalMoran_Sulsel.png", width = 1400, height = 1000, units = "px")
tm_shape(localmoran_map) +
  tm_fill(col = "Pr.z....E.Ii..",
          palette = "RdBu",
          style = "quantile",
          title = "P-value local moran statistic") 
dev.off()

# Map For Ii-value Local Moran's
png("E:/Workshop Spatial Econometrics/2020_P0/2020_P0_Ival_LocalMoran_Sulsel.png", width = 1400, height = 1000, units = "px")
tm_shape(localmoran_map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "I-values of local moran statistic") 
dev.off()

# Clusters Map

local_g <- localG(X, sulsellistw)   # Local Geary's C statistics
local_g <- cbind(sulsel.poly, as.matrix(local_g))

png("E:/Workshop Spatial Econometrics/2020_P0/2020_P0_ClusterLocal_Sulsel.png", width = 1400, height = 1000, units = "px")
tm_shape(local_g) + 
  tm_fill("as.matrix.local_g.", 
          palette = "RdBu",
          style = "pretty") +
  tm_borders(alpha=.4)
dev.off()

