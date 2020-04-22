#!/usr/bin/Rscript
rm(list = ls())
library(ncdf4)
library(raster)
library(maps)
library(pracma)
# setwd("..")
# a.	Baca BT kanal IR Window (IRW) 10.7 μm dan NWP Tropopause Temperature
rnc = raster("nc/H08_B13_Indonesia_201803020340.nc")
tmp_gfs = brick("GFS/gfsanl_3_20180302_0000_003.grb2")
tmp_gfs = tmp_gfs[[347]]
# plot(tmp_gfs)

# image(mnc)
The_Cold = 215
e = raster()
e2 = raster()
extent(e) = c(xmn = 130, xmx = 135, ymn = -11, ymx = 5)
# extent(e2) = c(xmn = 100, xmx = 145.2, ymn = -10, ymx = 10)
extent(e2) = c(xmn = 130, xmx = 150, ymn = -11, ymx = 5)
rnc = crop(rnc, e)
# plot(rnc)
# map("world", add = T)
# ini = tmp_gfs
# tmp_gfs = ini
tmp_gfs = crop(tmp_gfs, e2)
# extent(tmp_gfs) = extent(rnc)
PerbX = res(tmp_gfs)[1]/res(rnc)[1]
PerbY = res(tmp_gfs)[2]/res(rnc)[2]
tmp_gfs = disaggregate(tmp_gfs, c(PerbX, PerbY),methods = "bilinear")
XLNGTH = min(c(dim(tmp_gfs)[1], dim(rnc)[1]))
YLNGTH = min(c(dim(tmp_gfs)[2], dim(rnc)[2]))
tmp_gfs1 = t(apply(as.matrix(tmp_gfs), c(2), FUN = rev))
tmp_gfs1 = tmp_gfs1[1:YLNGTH,1:XLNGTH]
tmp_gfs1 = raster(apply(t(tmp_gfs1), c(2), FUN = rev))
extent(tmp_gfs1) = extent(rnc)
crs(tmp_gfs1) = crs(rnc)
tmp_gfs = tmp_gfs1

rnc1 = t(apply(as.matrix(rnc), c(2), FUN = rev))
rnc1 = rnc1[1:YLNGTH,1:XLNGTH]
rnc1 = raster(apply(t(rnc1), c(2), FUN = rev))
extent(rnc1) = extent(rnc)
crs(rnc1) = crs(rnc)
rnc = rnc1

mnc = t(apply(as.matrix(rnc), c(2), FUN = rev))
lon = round(seq(rnc@extent[1], rnc@extent[2], length = ncol(rnc)), 5)
lat = round(seq(rnc@extent[3], rnc@extent[4], length = nrow(rnc)), 5)
# ================ JIKA MENGGANTI DOMAIN ================
# ps_lon =  1:ncol(rnc)
# ps_lat =  (1):(nrow(rnc))
# MXY = meshgrid(ps_lon, ps_lat)
# MXY_S = matrix(0, nrow = nrow(MXY$X), ncol = ncol(MXY$X))
# for(i in 1:dim(MXY$Y)[1]){
#   for(j in 1:dim(MXY$Y)[2]){
#     MXY_S[i, j] = paste(c(MXY$X[i, j], MXY$Y[i, j]), collapse = "_")
#   }
# }
# MXY_Super = MXY_S
# MXY_Super = t(MXY_Super)
# save(MXY_Super, file = "MXY_Super.bin")
# ============ SELESAI MENGGANTI DOMAIN =============
load("MXY_Super.bin")
tmp_gfs = ((tmp_gfs - 32) * (5/9)) + 273.15 # Fahrenheit to Kelvin
Mtmp_gfs = t(apply(as.matrix(tmp_gfs), c(2), FUN = rev))

TITIK_DINGIN = data.frame(POINT = as.character(MXY_Super[mnc <= The_Cold]), TEMP = as.numeric(mnc[mnc <= The_Cold]), 
                          TEMP_Trop = as.numeric(Mtmp_gfs[mnc <= The_Cold]), stringsAsFactors = F)
TITIK_DINGIN <- TITIK_DINGIN[order(TITIK_DINGIN$TEMP),]

coord = matrix(unlist(strsplit(TITIK_DINGIN$POINT, split = "_")), ncol = 2, byrow = T)
coord_lon = round(lon[as.integer(coord[,1])], 5)
coord_lat = round(lat[as.integer(coord[,2])], 5)

TITIK_DINGIN = cbind(coord_lon, coord_lat, TITIK_DINGIN)
TITIK_DINGIN$TEMP = round(TITIK_DINGIN$TEMP, 5)
The_1st = TITIK_DINGIN[1,]
coor = cbind(coord_lon,coord_lat)
coor = coordinates(coor)
coord = data.frame(coord, stringsAsFactors = F)
names(coord) = c("xcoord", "ycoord")
COLD_DATA = SpatialPointsDataFrame(coords = coor, data = cbind(TITIK_DINGIN, coord))
crs(COLD_DATA) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
MASUK = c()
for(i in 1:length(COLD_DATA$TEMP)){
  if(any(COLD_DATA$TEMP[i] > COLD_DATA$TEMP_Trop[i])){
    MASUK[i] = "GAGAL"
  }else{
    MASUK[i] = i
  }
}
COLD_DATA = COLD_DATA[which(MASUK != "GAGAL"),]

# Mtmp_gfs = t(apply(as.matrix(tmp_gfs), c(2), FUN = rev))
# Mtmp_gfs[as.integer(COLD_DATA$xcoord), as.integer(COLD_DATA$ycoord)]
# c(as.integer(COLD_DATA$xcoord), as.integer(COLD_DATA$ycoord))
# image(lon, lat ,mnc)
# points(The_1st$coord_lon,The_1st$coord_lat,pch = 2, cex = 4)  
RAST = t(apply(as.matrix(rnc), c(2), FUN = rev))
RAST[as.integer(COLD_DATA@data$xcoord), as.integer(COLD_DATA@data$ycoord)] = 220
RAST = raster(apply(t(RAST), c(2), FUN = rev))
crs(RAST) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
extent(RAST) = extent(rnc)
PreOT_Point_C = function(x ){
  detect_15km = extract(RAST, COLD_DATA[x,], buffer = 15000 )[[1]]
  
  # detect_15km[which(detect_15km == min(detect_15km))[1]] = 220
  if(any(detect_15km < The_Cold + 6.5)){
    OT = "This is n't OT"
  }else{
    OT = "Overshooting"
  }
  return(OT)
}

OT_p = c()
for(i in 1:nrow(COLD_DATA)){
  OT_p[i] = PreOT_Point_C(i)
}


# for(i in 1:as.integer(nrow(COLD_DATA)/1000)){
#   OT_p[i] = PreOT_Point_C(i)
# }
# 
# for(i in 1:as.integer(nrow(COLD_DATA)/1000)){
#   OT_p[i] = PreOT_Point_C(i)
# }
# detect_15km = extract(rnc, COLD_DATA[1:10,], buffer = 8000 )