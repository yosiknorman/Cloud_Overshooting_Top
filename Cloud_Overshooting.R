#!/usr/bin/Rscript

library(ncdf4)
library(raster)
library(maps)
library(pracma)

# a.	Baca BT kanal IR Window (IRW) 10.7 μm dan NWP Tropopause Temperature
nc = nc_open("nc/H08_B13_Indonesia_201803020340.nc")
lon = ncvar_get(nc, "longitude")
lat = ncvar_get(nc, "latitude")
var = ncvar_get(nc, "IR")
rnc = raster("nc/H08_B13_Indonesia_201803020340.nc")
lon = round(seq(rnc@extent[1], rnc@extent[2], length = ncol(rnc)), 5)
lat = round(seq(rnc@extent[3], rnc@extent[4], length = nrow(rnc)), 5)
# ps_lon = paste0("X", 1:ncol(rnc))
# ps_lat = paste0("Y", 1:nrow(rnc))
ps_lon =  1:ncol(rnc)
ps_lat =  (9000+1):(nrow(rnc)+9000)
ps_xy = paste(ps_lon, ps_lat, collapse = "_")

# b.	Identifikasi piksel dengan IRW BT ≤ 215 K dan ≤ NWP tropopause temp
RA_VAR = var
RA_VAR[which(var > 215  )] = NA

# plot(rnc)
image(lon, lat, RA_VAR)
map("world", add = T)

MXY = meshgrid(ps_lon, ps_lat)
MXY$X[1:2]
MXY$Y[1:2]
MXY_S = paste(MXY$X, MXY$Y, collapse = "_")
# round(MXY$X[1:10], 5)
xy_Index = which(var <= 215  , arr.ind = T)
# lon_Index = round(lon[xy_Index[,1]], 5)
# lat_Index = round(lat[xy_Index[,2]], 5)
lon_Index = ps_lon[xy_Index[,1]]
lat_Index = ps_lat[xy_Index[,2]]
xy_STRING = paste(lon_Index, lat_Index, collapse = "_")
which(xy_STRING[1])

a = cbind(lon_Index[1:100], lat_Index[1:100], Suhu = c(var[xy_Index[, 1], xy_Index[, 2]]))
cood= cbind(lon_Index, lat_Index)
cood = coordinates(cood) 
SPD_XY = SpatialPointsDataFrame(data = data.frame(lon_Index, lat_Index), coords = cood)
crs(SPD_XY) = crs(rnc)
extract(rnc, cbind(lon_Index[1], lat_Index[1]), buffer = 15000 )
#Make a distance matrix =

dst <- pointDistance(a[1:10,1:2], lonlat=TRUE)
dst <- as.matrix(dst)
dst[dst == 0] = NA
this_is =  function(int ){
  JARAK = dst[int,][which(dst[int,] == min (dst[int,], na.rm = T))]
  return(JARAK)
}

Hasil_Jarak = c()
for(i in 1:nrow(dst)){
  # if(!is.na(this_is(int = i)) ){
    Hasil_Jarak[i] = this_is(int = i)/1000
  # }else{
    # Hasil_Jarak[i] = 0
  # }
}
Hasil_Jarak[is.na(Hasil_Jarak)] = 0
# coerce to dist object
HASIL = cbind(a[1:10, ], Hasil_Jarak)
HASIL = data.frame(HASIL, stringsAsFactors = F)
names(HASIL) =c("LON", "LAT", "TEMP", "DIST_KM")
OT = HASIL[HASIL$DIST_KM > 15,]

image(lon, lat, RA_VAR)
map("world", add = T)
points(OT$LON, OT$LAT, pch = 2, cex = 2) 

