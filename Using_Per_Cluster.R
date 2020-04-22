#!/usr/bin/Rscript
library(EBImage)
library(raster)
library(maps)
library(spatial.tools)

rm(list = ls())
rnc = raster("nc/H08_B13_Indonesia_201803020330.nc")
e = raster()
extent(e) = c(xmn = 130, xmx = 135, ymn = -11, ymx = 5)
rnc = crop(rnc, e)
lon = seq(extent(rnc)[1],extent(rnc)[2],length = ncol(rnc))
lat = seq(extent(rnc)[3],extent(rnc)[4],length = nrow(rnc))

mnc = t(apply(as.matrix(rnc), c(2), FUN = rev))
The_Matrix = mnc
The_Matrix[mnc > 215] = 0

The_Matrix = bwlabel(The_Matrix)
The_Table = table(c(The_Matrix))
Com_Shape = computeFeatures.shape(The_Matrix)
Com_Shape = data.frame(Com_Shape, stringsAsFactors = F)
The_Area = Com_Shape$s.area
The_RowNames = rownames(Com_Shape)
# length(The_Table)

The_List_Matrix = list()
i_pol = list()
mat_poly = list()
coords = list()
coords_box = list()
for(i in 1:length(The_RowNames)){
  The_A = The_Matrix
  The_A[The_Matrix != The_RowNames[i]] = NA
  The_List_Matrix[[i]] = The_A
  i_pol[[i]] = which(!is.na(The_A),arr.ind = T)
  mat_poly[[i]] = cbind(lon[i_pol[[i]][,1]],lat[i_pol[[i]][,2]])
  colnames(mat_poly[[i]]) = c("x","y")
  hpts = chull(x = mat_poly[[i]][,1],y = mat_poly[[i]][,2])
  hpts <- c(hpts, hpts[1])
  coords[[i]] <- mat_poly[[i]][hpts,]
}

sp_poly  = list()
for(i in 1:length(coords)){
  sp_poly[[i]] <- Polygons(list(Polygon(coords[[i]])), ID= i )
}
Spoly=SpatialPolygons(sp_poly)

Data_Cluster=SpatialPolygonsDataFrame(Spoly, data.frame(row.names=paste0(1:length(coords)), Cloud_Cluster=1:length(coords), y=1:length(coords)))
crs(Data_Cluster) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


bbox_SP = list()
for(i in 1:length(Data_Cluster)){
  bbox_SP[[i]] = bbox_to_SpatialPolygons(bbox(Data_Cluster[i,]), proj4string = crs(Data_Cluster))
  # bbox_SP[[i]] = Polygons(bbox_SP[[i]], ID = i)
  bbox_SP[[i]] = bbox_SP[[i]]@polygons[[1]]
  bbox_SP[[i]] = bbox_SP[[i]]@Polygons[[1]]
  bbox_SP[[i]] = Polygons(list(bbox_SP[[i]]), ID = i)
}
bbox_SP=SpatialPolygons(bbox_SP)
bbox_SP = SpatialPolygonsDataFrame((bbox_SP), data = data.frame(Cluster = 1:length(bbox_SP)))
crs(bbox_SP) = crs(rnc)

plot(rnc)
plot(bbox_SP, add = T)
map("world", add = T)