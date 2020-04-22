library(EBImage)
rm(list = ls())
rnc = raster("nc/H08_B13_Indonesia_201803020340.nc")
e = raster()
extent(e) = c(xmn = 130, xmx = 135, ymn = -11, ymx = 5)
rnc = crop(rnc, e)
mnc = t(apply(as.matrix(rnc), c(2), FUN = rev))
The_Matrix = mnc
The_Matrix[mnc > 215] = 0
image(The_Matrix)
The_Matrix = bwlabel(The_Matrix)
The_Table = table(c(The_Matrix))
Com_Shape = computeFeatures.shape(The_Matrix)
Com_Shape = data.frame(Com_Shape, stringsAsFactors = F)
The_Area = Com_Shape$s.area
The_RowNames = rownames(Com_Shape)
length(The_Table)

The_List_Matrix = list()
for(i in 1:length(The_RowNames)){
  The_A = The_Matrix
  The_A[The_Matrix != The_RowNames[i]] = NA
  The_List_Matrix[[i]] = The_A
}
The_List_Matrix = do.call("array", The_List_Matrix)

