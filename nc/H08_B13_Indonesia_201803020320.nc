
R version 3.4.1 (2017-06-30) -- "Single Candle"
Copyright (C) 2017 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> #!/usr/bin/Rscript
> 
> library("leaflet")
> library("EBImage")
> library("ncdf4")
> library("maps")
> library("raster")
Loading required package: sp

Attaching package: ‘raster’

The following objects are masked from ‘package:EBImage’:

    flip, rotate

> library("sp")
> library("maptools")
Checking rgeos availability: TRUE
> library("rgdal")
rgdal: version: 1.3-4, (SVN revision 766)
 Geospatial Data Abstraction Library extensions to R successfully loaded
 Loaded GDAL runtime: GDAL 2.2.2, released 2017/09/15
 Path to GDAL shared files: /usr/share/gdal/2.2
 GDAL binary built with GEOS: TRUE 
 Loaded PROJ.4 runtime: Rel. 4.9.2, 08 September 2015, [PJ_VERSION: 492]
 Path to PROJ.4 shared files: (autodetected)
 Linking to sp version: 1.3-1 
> library("magrittr")

Attaching package: ‘magrittr’

The following object is masked from ‘package:raster’:

    extract

> library("geojsonio")

Attaching package: ‘geojsonio’

The following object is masked from ‘package:base’:

    pretty

> args = commandArgs(trailingOnly=TRUE)
> 
> if (length(args)==0) {
+   stop("At least one argument must be supplied (input file).n", call.=FALSE)
+ }
Error: At least one argument must be supplied (input file).n
Execution halted
