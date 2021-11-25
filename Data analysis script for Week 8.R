
rm(list = ls())
gc()
dev.off()

library("raster")
library("tmap")
library("sp")
library("sf")
library("classInt")

setwd("/Users/anwarmusah/Documents/GITHUB/GEOG0114-PSA-WK8/datasets")

# load the shapefiles
kenya_border <- st_read("Shapefiles/Kenya_Border_3857.shp")
kenya_states <- st_read("Shapefiles/Kenya_States_3857.shp")

# load the raster data
temp <- raster("Rasters/Kenya Mean Teperature.tif")
nvdi <- raster("Rasters/Kenya NDVI.tif")
prec <- raster("Rasters/Kenya Precipitation.tif")
popl <- raster("Rasters/Kenya Population Density.tif")
elev <- raster("Rasters/Kenya Elevation.tif")
arid <- raster("Rasters/Kenya Aridity Index.tif")
# ignore these stupid warning message to my understanding its has something to do with an upgrade related proj4 to proj6
# cannot for the life of me figure out a way to mute these fucking warning message

# standardize the extents using the border shape file
RasterTemplate <- raster(nrow = dim(temp)[1], ncol = dim(temp)[2], crs=crs(temp), extent(temp))

popl_1km <- resample(popl, RasterTemplate, method = "bilinear")
arid_1km <- resample(arid, RasterTemplate, method = "bilinear")

# temp
temp_cl <- c(temp@data@min-1, 15, 0, 15, temp@data@max+1, 1)
temp_cl_mat <- matrix(temp_cl, ncol = 3, byrow = TRUE); temp_cl_mat
temp_recl <- reclassify(temp, temp_cl_mat)

# nvdi
nvdi_cl <- c(nvdi@data@min-1, 0.5, 0, 0.5, nvdi@data@max+1, 1)
nvdi_cl_mat <- matrix(nvdi_cl, ncol = 3, byrow = TRUE); nvdi_cl_mat
nvdi_recl <- reclassify(nvdi, nvdi_cl_mat)

# nvdi
prec_cl <- c(prec@data@min-1, 350, 0, 350, prec@data@max+1, 1)
prec_cl_mat <- matrix(prec_cl, ncol = 3, byrow = TRUE); prec_cl_mat
prec_recl <- reclassify(prec, prec_cl_mat)

# popl
popl_cl <- c(popl_1km @data@min-1, 0, 0, 0, popl_1km @data@max+1, 1)
popl_cl_mat <- matrix(popl_cl, ncol = 3, byrow = TRUE); popl_cl_mat
popl_recl <- reclassify(popl_1km , popl_cl_mat)

# elev
elev_cl <- c(elev@data@min-1, 1200, 1, 1200, elev@data@max+1, 0)
elev_cl_mat <- matrix(elev_cl, ncol = 3, byrow = TRUE); elev_cl_mat
elev_recl <- reclassify(elev, elev_cl_mat)

# arid
arid_cl <- c(arid_1km@data@min-1, 0.20, 0, 0.20, arid_1km@data@max+1, 1)
arid_cl_mat <- matrix(arid_cl, ncol = 3, byrow = TRUE); arid_cl_mat
arid_recl <- reclassify(arid_1km, arid_cl_mat)

# Combined
RasterStack <- stack(temp_recl, nvdi_recl, prec_recl, elev_recl, popl_recl, arid_recl)
SummedCriteria <- calc(RasterStack, sum)

tm_shape(SummedCriteria) + tm_raster(style = "cat", title = "Suitability score",
																		 palette=c("#FDFEFE", "#FADBD8", "#F5B7B1", "#F1948A", "#E74C3C"),
																		 labels=c("Very low (2)", "Low (3)", "Medium (4)", "High (5)", "High Very (6)")) +
	tm_shape(kenya_states) + tm_polygons(alpha = 0, border.col = "black") +
	tm_layout(frame = FALSE, legend.outside = TRUE)

MultiplyCriteria <- temp_recl*nvdi_recl*prec_recl*elev_recl*popl_recl*arid_recl

tm_shape(MultiplyCriteria) + tm_raster(style = "cat", title = "", palette=c("white", "red"), labels=c("", "Zone: Highly Suitable")) +
	tm_shape(kenya_states) + tm_polygons(alpha = 0, border.col = "black") +
	tm_layout(frame = FALSE, legend.outside = TRUE)

# take the original rasters and standardised all of them on a same scale from 0-1

ztemp <- (temp - temp@data@min) / (temp@data@max - temp@data@min)
zprec <- (prec - prec@data@min) / (prec@data@max - prec@data@min)
zelev <- (elev - elev@data@min) / (elev@data@max - elev@data@min)
znvdi <- (nvdi - nvdi@data@min) / (nvdi@data@max - nvdi@data@min)
zpopl <- (popl_1km - popl_1km@data@min) /(popl_1km@data@max - popl_1km@data@min)
zarid <- (arid_1km - arid_1km@data@min) /(arid_1km@data@max - arid_1km@data@min)

rzelev <- spatialEco::raster.invert(zelev)

suitablemap <- 0.3557*ztemp + 0.3717*zprec + 0.1585*zpopl + 0.0659*rzelev + 0.0901*znvdi + 0.0767*zarid

suitablemap <- suitablemap-suitablemap@data@min

# creating my own color scheme because I f**king hate the preset colours in R. 
cool = rainbow(50, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('blue'))[1])
warm = rainbow(50, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(warm))
mypalette <- colorRampPalette(cols)(255)

tm_shape(suitablemap) + tm_raster(style = "cont", title = "AHP Suitability", palette = colorRampPalette(cols)(3) , midpoint = NA, labels =c("Low", "Medium", "High")) +
tm_shape(kenya_states) + tm_polygons(alpha = 0, border.col = "black") +
tm_layout(frame = FALSE, legend.outside = TRUE)

# quantile(raster, )
# palette= "-RdYlBu"
