stop("Finished 20.05.2021")

library(watershed)
library(raster)
library(sf)
library(ggplot2)
library(data.table)
mara = raster("/Volumes/flee/spatial/catchments/mara/mara_dem_102022.tif")

## lukas had some catchment areas incorrect
## swap catchment area for 42 and 43; done for the eduts
## same for 31 and 32
# sites = readRDS("data/sites.rds")
sites = readRDS("data/sites_edits.rds")


## define outlet and delineate stream
outlet = c(1040958, -154588)
mara_r = delineate(mara, threshold = 1e6, outlet = outlet)
Tp = pixel_topology(mara_r)

options(mc.cores = 5)
mara_vect = vectorise_stream(mara_r$stream, Tp)

pl = ggplot(mara_vect) + geom_sf(aes(colour = reach_id)) + 
	geom_sf(data = sites, size=0.7, col='red')
if(interactive) print(pl)

## prep coordinates for catchment area
## Catchment area
## ## WARNING
## big computation, around 48h to finish
pts = as.data.frame(rasterToPoints(mara_r$stream))
pts$ca = NA
nr = nrow(pts)
reaches = unique(pts$stream)
for(r in reaches) {
	i = which(pts$stream == r)
	pts$ca[i] = catchment(mara_r, type="points", y = as.matrix(pts[i, 1:2]), area = TRUE, Tp = Tp)
	dn = sum(!is.na(pts$ca))
	cat(paste0(Sys.time(), "  ", dn, "/", nr, " (", round(100 * dn/nr, 0), "%)", "\r"))
}

caras = rasterFromXYZ(pts[,c(1,3,4)])
sites$catchment_watershed = extract(caras, sites)/(1000^2)
discharge = fread("data/site_discharge.csv")

sites = merge(sites, discharge, by.x = "Site_ID", by.y = "site", all.x = TRUE)
sites = subset(sites, !is.na(catchment_watershed))

qtab = hydraulic_geometry(sites$catchment_watershed, sites$q, pts$ca)
geom = cbind(pts, qtab)
geom$ca <- NULL
geom$stream <- NULL

hydro = geom
coordinates(hydro) = c('x', 'y')
crs(hydro) = crs(mara_r)
gridded(hydro) = TRUE
hydro = stack(hydro)

## write files to results - do not use tif, it doesn't support layer names
hydro = writeRaster(hydro, "res/hydraulic_geometry.grd")
mara_r = crop(mara_r, hydro, filename = "res/mara_delineation.grd", overwrite = TRUE)
geom = as.data.table(geom)
saveRDS(geom, "res/hydraulic_geom_tab.rds")
mara_pts = data.table(rasterToPoints(mara_r))
saveRDS(mara_pts, "res/mara_delineation_tab.rds")


## create and save watershed
library(WatershedTools)
mara_ws = Watershed(mara_r$stream, mara_r$drainage, mara, mara_r$accum, hydro$CA, 
	dropLayer(hydro, which(names(hydro) == "CA")))
saveRDS(mara_ws, "res/mara_watershed.rds")


## clean up and save sites
sites$id <- NULL
sites$catchment_area <- NULL
saveRDS(sites, "res/mara_sites_final.rds")

dat = as.data.table(mara_ws$data)[, .(log_ca = log(max(catchmentArea)), Q = max(Q)), by=vReachNumber]
ggplot(merge(mara_vect, dat, by.x = "reach_id", by.y = "vReachNumber")) + geom_sf(aes(colour=Q)) + scale_colour_viridis_c()
