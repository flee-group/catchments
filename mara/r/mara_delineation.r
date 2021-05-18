library(watershed)
library(raster)
library(sf)
library(ggplot2)

## eventaully get this from the DB
# library(fleeDB)
# library(keyring)
# pw = key_get("fleedb-login-matt")
# un = "c7701209"
# fdb = dbConnect(Postgres(), host = host, user = un,
#	password = pw, dbname = dbname)
# mara = fleedb_raster(fdb, "mara_dem")
mara = raster("~/work/students/Lukas/dem_aea.tif")

## lukas had some catchment areas incorrect
## swap catchment area for 42 and 43; done for the eduts
## same for 31 and 32
# sites = readRDS("data/sites.rds")
sites = readRDS("data/sites_edits.rds")

# mara2 = projectRaster(mara, crs="+init=epsg:32736")

outlet = c(1040958, -154588)
# outlet2 = SpatialPoints(matrix(c(1040958, -154588), nrow=1))
# crs(outlet2) = crs(mara)
# outlet2 = spTransform(outlet2, crs(mara2))
# mara_r = delineate(mara2, threshold = 1e6, outlet = coordinates(outlet2)[1,])
mara_r = delineate(mara, threshold = 1e6, outlet = outlet)

Tp = pixel_topology(mara_r)


options(mc.cores = 5)
mara_vect = vectorise_stream(mara_r$stream, Tp)

st_write(mara_vect, "data/mara.shp")

ggplot(mara_vect) + geom_sf(col='blue') + 
	geom_sf(data = sites, size=0.7, col='red')

## prep coordinates for catchment area
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



## also get for sites
ca = catchment(mara_r, type='points', y = st_coordinates(sites), area = TRUE, Tp = Tp)




# library(WatershedTools)
# mara_ws = Watershed(mara_r$stream, mara_r$drainage, mara, mara_r$accum)

