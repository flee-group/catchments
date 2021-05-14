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
sites = readRDS("data/sites_edits.shp")

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

st_write(mara_vect, "mara.shp")
st_write(sites, "mara_sites.shp")

ggplot(mara_vect) + geom_sf(col='blue') + 
	geom_sf(data = sites, size=0.7, col='red') + 
	geom_sf(data = sites_sn, size=0.7, col='yellow')

## only needed for sites
options(mc.cores = 1)
ca = catchment(mara_r, type='points', y = st_coordinates(sites), area = TRUE, Tp = Tp)

## a few points fall outside, need to fix these
sites$ca_grass = sapply(ca, function(x) ifelse(length(x) == 0, NA, x)) / (1000^2)


library(WatershedTools)
mara_ws = Watershed(mara_r$stream, mara_r$drainage, mara, mara_r$accum)
