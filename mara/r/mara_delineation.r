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
sites = readRDS("data/sites.rds")


outlet = c(1040958, -154588)
mara_r = delineate(mara, threshold = 1e6, outlet = outlet)

Tp = pixel_topology(mara_r)


options(mc.cores = 5)
mara_vect = vectorise_stream(mara_r$stream, Tp)

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
