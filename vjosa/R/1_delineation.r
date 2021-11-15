library(watershed)
library(raster)
library(sf)
library(ggplot2)
library(data.table)
library(units)

sites = st_read(fdb, Id(schema = "spatial", table = "sites_view"))
sites = sites[sites$watershed == "vjosa",]
sites = sites[1:48,] # drop extra sites

dem = raster("/Volumes/flee/spatial/elevation/vjosa_25m/vjosa_dem25.tif")
# store it locally for working
dem = writeRaster(dem, paste0(tempfile(), ".tif"))

vjosa_r = delineate(dem, threshold = 2e7, outlet = NA)
Tp = pixel_topology(vjosa_r)

options(mc.cores = 4)
vjosa_v = vectorise_stream(vjosa_r, Tp)

pl = ggplot(vjosa_v) + geom_sf(colour = 'blue') + geom_sf(data = sites, colour = 'red')
if(interactive()) print(pl)

pts = as.data.frame(rasterToPoints(vjosa_r$stream))
pts$ca = NA
nr = nrow(pts)
reaches = unique(pts$stream)

options(mc.cores = NULL) # catchment area doesn't work parallel
for(r in reaches) {
	i = which(pts$stream == r)
	pts$ca[i] = catchment(vjosa_r, type="points", y = as.matrix(pts[i, 1:2]), area = TRUE, Tp = Tp)
	dn = sum(!is.na(pts$ca))
	cat(paste0(Sys.time(), "  ", dn, "/", nr, " (", round(100 * dn/nr, 0), "%)", "\r"))
}



# get discharge data
q = paste("select site_visits.expedition_id, sites_view.site_id, primary_name as site_name,",
	"discharge_m3_per_s, catchment_area_km2, geometry",
	"from fluflux.hydromorphology",
	"left join fluflux.site_visits on hydromorphology.site_visit_id = site_visits.site_visit_id",
	"left join spatial.sites_view on site_visits.site_id = sites_view.site_id",
	"where sites_view.watershed = 'vjosa' and discharge_m3_per_s is not null",
	"order by expedition_id, site_name")

## soon
# q_sites = st_read(fdb, Id(schema="fluflux", table = "measured_discharge"))
q_sites = st_read(fdb, query = q)
qtab = by(q_sites, q_sites$expedition_id, function(x) 
	hydraulic_geometry(x$catchment_area_km2*1000^2, x$discharge_m3_per_s, pts$ca),
simplify = FALSE)

# ggplot(qtab) + geom_point(aes(x=log(units::drop_units(CA)), y = log(units::drop_units(Q)), colour = expedition_id)) + 
	# geom_point(data = q_sites, aes(x = log(catchment_area_km2*1000^2), y = log(discharge_m3_per_s), colour = factor(expedition_id)), pch = 2)





####
## pixel data
####
pdat = rasterToPoints(vjosa_r)
pdat = pdat[complete.cases(pdat),]
pdat = data.table(pdat, key = c('x', 'y'))
pts = data.table(pts, key = c('x', 'y'))
pdat = pts[pdat, .(x, y, pix_id = id, reach_id = stream, catchment_area_km2 = ca/1000^2, drainage)]

####
## pixel-time data
####
ptdat = lapply(qtab, function(x) cbind(x, pts[, c('x', 'y')]))
ptdat = rbindlist(ptdat, id = "expedition_id")
setkey(ptdat, x, y)
ptdat = pdat[ptdat, .(pix_id, expedition_id, discharge_m3_per_s = drop_units(Q), 
	velocity_m_per_s = drop_units(v), depth_m = drop_units(z), width_m = drop_units(w))]
pdat = st_as_sf(pdat, coords = c('x', 'y'), crs = crs(vjosa_v))
pdat$elevation_m = extract(dem, pdat)


####
## topologies
####
Tp_r = reach_topology(vjosa_r, Tp)


####
## vector data
####
colnames(vjosa_v)[2] = "geometry"
st_geometry(vjosa_v) = "geometry"
vjosa_v = vjosa_v[order(vjosa_v$reach_id),]
vjosa_v$catchment_area_km2 = tapply(pdat$catchment_area_km2, pdat$reach_id, max) # comes out sorted


## write to files
sppath = file.path("/", "Volumes", "flee", "spatial", "catchments")
vjpath = file.path(sppath, "vjosa", "1.2.0")
saveRDS(vjosa_v, file.path(vjpath, "vjosa_vector.rds"))
saveRDS(pdat, file.path(vjpath, "vjosa_pdat.rds"))
saveRDS(ptdat, file.path(vjpath, "vjosa_ptdat.rds"))
saveRDS(Tp, file.path(vjpath, "vjosa_Tp.rds"))
saveRDS(Tp_r, file.path(vjpath, "vjosa_Tr.rds"))





