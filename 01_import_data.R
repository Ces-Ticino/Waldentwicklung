################################################################################
## Setting up the env
################################################################################

source("00_libraries.R")

ces <- vec2poly(c(
  2707294, 1143470,
  2706034, 1144930,
  2704954, 1144050,
  2706259, 1142535,
  2707294, 1143470
  
), crs = 2056)

# st_write(ces, "data-out/ces.gpkg", "aoi",append = FALSE)


tm_shape(ces) + tm_borders()



r1961_full <- rast("data-out/prepared1/1961.tif")
r1999_full <- rast("data-out/prepared1/1999.tif")


names(r1961_full) <- ""
names(r1999_full) <- c("r","g","b")
