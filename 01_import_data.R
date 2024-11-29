################################################################################
## Setting up the env
################################################################################

source("00_libraries.R")



# st_write(ces, "data-out/ces.gpkg", "aoi",append = FALSE)


tm_shape(ces) + tm_borders()



r1961_full <- rast("data-out/prepared1/1961.tif")
r1999_full <- rast("data-out/prepared3/1999.tif")


names(r1961_full) <- ""
names(r1999_full) <- c("r","g","b")
