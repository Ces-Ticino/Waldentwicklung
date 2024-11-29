source("00_libraries.R")

# 01 Import data
# 02 Get Features
r1961_stack <- rast("data-intermediate/r1961_stack.tif")
r1999_stack <- rast("data-intermediate/r1999_stack.tif")

# 03 Train Model
load("data-intermediate/rfmodel_1961.rds")
load("data-intermediate/rfmodel_1999.rds")



################################################################################
## Predictions
################################################################################


# r1961_pred <- predict(r1961_stack, cartmodel,  na.rm=TRUE)
# ranger 0.17.0 using 2 threads (default).
# Change with num.threads in ranger() and predict(), 
# options(Ncpus = N), options(ranger.num.threads = N) or environment variable R_RANGER_NUM_THREADS.

options(ranger.num.threads = 128)

r1961_pred <- predict(r1961_stack, rfmodel_1961,  pfun, na.rm = TRUE)
r1999_pred <- predict(r1999_stack, rfmodel_1961,  pfun, na.rm = TRUE)

# r1961_pred <- mask(r1961_pred, vect(ces))

# plot(r1961_pred)

# writeRaster(r1961_pred[["Wald"]], "data-intermediate/1961.tif", overwrite = TRUE)


# this funtion is used by the CART model
# raster2bool <- \(ras, name, thresh = .8){
#   ras <- ras[[name]]
#   
#   ras[ras < thresh] <- NA
#   ras[!is.na(ras)] <- 1
#   as.bool(ras)
# }

raster2bool_2 <- \(ras, class){
  lvs <- levels(ras)[[1]]
  val <- lvs$value[lvs$class == class]
  stopifnot(length(val) == 1)
  ras[ras != val] <- NA
  ras
}

r1961_wald <- raster2bool_2(r1961_pred, "Wald")
r1999_wald <- raster2bool_2(r1999_pred, "Wald")




################################################################################
## Clean predictions
################################################################################

# builidings_mask <- read_sf("data/ces.gpkg", "buildings_mask")

# r1961_wald <- mask(r1961_wald, builidings_mask, inverse = TRUE)



