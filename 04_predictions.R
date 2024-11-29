source("00_libraries.R")

# 01 Import data
# 02 Get Features
r1961_stack <- rast("data-intermediate/r1961_stack.tif")

# 03 Train Model
load("data-intermediate/rfmodel_1961.rds")
load("data-intermediate/rfmodel_1961_small.rds")



################################################################################
## Predictions
################################################################################


# r1961_pred <- predict(r1961_stack, cartmodel,  na.rm=TRUE)

pfun <- \(...) {
  predict(...)$predictions
}
r1961_pred <- predict(r1961_stack, rfmodel_1961_small,  pfun, na.rm = TRUE)
# r1961_pred <- mask(r1961_pred, vect(ces))

# plot(r1961_pred)

# writeRaster(r1961_pred[["wald"]], "data-tmp/pred_wald.tif")


# this funtion is used by the CART model
# raster2bool <- \(ras, name, thresh = .8){
#   ras <- ras[[name]]
#   
#   ras[ras < thresh] <- NA
#   ras[!is.na(ras)] <- 1
#   as.bool(ras)
# }

raster2bool_2 <- \(ras, val){
  ras[ras != val] <- NA
  ras
}

r1961_wald <- raster2bool_2(r1961_pred, 3)

# r1961_pred2 <- raster2bool(r1961_pred, "Wald")

plot(r1961_wald)

################################################################################
## Clean predictions
################################################################################

# builidings_mask <- read_sf("data/ces.gpkg", "buildings_mask")

# r1961_wald <- mask(r1961_wald, builidings_mask, inverse = TRUE)



