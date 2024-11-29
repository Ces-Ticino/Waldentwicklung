
################################################################################
## Run model on all grayscales
################################################################################

tifs <- list.files("data-out/prepared1/", "\\.tif$", full.names = TRUE)

library(stringr)
names(tifs) <- basename(tifs) |> str_remove(".tif")

tifs_r <- map(tifs, rast)

nlyrs <- map_int(tifs_r, nlyr)

tifs_g <- tifs_r[nlyrs == 1]



imap(tifs_g, \(x,y){
  # browser()
  x2 <- mask(x, vect(ces))
  
  x_stack <- get_features(x2, radii = radii)
  # x_pred <- predict(x_stack, rfmodel,  na.rm=TRUE)
  x_pred <- predict(x_stack, rfmodel,  pfun, na.rm = TRUE)
  
  
  x_pred2 <- raster2bool_2(x_pred, 3)
  
  writeRaster(x_pred2, glue("data-intermediate/grayscale_predicted_rf/{y}.tif"), datatype="INT1U", overwrite = TRUE)
  
}, .progress = TRUE)



