source("00_libraries.R")

# 01 Import data
# 02 Get Features

# 03 Train Model
load("data-intermediate/rfmodel_1961.rds")
load("data-intermediate/rfmodel_1999.rds")

options(ranger.num.threads = 128)


################################################################################
## Run model on all raster
################################################################################

tifs_g_path <- list.files("data-out/prepared1", "\\.tif$", full.names = TRUE)
tifs_rgb_path <- list.files("data-out/prepared3", "\\.tif$", full.names = TRUE)

names(tifs_g_path) <- basename(tifs_g_path) |> str_remove(".tif")
names(tifs_rgb_path) <- basename(tifs_rgb_path) |> str_remove(".tif")

tifs_g <- map(tifs_g_path, rast)
tifs_rgb <- map(tifs_rgb_path, rast)





options(ranger.num.threads = 128)

imap(tifs_g, \(x,y){
  names(x) <- ""
  x_stack <- get_features(x)
  x_pred <- predict(x_stack, rfmodel_1961,  pfun, na.rm = TRUE)
  x_pred2 <- raster2bool_2(x_pred, "Wald")  
  writeRaster(x_pred2, glue("data-intermediate/predicted/{y}.tif"), datatype="INT1U", overwrite = TRUE)
}, .progress = TRUE)

imap(tifs_rgb, \(x,y){
  # browser()
  names(x) <- c("r","g","b")
  x_stack <- get_features_rgb(x)
  x_pred <- predict(x_stack, rfmodel_1999,  pfun, na.rm = TRUE)
  x_pred2 <- raster2bool_2(x_pred, "Wald")  
  writeRaster(x_pred2, glue("data-intermediate/predicted/{y}.tif"), datatype="INT1U", overwrite = TRUE)
}, .progress = TRUE)

