source("00_libraries.R")

# 01 Import data

source("01_import_data.R")


################################################################################
## Feature engineering
################################################################################









r1961_stack <- get_features(r1961_full, progress = TRUE)


r1999_stack <- get_features_rgb(r1999_full, progress = TRUE)



writeRaster(r1961_stack, "data-intermediate/r1961_stack.tif", overwrite = TRUE)
writeRaster(r1999_stack, "data-intermediate/r1999_stack.tif", overwrite = TRUE)

