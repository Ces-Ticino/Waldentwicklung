################################################################################
## Feature engineering
################################################################################

source("00_libraries.R")


get_features <- function(
    ras, 
    radii = 5, 
    funs = c("sd", "mean", "max", "min"), 
    
    progress = FALSE){
  
  focal_ras <- expand_grid(radii, funs) |> 
    pmap(\(radii, funs){
      # browser()
      
      focal_circle(ras, radii, funs)
    }, .progress = progress)
  
  names(ras) <- "original"
  ras_stack <- rast(c(focal_ras, ras))
  normalize_minmax(ras_stack)
}


radii <- c(3, 5, 10)

r1961_stack <- get_features(r1961_full, radii, progress = TRUE)
# r1999_stack <- get_features(r1999_full, radii, progress = TRUE)
## Continue here! (do the same for 1999+)



writeRaster(r1961_stack, "data-intermediate/r1961_stack.tif", overwrite = TRUE)
