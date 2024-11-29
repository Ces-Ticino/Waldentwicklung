################################################################################
## Libraries
################################################################################

library(terra)
library(mapview)
library(sf)
library(dplyr)
library(purrr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(glue)
library(tidyr)
library(forcats)
library(tmap)
library(stringr)
# library(rpart)
library(ranger)     # ← current method


################################################################################
## Functions
################################################################################



llur2poly <- \(xmin,ymin,xmax,ymax, crs = 2056){
  st_bbox(c(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax
  )) |>
    st_as_sfc() |> 
    st_set_crs(crs)
}


focal_circle <- function(ras, radius, fun = "mean"){
  # browser()
  focmat <- focalMat(ras, radius, type="circle",fillNA = TRUE)
  ras_foc <- focal(ras,focmat, fun)
  names(ras_foc) <- paste0(names(ras), paste(fun,radius, sep = "_"))
  ras_foc
}




raster_bool2poly <- function(ras_bool){
  ras_bool[!ras_bool] <- NA
  ras_poly <- st_as_sf(as.polygons(ras_bool,values = FALSE))
  st_cast(ras_poly, "POLYGON")
}


normalize_minmax <- \(ras){
  mm <- minmax(ras)
  
  (ras-mm[1,])/(mm[2,]- mm[1,])
}

# To use this function, make it work for multiple rasters
# (similar to normalize_minmax)
# normalize_zscore <- \(ras){
#   ras_mean <- as.matrix(global(ras,"mean", na.rm = TRUE))[1,]
#   ras_sd <- as.matrix(global(ras,"sd", na.rm = TRUE))[1,]
#   
#   (ras-ras_mean)/ras_sd
# }

train_test <- function(len, prop){
  sample(c(TRUE,FALSE), size = len, prob = c(prop, 1-prop),replace = TRUE)
}


prediction2polygon <- \(ras, thresh = NULL, values = TRUE){
  
  if(!is.null(thresh)){
    ras <- ras > thresh
    ras[!ras] <- NA
  }
  
  
  ras |> 
    as.polygons(values = is.null(thresh)) |> 
    st_as_sf() |> 
    st_cast("POLYGON",warn = FALSE)
}


extract2 <- \(ras, vec, pred_col = "class"){
  extracted <- terra::extract(ras, vec, ID = FALSE)
  
  cbind(vec, extracted) |> 
    st_drop_geometry() |> 
    as_tibble()
}


vec2poly <- \(x, crs, byrow = TRUE){
  x |> 
    matrix(ncol = 2,byrow = byrow) |> 
    list() |> 
    st_polygon() |> 
    st_sfc(crs = crs) |> 
    st_as_sf() |> 
    st_geometry("geom")
  
}