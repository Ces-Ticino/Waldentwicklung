

################################################################################
## Functions and Libraries
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


focal_circle <- function(ras, radius, fun = "mean", prefix = ""){
  focmat <- focalMat(ras, radius, type="circle",fillNA = TRUE)
  ras_foc <- focal(ras,focmat, fun)
  names(ras_foc) <- paste(names(ras),prefix, fun,radius, sep = "_")
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


################################################################################
## Setting up the env
################################################################################



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


r1961 <- mask(r1961_full, vect(ces))
r1999 <- mask(r1999_full, vect(ces))



################################################################################
## Feature engineering
################################################################################


get_features <- function(
    ras, 
    radii = 5, 
    funs = c("sd", "mean", "max", "min"), 
    
    progress = FALSE){
  # browser()
  
  focal_ras <- expand_grid(radii, funs) |> 
    pmap(\(radii, funs){
      focal_circle(ras, radii, funs, prefix = "ras")
    }, .progress = progress)
  
  roughness <- terrain(ras, "roughness")
  
  # focal_rough <- expand_grid(radii, funs) |> 
  #   pmap(\(radii, funs){
  #     focal_circle(roughness, radii, funs, "roughness")
  #   }, .progress = progress)
  
  
  
  # fx=matrix(c(-1,-2,-1,0,0,0,1,2,1), nrow=3)
  # fy=matrix(c(1,0,-1,2,0,-2,1,0,-1), nrow=3)
  

  # sobels <- list(fx = fx, fy = fy)
  
  # ras_edges <- imap(sobels, \(x,y){
  #   ou <- focal(ras, x) |> 
  #     normalize_zscore()
  #   
  #   ou2 <- ou>2 | ou < -2
  # }) |> 
  #   rast() |> 
  #   max() |> 
  #   distance(target = 0)
  
  # names(ras_edges) <- "edges_dist"
  
  names(ras) <- "original"
  ras_stack <- rast(c(focal_ras, roughness, ras))
  normalize_minmax(ras_stack)
  }


radii <- c(3, 5, 10)

r1961_stack <- get_features(r1961_full, radii, progress = TRUE)
r1999_stack <- get_features(r1999_full, radii, progress = TRUE)
## Continue here! (do the same for 1999+)

################################################################################
## Import Train / Test Data
################################################################################
st_layers("data/training.gpkg")$name -> lyrs



train_test_sf <- lapply(lyrs, \(x){
  st_read("data/training.gpkg", x) |> 
    mutate(year_class = x) |> 
    st_set_geometry("geom")
  
}) |> 
  do.call(rbind, args = _) |> 
  separate_wider_delim(year_class, "_",names = c("year", "class"), too_many = "merge") |> 
  st_sf()


unique(train_test_sf$class)




################################################################################
## Extract features to points and split data
################################################################################

train_test_sf <- train_test_sf |> 
  mutate(class = as.factor(class))

train_test_1961 <- train_test_sf |> 
  filter(year == 1961) |>
  select(-year)

train_test_1999 <- train_test_sf |> 
  filter(year == 1999) |>
  select(-year)

# repeat for 1999 from here
train_test_df <- extract2(r1961_stack, train_test_sf) 

# train_test_df <- train_test_df |> 
#   na.omit() |> 
#   group_by(class) |> 
#   mutate(train = train_test(n(),.75)) |> 
#   ungroup()

ggplot(train_test_df, aes(ras_sd_3, ras_mean_3)) +
  geom_point(aes(color = class)) 

# the number of samples class should be propotional to the expected
# amount of area per class
fct_count(train_test_df$class,prop = TRUE)

# train <- train_test_df |> 
#   filter(train) |> 
#   select(-train)

# test <- train_test_df |> 
#   filter(!train)|> 
#   select(-train)

# nrow(train)/nrow(train_test_df)

################################################################################
## Train the model
################################################################################

library(rpart)
library(ranger)     # ← current method

tm_shape(r1961_full) + 
  tm_raster() +
  tm_shape(train_test_sf) +
  tm_dots()



# cartmodel <-  rpart(class~., data = train_test_df)
rfmodel <-  ranger(class~., data = train_test_df)


# r1961_pred <- predict(r1961_stack, cartmodel,  na.rm=TRUE)

pfun <- \(...) {
  predict(...)$predictions
}
r1961_pred <- predict(r1961_stack, rfmodel,  pfun, na.rm = TRUE)
# r1961_pred <- mask(r1961_pred, vect(ces))

# plot(r1961_pred)

# writeRaster(r1961_pred[["wald"]], "data-tmp/pred_wald.tif")


raster2bool <- \(ras, name, thresh = .8){
  ras <- ras[[name]]
  
  ras[ras < thresh] <- NA
  ras[!is.na(ras)] <- 1
  as.bool(ras)
}

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



