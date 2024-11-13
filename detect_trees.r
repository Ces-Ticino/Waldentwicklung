

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
  names(ras_foc) <- paste(prefix, fun,radius, sep = "_")
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

normalize_zscore <- \(ras){
  ras_mean <- as.matrix(global(ras,"mean", na.rm = TRUE))[1,]
  ras_sd <- as.matrix(global(ras,"sd", na.rm = TRUE))[1,]
  
  (ras-ras_mean)/ras_sd
}

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

st_write(ces, "data-out/ces.gpkg", "aoi",append = FALSE)


tm_shape(ces) + tm_borders()



r1961_full <- rast("data-out/1961.tif")

r1961 <- mask(r1961_full, vect(ces))
# plot(r1961)

# ces_bb <- llur2poly(2706340.286,1143997.584,2706452.327,1144104.838)

# ces_bb <- llur2poly(2706224,1143730,2706566,1143982)



# tm_shape(r1961) + tm_raster()




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
  
  ras_stack <- rast(c(focal_ras, roughness))
  # browser()
  normalize_minmax(ras_stack)
  }


r1961_stack <- get_features(r1961, 3, progress = TRUE)
# hist(r1961_stack)
r1961_stack <- rast("data-tmp/r1961_stack.tif")


plot(r1961_stack)


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


# writeRaster(r1961_stack, filename = paste0("data-out/tmp/features/", names(r1961_stack), ".tif"),overwrite = TRUE)



################################################################################
## Extract features to points and split data
################################################################################

train_test_df <- extract2(r1961_stack, train_test_sf) |> 
  select(-year) |> 
  mutate(class = as.factor(class)) |> 
  cbind(st_coordinates(train_test_sf)) # this is for mlr3 and might be an issue for other approaches (formula: class~.)


train_test_df <- train_test_df |> 
  na.omit() |> 
  group_by(class) |> 
  mutate(train = train_test(n(),.75)) |> 
  ungroup()


# the number of samples class should be propotional to the expected
# amount of area per class
fct_count(train_test_df$class,prop = TRUE)

train <- train_test_df |> 
  filter(train) |> 
  select(-train)

test <- train_test_df |> 
  filter(!train)|> 
  select(-train)

nrow(train)/nrow(train_test_df)



# plot(train_test_df)

################################################################################
## Make model (mlr3)
################################################################################
library(mlr3)               # unified interface to machine learning algorithms
library(mlr3learners)       # most important machine learning algorithms
library(mlr3extralearners)  # access to even more learning algorithms
library(mlr3spatiotempcv)   # spatio-temporal resampling strategies
library(mlr3tuning)         # hyperparameter tuning
library(mlr3viz)            # plotting functions for mlr3 objects
library(mlr3spatial)

# 1. create task (mlr3spatiotempcv)
task = as_task_classif_st(
  train, 
  target = "class", 
  id = "wald",
  # positive = "wald",
  coordinate_names = c("X", "Y"),
  crs = "EPSG:2056",
  coords_as_features = FALSE
)

# 2a: Choose a learner:
learners_df <- mlr3extralearners::list_mlr3learners(
  filter = list(
    class = "classif", 
    properties = "multiclass"
    # properties = "twoclass"
    ),
  # select = c("id", "mlr3_package", "required_packages")
  )

learners_df

# 2b. specify learner
# learner = lrn("classif.log_reg", predict_type = "prob")
learner = lrn("classif.rpart", predict_type = "prob")




# 3. specify resampling
# repeats was 100, took too long for rf
resampling = mlr3::rsmp("repeated_spcv_coords", folds = 5, repeats = 100)


# run spatial cross-validation and save it to resample result glm (rr_glm)
rr <- mlr3::resample(
  task = task,
  learner = learner,
  resampling = resampling
  )


rr$score(msr("classif.ce"))

pred <- rr$prediction()

pred$confusion

learner$train(task)
predict(learner, test)

# prediction does not seem to work when the raster is in memory
writeRaster(r1961_stack, "data-tmp/r1961_stack.tif", overwrite = TRUE)
r1961_stack <- rast("data-tmp/r1961_stack.tif")

pred_spat <- predict_spatial(r1961_stack, learner)

writeRaster(pred_spat, "data-tmp/r1961_pred.tif")

pred_vec <- prediction2polygon(pred_spat)

st_write(pred_vec, "data-tmp/predict.gpkg", "1961",append = FALSE)

pred$confusion



################################################################################
## Run model on all grayscales
################################################################################


tifs <- list.files("data-out", "\\.tif$", full.names = TRUE)

library(stringr)
names(tifs) <- basename(tifs) |> str_remove(".tif")

tifs_r <- map(tifs, rast)

nlyrs <- map_int(tifs_r, nlyr)

tifs_g <- tifs_r[nlyrs == 1]



imap(tifs_g, \(x,y){
  
  x2 <- mask(x, vect(ces))
  
  x_stack <- get_features(x2)
  writeRaster(x_stack, glue("data-intermediate/grayscale_features/{y}.tif"), overwrite = TRUE)
  
}, .progress = TRUE)


