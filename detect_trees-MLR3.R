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


