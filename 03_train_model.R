
source("00_libraries.R")

# 01 Import data
# 02 Get Features

r1961_stack <- rast("data-intermediate/r1961_stack.tif")
r1999_stack <- rast("data-intermediate/r1999_stack.tif")

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


table(train_test_sf$class, train_test_sf$year)




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


train_test_1961_df <- extract2(r1961_stack, train_test_1961) 
train_test_1999_df <- extract2(r1999_stack, train_test_1999) 

################################################################################
## Train the model
################################################################################



# cartmodel <-  rpart(class~., data = train_test_df)
rfmodel_1961 <-  ranger(
  class~., 
  data = train_test_1961_df, 
  importance = "permutation" # or "impurity"
)

rfmodel_1999 <-  ranger(
  class~., 
  data = train_test_1999_df, 
  importance = "permutation" # or "impurity"
)

save(rfmodel_1961, file = "data-intermediate/rfmodel_1961.rds")
save(rfmodel_1999, file = "data-intermediate/rfmodel_1999.rds")


################################################################################
## Variable importance
################################################################################

importance_values <- rfmodel_1961$variable.importance

importance_df <- tibble(
  Feature = names(importance_values),
  Importance = importance_values
)

importance_df <- arrange(importance_df, desc(Importance))

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Feature Importance",
    x = "Features",
    y = "Importance"
  )

################################################################################
## Train a smaller model
################################################################################



# rfmodel_1961_small <-  ranger(
#   class~., 
#   data = train_test_1961_df[,c("class", importance_df$Feature[1:7])], 
#   importance = "permutation" # or "impurity"
# )


# save(rfmodel_1961_small, file = "data-intermediate/rfmodel_1961_small.rds")




