

## Libraries ###################################################################

library(sf)
library(terra)
library(dplyr)
library(tmap)
library(smoothr)
library(purrr)
## Data ########################################################################

ces <- st_read("data/ces.gpkg")
si2021 <- terra::rast("swissimage_2021.vrt")
crs(si2021) <- "epsg:2056"



## Contourlines / Höhenlinien ##################################################

swissalti_files <- list.files("data/swissalti/",pattern = "\\.tif$",full.names = TRUE)

swissalti <- sapply(swissalti_files, rast) |> 
  sprc() |> 
  mosaic()

abstand <- 5
swissalti_round <- round(swissalti/abstand)*abstand
alti_minmax <- minmax(swissalti_round)

nlevs <- (alti_minmax[2]- alti_minmax[1])/abstand
contours <- st_as_sf(as.contour(swissalti_round,nlevels = nlevs))

contours_sm <- smooth(contours,method = "ksmooth")
# contours_sm <- smooth(contours,method = "chaikin")
# contours_sm <- smooth(contours,method = "spline")
# contours_sm <- smooth(contours,method = "chaikin")


# Graticules ###################################################################

coords <- st_coordinates(ces)
xrange <- range(coords[,1])
yrange <- range(coords[,2])
x <- seq(xrange[1],xrange[2], 100)
y <- seq(xrange[1],xrange[2], 100)


bb <- st_bbox(ces)

round_to_multiple <- function(x, multiple, fun = round) fun(x/multiple)*multiple

xmin <- round_to_multiple(bb$xmin,1000,floor)
ymin <- round_to_multiple(bb$ymin,1000,floor)
xmax <- round_to_multiple(bb$xmax,1000,ceiling)
ymax <- round_to_multiple(bb$ymax,1000,ceiling)

xs <- seq(xmin,xmax,100)
ys <- seq(ymin,ymax,100)
xs_sf <-
  lapply(xs, \(x){
    ou <- matrix(c(x,ymin, x, ymax), ncol = 2, byrow = TRUE) |> 
      st_linestring() |> 
      st_sfc() |> 
      st_sf() |> 
      mutate(text = x)
    
    st_geometry(ou) <- "geom"
    
    ou
  }) |> bind_rows() |> st_set_crs(2056)

ys_sf <-
  lapply(ys, \(y){
    ou <- matrix(c(xmin,y, xmax, y), ncol = 2, byrow = TRUE) |> 
      st_linestring() |> 
      st_sfc() |> 
      st_sf() |> 
      mutate(text = y)
    
    st_geometry(ou) <- "geom"
    
    ou
  }) |> bind_rows() |> st_set_crs(2056)


# Kachelung (versuch 1) ########################################################

# 297 x 420
# 1:1000
# 420*1000/1000
kartierung <- st_make_grid(ces, cellsize = 297) 

st_coordinates(kartierung)

# grati <- st_make_grid(ces, cellsize = 50) 

kartierung <- kartierung[ces,,]



# 297 x 420

# Kachelung (versuch 2) ########################################################

kachel_w <- 420
kachel_h <- 297
kacheln <- expand.grid(x1 = seq(xmin,xmax,kachel_w-50),y1 = seq(ymin,ymax,kachel_h-50)) |> 
  pmap(function(x1,y1){
    # browser()
    x2 <- x1 + kachel_w
    y2 <- y1 + kachel_h
    ou <- c(x1,y1,
      x1,y2,
      x2,y2,
      x2,y1,
      x1,y1) |> matrix(ncol = 2,byrow = TRUE) |> 
      list() |> 
      st_polygon() |> 
      st_sfc() |> 
      st_sf()
    
    st_geometry(ou) <- "geom"
    ou
  }) |> bind_rows() |> 
  st_set_crs(2056)


kacheln <- kacheln[ces,,]

# Karten pro Kachel erstellen ##################################################



kacheln$i <- seq_len(nrow(kacheln))

si2021_2 <- terra::aggregate(si2021, fact = 10)

si2021_3 <- crop(si2021_2, kacheln)

map0 <- tm_shape(si2021_3) + tm_rgb() +
  tm_shape(kacheln |> mutate(i = as.character(i))) + tm_polygons(col = "i",alpha = .2) +
  tm_shape(kacheln) + 
  tm_text(text = "i",size = 2) +
  tm_layout(legend.show = FALSE, frame = FALSE)

tmap_save(map0, 
          "data-tmp/uebersichtskarte.jpg",
          width = 297, 
          height = 420, 
          units = "mm")

map(kacheln$i, \(kach){
  k1 <- kacheln[kach,]
  

  si1 <- crop(si2021, vect(k1))
  
  
  
  si1_bb <- ext(si1) |> 
    vect() |> 
    as("Spatial") |> 
    st_as_sfc() |> 
    st_set_crs(2056)
  
  
  xs_sf_crop <- st_intersection(xs_sf, si1_bb)
  ys_sf_crop <- st_intersection(ys_sf, si1_bb)
  x_points <- st_intersection(
    xs_sf, 
    st_cast(st_buffer(si1_bb, -10), "LINESTRING")) |> 
    st_cast("POINT") |> st_sf()
  y_points <- st_intersection(
    ys_sf, 
    st_cast(st_buffer(si1_bb, -10), "LINESTRING")) |> 
    st_cast("POINT") |> st_sf()
  
  
  
  # browser()  
  map <-
  tm_shape(si1) + tm_rgb() +
    tm_shape(contours_sm) + 
    tm_lines(col = "white", lwd = .5,alpha = .8) +
    tm_shape(xs_sf_crop) + 
    tm_lines() +
    tm_shape(ys_sf_crop) + 
    tm_lines() +
    tm_shape(x_points) + 
    tm_text(text = "text",bg.alpha = .5,bg.color = "white",size = 1) +
    tm_shape(y_points) + 
    tm_text(text = "text",bg.alpha = .5,bg.color = "white",size = 1,just = "left")+
    tm_layout(frame = FALSE,frame.lwd = 0)
  
  filename <- file.path("data-tmp","maps",paste0("map",kach,".pdf"))
  tmap_save(map, width = 420,height = 297, units = "mm",filename = filename) |> 
    invisible()


},.progress = TRUE)
