

library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(glue)
library(raster)
library(ggplot2)
terraOptions(progress = 0)

rast2poly <- \(x)ext(x) |> vect() |> as("Spatial") |> st_as_sfc() |> st_as_sf()

ces_bb <- st_bbox(c(
  xmin = 2705685,
  xmax = 2707625,
  ymin = 1142942,
  ymax = 1144422
)) |>
  st_as_sfc(crs = 2056)

# https://s.geo.admin.ch/wu0fc107mip8
c(
  2704949, 1144045,
  2706029, 1144915,
  2707289, 1143465,
  2706249, 1142515,
  2704949, 1144045
) |> 
  matrix(ncol = 2, byrow = TRUE) |> 
  list() |> 
  st_polygon() -> ces



fi <- c("data/SWISSIMAGE/1961-1995", "data/SWISSIMAGE/1999-2005", "data/SWISSIMAGE/2009-2015") |> 
  lapply(\(x)list.files(x, "\\.tif$",full.names = TRUE)) |> 
  unlist()
  

fi2 <- list.files("data/SWISSIMAGE/2018-2021/", "\\.tif$",full.names = TRUE)

  


fi2_r <- lapply(fi2, rast)

exts <- lapply(fi2_r, rast2poly)

df1 <- tibble(file = fi) |> 
  mutate(
    year = str_match(fi, "(\\d{4})_1")[,2],
    kartenblatt = str_match(fi, "_(\\d{4}-\\d{2})")[,2],
    )

df2 <-tibble(file = fi2) |> 
  separate_wider_delim(file, "_", names = c(NA,NA,"NR1","NR2", "year", NA),cols_remove = FALSE) |> 
  transmute(file, year, kartenblatt = paste(NR1, NR2, sep = "_"))

df <- bind_rows(df1, df2)

# reso <- list.files("data-out","\\.tif$",full.names = TRUE) 
# names(reso) <- basename(reso)
# imap(reso, \(x,y) x |> rast() |> res()) |> 
#   sapply(\(x)x)




rasts <- df |> 
  # filter(as.integer(year)>= 2009) |> 
  (\(x)split(x, x$year))()  |> 
  imap(\(df2, year){
    # browser()
    
    
    rasts <- sapply(df2$file, rast) 
    
    bbs <- lapply(rasts, rast2poly) |> do.call(rbind, args = _)
    
    
    rasts2 <- rasts[st_intersects(bbs, ces, sparse = FALSE)]
    
    rasts3 <- map(rasts2, \(x){
      y <- crop(x, ces)
      fac <- .5/unique(res(y))
      aggregate(y, fac)
    })
    
    out <- Reduce(merge,x = rasts3)
    
    out2 <- crop(out, ces)
    
    crs(out2) <- "epsg:2056"
    
    writeRaster(out2, glue("data-out/prepared0/{year}.tif"), overwrite = TRUE)
    out2
  }, .progress = TRUE)




## part 2


prep0 <- list.files("data-out/prepared0/", "\\.tif$", full.names = TRUE)

names(prep0) <- basename(prep0)


imap(prep0, \(x,y){
  r <- rast(x)  
  writeRaster(r, glue("data-out/prepared1/{y}"),datatype = "INT1U", overwrite = TRUE)
})












# Laplacian <- matrix(c(0,1,0,1,-4,1,0,1,0), nrow=3)
# fx=matrix(c(-1,-2,-1,0,0,0,1,2,1), nrow=3)
# fy=matrix(c(1,0,-1,2,0,-2,1,0,-1), nrow=3)

# tmp <- focal(wald2,w = fx, fun = mean)
# tmp2 <- focal(wald2,w = fy, fun = mean)


