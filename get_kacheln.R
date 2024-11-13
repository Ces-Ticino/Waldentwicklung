
library(dplyr)
library(sf)
library(purrr)
library(glue)
kacheln <- list.files("data/swissimage_footprints_shape_per_year", "\\.shp$",full.names = TRUE)


kacheln_sf <- map(kacheln, \(x){
  read_sf(x) |> 
    st_transform(2056)
  }) |> 
  bind_rows()



ces_bb <- st_bbox(c(
  xmin = 2705685,
  xmax = 2707625,
  ymin = 1142942, 
  ymax = 1144422
)) |> 
  st_as_sfc()

ces_bb <- st_set_crs(ces_bb, 2056)

kacheln_ces <- kacheln_sf[ces_bb,,]


years <- unique(kacheln_ces$TemporalKe)


# glue("https://map.geo.admin.ch/?lang=de&topic=ech&bgLayer=ch.swisstopo.pixelkarte-farbe&layers=ch.swisstopo.swissimage-product&layers_timestamp={years}&E=2706457.08&N=1144000.12&zoom=10")


