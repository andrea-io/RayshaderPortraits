options(rgl.useNULL = FALSE)

library(sf)
library(tigris)
library(tidyverse)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)

# Load Kontur data
uzb <- st_read("data/kontur_population_UZ_20220630.gpkg")

# Plot to view data
uzb |>
  ggplot() +
  geom_sf()

# Define aspect ratio based on bounding box
bb <- st_bbox(uzb)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(uzb))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(uzb))

# Check by plotting points
uzb |>
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")


width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |>
  st_sfc(crs = st_crs(uzb))

height <- st_distance(bottom_left, top_left)

# Handle conditions of width or height being the longer side
if(width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
} 

# Convert to raster so we can then convert to matrix
size <- 5000
uzb_rast <- st_rasterize(uzb, nx = floor(size * w_ratio), ny = floor(size * h_ratio))

mat <- matrix(uzb_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

# Create color palette 
color <- c("#0099B5", "#1EB53A", "#FFFFFF", "#CE1126")

tx <- grDevices::colorRampPalette(color, bias =2.5)(256)
swatchplot(tx)
swatchplot(color)

# Plot the matrix
rgl::rgl.close()

mat |>
  height_shade(texture = tx) |>
  plot_3d(heightmap = mat,
          zscale = 105,
          solid = FALSE,
          shadowdepth = 0)

render_camera(theta = 0, phi = 45, zoom =.75)

outfile <- "images/final_plot.png"


{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality (
    filename = outfile,
    interactive = FALSE,
    lightdirection = 200,
    lightaltitude = c(20,80),
    lightcolor = c(color[2], "white"),
    lightintensity = c(300,375),
    samples = 450,
    height = 6000,
    width = 6000
  )
  
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}