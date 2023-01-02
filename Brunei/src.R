options(rgl.useNULL = FALSE)

library(sf)
library(tigris)
library(tidyverse)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)

# Load Brunei Kontur data
brunei_data <- st_read("data/kontur_population_BN_20220630.gpkg")

# Plot data to view general object shape
brunei_data |>
  ggplot() +
  geom_sf()

# Define aspect ratio based on bounding box
bb <- st_bbox(brunei_data)

# Retrieve bottom left/right coordinates to find width and height values
bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(brunei_data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(brunei_data))

# Use distance between the two points to find width
width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |>
  st_sfc(crs = st_crs(brunei_data))

# Use distance between bottom and top left to find heigh
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

brunei_rast <- st_rasterize(brunei_data, nx = floor(size * w_ratio), ny = floor(size * h_ratio))

mat <- matrix(brunei_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

# Create color palette 
color <- MetBrewer::met.brewer(name="Demuth")

tx <- grDevices::colorRampPalette(color, bias = 1.5)(256)
swatchplot(tx)
swatchplot(color)

# Plot the matrix
rgl::rgl.close()

mat |>
  height_shade(texture = tx) |>
  add_overlay(sphere_shade(mat, texture = "desert", 
  zscale= 0.5, colorintensity = 4), alphalayer=0.5) |>
  plot_3d(heightmap = mat,
          zscale = 100 / 5 / 2,
          solid = FALSE,
          shadowdepth = 0,
          shadowcolor = color[7],
          shadow_darkness = 2)

render_camera(theta = 200, phi = 30, zoom =.65)

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
    lightdirection = 350,
    lightaltitude = c(20,80),
    lightcolor = c(color[5], "white"),
    lightintensity = c(800,200),
    samples = 450,
    height = 6000,
    width = 6000
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}