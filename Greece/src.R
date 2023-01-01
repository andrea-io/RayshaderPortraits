library(sf)
library(tidyverse)
library(tigris)
library(ggplot2)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)

# Load Kontur data
greece <- st_read("data/kontur_population_GR_20220630.gpkg")

greece |>
  ggplot() +
  geom_sf()

bb <- st_bbox(greece)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(greece))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(greece))

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |>
  st_sfc(crs = st_crs(greece))

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

greece_rast <- st_rasterize(greece, nx = floor(size * w_ratio), ny = floor(size * h_ratio))

mat <- matrix(greece_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

# Create color pallette 
color <- c("#284177", "#006BBD", "#83CEEC", "#EDE8E4", "#C2AFA8")

tx <- grDevices::colorRampPalette(color, bias = 2)(256)
swatchplot(tx)
swatchplot(color)

# Plot the matrix
rgl::rgl.close()

mat |>
  height_shade(texture = tx) |>
  add_overlay(sphere_shade(mat, texture = "desert", 
  zscale= 4, colorintensity = 1), alphalayer=0.5) |>
  plot_3d(heightmap = mat,
          zscale = 100 / 5,
          solid = FALSE,
          shadowdepth = 0)

render_camera(theta = -20, phi = 45, zoom =.66)

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
    lightdirection = 280,
    lightaltitude = c(20,80),
    lightcolor = c(color[3], "white"),
    lightintensity = c(800,200),
    samples = 450,
    height = 6000,
    width = 6000
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}





