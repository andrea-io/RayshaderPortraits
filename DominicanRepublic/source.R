library(sf)
library(tidyverse)
library(tigris)
library(ggplot2)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)

# Load Kontur data
do <- st_read("data/kontur_population_DO_20220630.gpkg")

do |>
  ggplot() +
  geom_sf()

bb <- st_bbox(do)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(do))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(do))

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |>
  st_sfc(crs = st_crs(do))

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
size <- 1000

do_rast <- st_rasterize(do, nx = floor(size * w_ratio), ny = floor(size * h_ratio))

mat <- matrix(do_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

# Create color palette 
color <- MetBrewer::met.brewer(name="Benedictus", direction = -1)

tx <- grDevices::colorRampPalette(color, bias = 2.6)(256)
swatchplot(tx)
swatchplot(color)

# Plot the matrix
rgl::rgl.close()

mat |>
  height_shade(texture = tx) |>
  plot_3d(heightmap = mat,
          zscale = 60,
          solid = FALSE,
          shadowdepth = 0)

render_camera(theta = -8, doi = 55, zoom =.66)

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
    lightdirection = 255,
    lightaltitude = c(20,80),
    lightcolor = c(color[1], "white"),
    lightintensity = c(900,200),
    samples = 450,
    height = 6000,
    width = 6000
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}





