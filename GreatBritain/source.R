library(sf)
library(tidyverse)
library(tigris)
library(ggplot2)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)

# Load Kontur data
gb <- st_read("data/kontur_population_GB_20220630.gpkg")

gb |>
  ggplot() +
  geom_sf()

bb <- st_bbox(gb)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(gb))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(gb))

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |>
  st_sfc(crs = st_crs(gb))

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

nx = floor(size * w_ratio)
ny = floor(size * h_ratio)

gb_rast <- st_rasterize(gb, nx = 531, ny = 1000)

mat <- matrix(gb_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

# Create color palette 
color <- MetBrewer::met.brewer(name="Derain")

tx <- grDevices::colorRampPalette(color)(256)
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

render_camera(theta = -8, gbi = 55, zoom =.66)

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





