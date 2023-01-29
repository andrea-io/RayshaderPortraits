library(sf)
library(tidyverse)
library(tigris)
library(ggplot2)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)

# Load Kontur data
IT <- st_read("data/kontur_population_IT_20220630.gpkg")

bb <- st_bbox(IT)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(IT))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(IT))

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |>
  st_sfc(crs = st_crs(IT))

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

nx = floor(size * w_ratio)
ny = floor(size * h_ratio)

IT_rast <- st_rasterize(IT, nx = 3836, ny = 5000)

mat <- matrix(IT_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

# Create color palette 
color <- MetBrewer::met.brewer(name="Paquin", direction = -1)

tx <- grDevices::colorRampPalette(color, bias = 4.5)(256)
swatchplot(tx)
swatchplot(color)

# Plot the matrix
rgl::rgl.close()

mat |>
  height_shade(texture = tx) |>
  plot_3d(heightmap = mat,
          zscale = 50,
          solid = FALSE,
          shadowdepth = 0)

render_camera(theta = 6, phi = 40, zoom =.57)

testfile <- "images/test_plot.png"

render_highquality (
  filename = testfile,
  interactive = FALSE,
  lightdirection = 250,
  lightaltitude = c(20,80),
  lightcolor = c(color[6], "white"),
  lightintensity = c(900,150)
)

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
    lightdirection = 250,
    lightaltitude = c(20,80),
    lightcolor = c(color[6], "white"),
    lightintensity = c(900,150),
    samples = 450,
    height = 6000,
    width = 6000
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}



