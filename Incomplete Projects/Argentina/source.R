library(sf)
library(tidyverse)
library(tigris)
library(ggplot2)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)

# Load Kontur data
AR <- st_read("data/kontur_population_AR_20220630.gpkg")

AR |>
  ggplot() +
  geom_sf()

bb <- st_bbox(AR)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(AR))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(AR))

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |>
  st_sfc(crs = st_crs(AR))

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

AR_rast <- st_rasterize(AR, nx = 2251, ny = 5000)

mat <- matrix(AR_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

# Create color palette 
color <- MetBrewer::met.brewer(name="Manet")

tx <- grDevices::colorRampPalette(color, 1.5)(256)
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

render_camera(theta = -18, phi = 40, zoom = .69)

testfile <- "images/test.png"

render_highquality (
  filename = testfile,
  interactive = FALSE,
  lightdirection = 255,
  lightaltitude = c(20,80),
  lightcolor = c(color[4], "white"),
  lightintensity = c(900,100)
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
    lightdirection = 255,
    lightaltitude = c(20,80),
    lightcolor = c(color[4], "white"),
    lightintensity = c(900,100),
    samples = 450,
    height = 6000,
    width = 6000
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}





