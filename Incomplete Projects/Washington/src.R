options(rgl.useNULL = FALSE)

library(sf)
library(tigris)
library(tidyverse)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)

# Load Kontur data
data <- st_read("data/kontur_population_US_20220630.gpkg")

# Load US States
st <- states()

# Retrieve Washington data object
wash <- st |>
  filter(NAME == "Washington") |>
  st_transform(crs = st_crs(data))

# Plot to view data
wash |>
  ggplot() +
  geom_sf()

# ST Intersection of points on Washington polygon object
st_wash <- st_intersection(data, wash)

# Define aspect ratio based on bounding box
bb <- st_bbox(st_wash)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(data))

# Check by plotting points
wash |>
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |>
  st_sfc(crs = st_crs(data))

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
wash_rast <- st_rasterize(st_wash, nx = floor(size * w_ratio), ny = floor(size * h_ratio))

mat <- matrix(wash_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

# Create color palette 
color <- MetBrewer::met.brewer(name="Pillement")

tx <- grDevices::colorRampPalette(color, bias = 2.5)(256)
swatchplot(tx)
swatchplot(color)

# Plot the matrix
rgl::rgl.close()

mat |>
  height_shade(texture = tx) |>
  plot_3d(heightmap = mat,
          zscale = 50,
          solid = FALSE,
          shadowdepth = 0,
          shadow_darkness = 1,
          shadowcolor = color[5])

render_camera(theta = 10, phi = 40, zoom = .8)

outfile <- "images/test_plot.png"

render_highquality (
  filename = outfile,
  interactive = FALSE,
  lightdirection = 100,
  lightaltitude = c(20,80),
  lightcolor = c(color[3], "white"),
  lightintensity = c(300,240)
)

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality (
    filename = outfile,
    interactive = FALSE,
    lightdirection = 100,
    lightaltitude = c(20,80),
    lightcolor = c(color[3], "white"),
    lightintensity = c(300,240),
    samples = 450,
    height = 6000,
    width = 6000
  )
  
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}