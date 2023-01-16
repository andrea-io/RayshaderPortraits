library(sf)
library(tidyverse)
library(tigris)
library(ggplot2)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)
library(rnaturalearth)

map <- "bosphorus"

# Load Kontur data
turkiye <- st_read("data/kontur_population_TR_20220630.gpkg")

marine_areas <- st_read("data/ne_10m_geography_marine_polys/ne_10m_geography_marine_polys.shp")

bosp <- rivers |>
  filter(name == "Bosporus")

tr <- st_as_sf(countries110) |>
  filter(admin == "Turkey")

bosp |>
  ggplot() +
  geom_sf(data = tr) +
  geom_sf()

# Create buffer along strait
bosp_buff <- st_transform(bosp, crs = st_crs(turkiye)) |>
  st_buffer(80000)

bosp_buff |>
  ggplot() +
  geom_sf()

# Limit data
int <- st_intersects(bosp_buff, turkiye)

st_d <- turkiye[int[[1]],]

bb <- st_bbox(st_d)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]]))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]]))

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]]))

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

rast <- st_rasterize(st_d |>
                       select(population, geom),
                     nx = floor(size * w_ratio), ny = floor(size * h_ratio))

mat <- matrix(rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

# Create color palette 
color <- MetBrewer::met.brewer(name="Morgenstern", direction = -1)

tx <- grDevices::colorRampPalette(color, bias = 0.5)(256)
swatchplot(tx)
swatchplot(color)

# Plot the matrix
rgl::rgl.close()

mat |>
  height_shade(texture = tx) |>
  plot_3d(heightmap = mat,
          zscale = 100,
          solid = FALSE,
          shadowdepth = 0)

render_camera(theta = 0, phi = 55, zoom =.66)

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