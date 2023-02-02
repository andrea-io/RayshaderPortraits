library(sf)
library(tidyverse)
library(tigris)
library(ggplot2)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)
library(glue)

map <- "blk"

# Load subregion data
black_sea <- st_read("data/MSFD_Regions_and_Subregions.gpkg")

black_sea |> 
  filter(region == "BLK") |> 
  st_zm() |> 
  ggplot() + 
  geom_sf()

country_codes <- c(
  "BG",
  "GE",
  "MD",
  "RO",
  "RU",
  "TR",
  "UA"
)

data <- map_df(country_codes, function(i) {
  st_read(glue("data/kontur_population_{i}_20220630.gpkg"))
})

blk <- black_sea |> 
  filter(region == "BLK") |> 
  st_zm() |> 
  st_union() |> 
  st_transform(crs = st_crs(data)) |> 
  st_buffer(25000)

int <- st_intersects(data, blk) 

st_dd <- map_dbl(int, function(i) {
  if (length(i) > 0) {
    return(i)
  } else {
    return(0)
  }
})

st_d <- data[which(st_dd == 1),]

bb <- st_bbox(st_d)
yind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmin"]], bb[["ymax"]])))
xind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmax"]], bb[["ymin"]])))

# Handle conditions of width or height being the longer side
if (yind > xind) {
  y_rat <- 1
  x_rat <- xind / yind
} else {
  x_rat <- 1
  y_rat <- yind / xind
}

# Convert to raster so we can then convert to matrix
size <- 6000

blk_rast <- st_rasterize(st_d |> 
                       select(population, geom),
                     nx = floor(size * x_rat), ny = floor(size * y_rat))

mat <- matrix(blk_rast$population, nrow = floor(size * x_rat), ncol = floor(size * y_rat))

# Create color palette 
color <- MetBrewer::met.brewer(name="Troy")

tx <- grDevices::colorRampPalette(color, bias = 2)(256)
swatchplot(tx)
swatchplot(color)

# Plot the matrix
rgl::rgl.close()

mat |>
  height_shade(texture = tx) |>
  plot_3d(heightmap = mat,
          zscale = 15,
          solid = FALSE,
          windowsize = c(200,200),
          shadowdepth = 0)

render_camera(theta = -8, phi = 50, zoom =.82)

testfile <- "images/test_plot.png"
outfile <- "images/final_plot.png"

render_highquality (
  filename = testfile,
  interactive = FALSE,
  lightdirection = 75,
  lightaltitude = c(20,80),
  lightcolor = c(color[4], "white"),
  lightintensity = c(900,200),
  height = 500,
  width = 500
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
    lightdirection = 75,
    lightaltitude = c(20,80),
    lightcolor = c(color[4], "white"),
    lightintensity = c(900,200),
    samples = 450,
    height = 6000,
    width = 6000
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}





