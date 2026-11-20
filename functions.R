library(sf)
library(tidyverse)
library(terra)
library(mapview)
library(wav)
library(arcgislayers)

philmont_6m_data <- rast(r"(H:\GIS\philmont_1to1_minecraft\results\seamless_6m_mosaic.tif)")
# Replace zeros with NA in original raster
philmont_clean <- terra::ifel(
  philmont_6m_data == 0,
  NA,
  philmont_6m_data
)

# Aggregate, ignoring NA values
philmont_display_data <- terra::aggregate(
  philmont_clean,
  fact = 10,
  fun = mean,
  na.rm = TRUE
)

# Remove any remaining zeros
philmont_display_data <- terra::ifel(
  philmont_display_data == 0,
  NA,
  philmont_display_data
)

# Get raster extent
raster_ext <- st_bbox(philmont_display_data) |>
  st_transform(4326) |>
  st_as_sfc() |>
  st_as_sf()

print(raster_ext)

# philmont_6m_bbox <- st_bbox(philmont_6m_data) |> st_as_sfc() |> st_as_sf()
usgs_url <- paste0(
  "https://elevation.nationalmap.gov/arcgis/rest/services/",
  "3DEPElevation/ImageServer"
)

usgs_dem <- arc_open(usgs_url)

us <- tigris::states() |>
  filter(!STUSPS %in% c("AK", "HI", "PR", "GU", "AS", "VI", "MP")) |>
  st_transform(4326) |>
  st_bbox() |>
  st_as_sfc() |>
  st_as_sf()

get_terrain <- function(aoe, width = 256, height = 256) {
  
  aoe_3857 <- st_transform(aoe, 3857)
  bb <- st_bbox(aoe_3857)
  
  terrain <- arc_raster(
    x = usgs_dem,
    xmin = bb["xmin"],
    ymin = bb["ymin"],
    xmax = bb["xmax"],
    ymax = bb["ymax"],
    bbox_crs = 3857,
    crs = 3857,
    width = width,
    height = height,
    format = "tiff"
  )
  
  # Restrict the rectangular result to the actual AOI
  terrain <- terra::mask(
    terrain,
    terra::vect(aoe_3857)
  )
  
  return(terrain)
}

generate_wavetable <- function(terrain_rast, aoe, path){
  # Crop the raster
  aoe <- aoe |>
    st_transform(st_crs(terrain_rast))
  
  clip_rast <- crop(
    terrain_rast,
    vect(aoe)
  )
  
  # Mask to polygon
  clip_rast <- mask(
    clip_rast,
    vect(aoe)
  )
  
  # Create target raster
  target <- rast(
    nrows = 256,
    ncols = 256,
    ext = ext(clip_rast),
    crs = crs(clip_rast)
  )
  
  # Resample elevation values
  terrain_grid <- resample(
    clip_rast,
    target,
    method = "bilinear"
  )
  
  terrain_mat <- matrix(
    values(terrain_grid),
    nrow = 256,
    ncol = 256,
    byrow = TRUE
  )
  
  # Interpolate each transect to 2048 samples
  wavetable <- t(apply(terrain_mat, 1, function(x) {
    
    # Interpolate missing elevations
    good <- which(!is.na(x))
    
    if (length(good) < 2) {
      stop("A transect contains insufficient elevation data.")
    }
    
    x <- approx(
      x = good,
      y = x[good],
      xout = seq_along(x),
      rule = 2
    )$y
    
    # Resample to 2048 samples
    approx(
      x = seq(0, 1, length.out = length(x)),
      y = x,
      xout = seq(0, 1, length.out = 2048)
    )$y
    
  }))
  
  wavetable <- t(apply(wavetable, 1, make_periodic))
  
  wavetable <- wavetable - mean(wavetable)
  
  peak <- max(abs(wavetable))
  
  if (peak == 0 || !is.finite(peak)) {
    stop("Invalid wavetable amplitude.")
  }
  
  wavetable <- wavetable / peak
  wavetable <- wavetable[nrow(wavetable):1, ]
  # Flatten frames into one continuous waveform
  wavetable_audio <- as.vector(t(wavetable))
  
  par(mfrow = c(1, 1))
  # Export
  wav::write_wav(
    matrix(wavetable_audio, nrow = 1),
    path,
    sample_rate = 44100,
    normalize = FALSE
  )
  
  par(mfrow = c(2, 1))
  
  plot(
    wavetable[1, ],
    type = "l",
    main = "First Wavetable Frame",
    xlab = "Sample",
    ylab = "Amplitude"
  )
  
  plot(
    wavetable[1, 1900:2048],
    type = "l",
    main = "End of First Frame",
    xlab = "Sample",
    ylab = "Amplitude"
  )
}

# Make each frame periodic by matching its beginning and end
make_periodic <- function(x, window = 8) {
  
  n <- length(x)
  t <- seq(0, 1, length.out = n)
  
  # Difference between endpoints
  delta <- x[n] - x[1]
  
  # Estimate slope difference at the endpoints
  start_slope <- mean(diff(x[1:(window + 1)]))
  end_slope <- mean(diff(x[(n - window):n]))
  
  slope_delta <- (end_slope - start_slope) * (n - 1)
  
  # Cubic correction matching both endpoint values and slopes
  correction <- (slope_delta - 2 * delta) * t^3 +
    (3 * delta - slope_delta) * t^2
  
  x - correction
}

make_square <- function(aoe) {
  
  req(!is.null(aoe), nrow(aoe) > 0)
  
  aoe <- st_transform(aoe, 3857)
  
  # Combine all drawn geometries
  aoe <- st_union(aoe)
  
  bb <- st_bbox(aoe)
  
  if (anyNA(bb)) {
    stop("Could not calculate AOI bounding box.")
  }
  
  width <- as.numeric(bb["xmax"] - bb["xmin"])
  height <- as.numeric(bb["ymax"] - bb["ymin"])
  
  if (!is.finite(width) || !is.finite(height) ||
      width <= 0 || height <= 0) {
    stop("Drawn AOI has invalid dimensions.")
  }
  
  side <- max(width, height)
  
  cx <- mean(c(bb["xmin"], bb["xmax"]))
  cy <- mean(c(bb["ymin"], bb["ymax"]))
  
  square <- st_as_sfc(
    st_bbox(
      c(
        xmin = cx - side / 2,
        ymin = cy - side / 2,
        xmax = cx + side / 2,
        ymax = cy + side / 2
      ),
      crs = st_crs(aoe)
    )
  )
  
  st_sf(geometry = st_transform(square, 4326))
}