library(sf)
library(tidyverse)
library(terra)
library(mapview)
library(wav)

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
  
  # Center around zero using the overall terrain mean
  wavetable <- wavetable - mean(wavetable)
  
  # Normalize globally
  wavetable <- wavetable / max(abs(wavetable))
  
  # Flatten frames into one continuous waveform
  wavetable_audio <- as.vector(t(wavetable))
  
  # Export
  wav::write_wav(
    matrix(wavetable_audio, nrow = 1),
    path,
    sample_rate = 44100,
    normalize = FALSE
  )
}
