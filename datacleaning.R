# Load libraries
library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(janitor)

##############################
# Cleaning ALA data

ala_data <- data.table::fread("tawny-coster-records-2026-02-05.csv")

# Removing blank cells

ala_data <- ala_data[!(is.na(ala_data$species) | ala_data$species == ""),]
ala_data <- ala_data[!(is.na(ala_data$decimalLongitude) | ala_data$decimalLongitude == ""),]
ala_data <- ala_data[!(is.na(ala_data$decimalLatitude) | ala_data$decimalLatitude == ""),]

# Removing duplicated records
ala_data <- ala_data[!duplicated(ala_data),]

write_csv(ala_data, "cleanedRecords_ala.csv")

# Cleaning memory
rm(ala_data)

##############################
# Filter data frame

occ_raw <- read_csv("cleanedRecords_ala.csv")

# Convert to sf + filter to region
crs_proj <- "EPSG:20353"  # UTM zone 53, metres)

occ_sf <- st_as_sf(occ_raw, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs_proj)

countries <- ne_countries(
  scale = "large",
  country = ("Australia"),
  returnclass = "sf"
) %>%
  st_transform(crs_proj) %>%
  st_union() %>%
  st_sf()

occ_sf <- occ_sf[st_within(occ_sf, countries, sparse = FALSE), ]

# Create 10 × 10 km grid over the region
grid <- st_make_grid(
  st_as_sfc(st_bbox(countries)),
  cellsize = 10000,  # 10 km in metres
  square = TRUE
) %>%
  st_sf(grid_id = seq_along(.))

# Keep only grid cells that intersect the region
grid <- grid[lengths(st_intersects(grid, countries)) > 0, ] %>%
  mutate(grid_id = row_number())

# Assign records to grids
occ_grid <- st_join(occ_sf, grid, join = st_within) %>%
  filter(!is.na(grid_id))

# Build grid × source × species (species-weighted)
grid_species <- occ_grid %>%
  st_drop_geometry() %>%
  dplyr::select(grid_id, source, species) %>%
  filter(!is.na(species)) %>%
  distinct(grid_id, source, species)    # <- each species counts once per grid × source

# Digging deeper into the data (how many occurences per grid)
n_records <- occ_grid %>%
  st_drop_geometry() %>%
  count(grid_id, source, name = "n_records")

n_species <- grid_species %>%
  count(grid_id, source, name = "n_species")

trait_grid_summary <- n_species %>%
  left_join(n_records,       by = c("grid_id", "source"))

# Final analysis table (grid × source, fill zeros for missing source)
valid_grids <- occ_grid %>%
  st_drop_geometry() %>%
  distinct(grid_id)

analysis_df <- expand_grid(
  grid_id = valid_grids$grid_id,
  source  = c("CS", "Facebook", "Museum")
) %>%
  left_join(trait_grid_summary, by = c("grid_id", "source")) %>%
  mutate(
    n_species = replace_na(n_species, 0L),
    n_records = replace_na(n_records, 0L)
  )

# Export data
write_csv(trait_grid_summary, "trait_grid_summary_speciesbased.csv")

## Citation
# ALA Occurrence Download https://doi.ala.org.au/doi/10.26197/ala.05a67574-50c1-491a-8ea6-ac1a8700dd18