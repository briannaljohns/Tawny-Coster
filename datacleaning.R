# Load libraries
library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(janitor)

##############################
# Cleaning ALA data

ala_data <- data.table::fread("ATPF_occ.csv")
colnames(ala_data)

# Removing blank cells

ala_data <- ala_data[!(is.na(ala_data$V1) | ala_data$V1 == ""),]
ala_data <- ala_data[!(is.na(ala_data$recordID) | ala_data$recordID == ""),]
ala_data <- ala_data[!(is.na(ala_data$taxonConceptID) | ala_data$decimalLatitude == ""),]
ala_data <- ala_data[!(is.na(ala_data$scientificName) | ala_data$scientificName == ""),]
ala_data <- ala_data[!(is.na(ala_data$decimalLongitude) | ala_data$decimalLongitude == ""),]
ala_data <- ala_data[!(is.na(ala_data$decimalLatitude) | ala_data$decimalLatitude == ""),]
ala_data <- ala_data[!(is.na(ala_data$occurrenceStatus) | ala_data$occurrenceStatus == ""),]
ala_data <- ala_data[!(is.na(ala_data$dataResourceName) | ala_data$dataResourceName == ""),]

# Removing duplicated records
ala_data <- ala_data[!duplicated(ala_data),]

write_csv(ala_data, "cleanedRecordsATPF_ala.csv")

# Cleaning memory
rm(ala_data)

##############################
# Filter data frame

occ_raw <- read_csv("cleanedRecordsATPF_ala.csv")

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

# Create 25 × 25 km grid over the region
grid <- st_make_grid(
  st_as_sfc(st_bbox(countries)),
  cellsize = 25000,  # 25 km in metres
  square = TRUE
) %>%
  st_sf(grid_id = seq_along(.))

# Keep only grid cells that intersect the region
grid <- grid[lengths(st_intersects(grid, countries)) > 0, ] %>%
  mutate(grid_id = row_number())

# Assign records to grids
occ_grid <- st_join(occ_sf, grid, join = st_within) %>%
  filter(!is.na(grid_id))

# Build grid × species (species-weighted)
n_records_species <- occ_grid %>%
  st_drop_geometry() %>%
  group_by(grid_id, scientificName) %>%
  summarise(n_records = n(), .groups = "drop")

n_records_species_wide <- n_records_species %>%
  pivot_wider(
    names_from  = scientificName,
    values_from = n_records,
    values_fill = 0  # grids with no records for a species get 0
  ) %>%
  rowwise() %>%
  mutate(
    n_species = sum(c_across(`Acraea terpsicore`:`Passiflora foetida`) > 0)  # species present
  ) %>%
  ungroup()

# Export data
write_csv(n_records_species_wide, "grid_summary_speciesbasedATPF.csv")

## Citation