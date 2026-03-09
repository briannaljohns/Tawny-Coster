library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(janitor)

##############################
# Cleaning ALA data

ala_data <- data.table::fread("output/allocc.csv")

#check column names
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

#create new group with passiflora family, tawny coster and glasswing as values

ala_data <- ala_data %>%
  mutate(group = case_when(
    scientificName %in% c("Acraea terpsicore", "Acraea andromacha") ~ scientificName,
    grepl("Passiflora", scientificName) ~ "Passifloraceae",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(group))

write_csv(ala_data, "output/cleaned_allocc.csv")

# Cleaning memory
rm(ala_data)

##############################
# Filter data frame

occ_raw <- read_csv("output/cleaned_allocc.csv")

# Convert to sf + filter to region ; have map include states
crs_proj <- "EPSG:20353"  # UTM zone 53, metres)

occ_sf <- st_as_sf(occ_raw, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs_proj)

countries <- ne_states(
  country = "Australia",
  returnclass = "sf"
) %>%
  st_transform(crs_proj) %>%
  st_sf()

plot(countries["geometry"])

occ_sf <- st_filter(occ_sf, countries)

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
  group_by(grid_id, group) %>%
  summarise(n_records = n(), .groups = "drop")

n_records_species_wide <- n_records_species %>%
  pivot_wider(
    names_from  = group,
    values_from = n_records,
    values_fill = 0  # grids with no records for a species get 0
  ) %>%
  rowwise() %>%
  mutate(
    n_species = sum(c("Acraea terpsicore","Acraea andromacha","Passifloraceae") > 0)  # species present
  ) %>%
  ungroup()

# Export data
write_csv(n_records_species_wide, "output/gridsummary.csv")
