# Load libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(stringr)
library(ggplot2)
library(janitor)
library(sp)

# Load occurence data
occ <- read_csv("tawny-coster-records-2026-02-05.csv", show_col_types = FALSE) %>% clean_names()
analysis_df <- read_csv("trait_grid_summary_speciesbased.csv", show_col_types = FALSE)

### Use a standard CRS (metres), avoids km/metre confusion
crs_proj <- 20353  # EPSG:20353 (UTM zone 53, metres)

# Get polygon (Australia only) and transform to UTM
countries_utm <- ne_countries(
  scale = "large",
  country = ("Australia"),
  returnclass = "sf"
) %>%
  st_transform(crs_proj) %>%
  st_union() %>%
  st_sf()

#check that it is the correct country
plot(countries_utm)

#convert lat-long to UTM
to_utm <- function(df) {
  st_as_sf(df, coords = c("decimal_longitude", "decimal_latitude"), crs = 4326) %>%
    st_transform(crs_proj)
}

tc_utm <- occ %>% to_utm()

inside_region <- function(x) x[st_within(x, countries_utm, sparse = FALSE), ]
tc_utm <- inside_region(tc_utm)

# Build the grid from the countries
grid <- st_make_grid(
  st_as_sfc(st_bbox(countries_utm)),
  cellsize = 10000,
  square = TRUE
) %>%
  st_sf(grid_id = seq_along(.))

# Filter grid to Australia
grid <- grid[lengths(st_intersects(grid, countries_utm)) > 0, ] %>%
  mutate(grid_id = row_number())

# Presence distribution
grid_presence <- analysis_df %>%
  dplyr::select(grid_id, source, n_species) %>%
  distinct() %>%
  mutate(presence = as.integer(n_species > 0)) %>%
  dplyr::select(-n_species) %>%
  pivot_wider(names_from = source, values_from = presence, values_fill = 0)

# Add presence column to grid
grid_map_plot <- grid %>%
  left_join(
    dplyr::select(analysis_df, grid_id, n_species),
    by = "grid_id"
  ) %>%
  mutate(
    n_species = replace_na(n_species, 0),          # replace NA with 0
    presence = n_species > 0                       # TRUE if species present
  )

# Bounding box for cropping
bb <- st_bbox(countries_utm)

# Plot all grids
ggplot() +
  geom_sf(data = countries_utm, fill = NA, colour = "grey80", linewidth = 0.2) +
  geom_sf(
    data = grid_map_plot,
    aes(fill = n_species),
    colour = "white",     # grid borders
    linewidth = 0.1
  ) +
  scale_fill_gradient(
    low = "grey90",       # empty grids
    high = "steelblue",   # grids with more species
    name = "Species count"
  ) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  ) +
  labs(x = NULL, y = NULL)