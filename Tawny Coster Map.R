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
library(patchwork)

###### CREATE MAP
# Load occurence data
analysis_df <- read_csv("grid_summary_speciesbasedATAAhostplants.csv", show_col_types = FALSE)

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

# Build the grid from the countries
grid <- st_make_grid(
  st_as_sfc(st_bbox(countries_utm)),
  cellsize = 25000, # 25 x 25 km grid
  square = TRUE
) %>%
  st_sf(grid_id = seq_along(.))

# Filter grid to Australia
grid <- grid[lengths(st_intersects(grid, countries_utm)) > 0, ] %>%
  mutate(grid_id = row_number())

###### CREATE SIDE BY SIDE HEAT MAPS FOR BOTH SPECIES
# Presence distribution
grid_map_plot <- grid %>%
  left_join(analysis_df, by = "grid_id") %>%
  mutate(
    `Acraea andromacha` = replace_na(`Acraea andromacha`, 0),
    `Acraea terpsicore` = replace_na(`Acraea terpsicore`, 0),
    `Afrohybanthus enneaspermus` = replace_na(`Afrohybanthus enneaspermus`, 0),
    `Adenia heterophylla` = replace_na(`Adenia heterophylla`, 0),
  )

# Bounding box for cropping
bb <- st_bbox(countries_utm)

###### CREATE ONE MAP OF SPECIES PRESENCE
grid_map_plot <- grid_map_plot %>%
  mutate(
    presence_category = case_when(
      `Adenia heterophylla` > 0 & `Afrohybanthus enneaspermus` > 0 & `Acraea andromacha` > 0 & `Acraea terpsicore` > 0 ~"All species present",
      `Adenia heterophylla` > 0 & `Afrohybanthus enneaspermus` == 0 & `Acraea andromacha` > 0 & `Acraea terpsicore` > 0 ~ "A. heterophylla and both butterflies present",
      `Adenia heterophylla` == 0 & `Afrohybanthus enneaspermus` > 0 & `Acraea andromacha` > 0 & `Acraea terpsicore` > 0 ~ "A. enneaspermus and both butterflies present",
      `Adenia heterophylla` > 0 & `Afrohybanthus enneaspermus` > 0 & `Acraea andromacha` == 0 & `Acraea terpsicore` == 0 ~"Only host plants present",
      TRUE ~ "None of the species present"
    )
  )

species_presence_map <- ggplot() +
    geom_sf(data = countries_utm, fill = NA, colour = "grey80", linewidth = 0.2) +
    geom_sf(
      data = grid_map_plot,
      aes(fill = presence_category),
      colour = "white",
      linewidth = 0.1
    ) +
    scale_fill_manual(
      values = c(
        "A. heterophylla and both butterflies present" = "purple",
        "A. enneaspermus and both butterflies present" = "orange",
        "All species present" = "blue",
        "Only host plants present" = "green",
        "None of the species present" = "grey90"
      ),
      name = "Species present"
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

species_presence_map
ggsave("species_presence_mapATAAhostplants.pdf", species_presence_map, width = 12, height = 6, dpi = 1200)

# # Plot heatmap
# # Function to plot heatmap per species
# plot_species_heatmap <- function(species_col, species_name) {
#   ggplot() +
#     geom_sf(data = countries_utm, fill = NA, colour = "grey80", linewidth = 0.2) +
#     geom_sf(
#       data = grid_map_plot,
#       aes(fill = !!sym(species_col)),
#       colour = "white",
#       linewidth = 0.1
#     ) +
#     scale_fill_viridis_c(
#       trans = "log10",
#       na.value = "grey90",
#       name = paste("Records of", species_name)
#     ) +
#     coord_sf(
#       xlim = c(bb["xmin"], bb["xmax"]),
#       ylim = c(bb["ymin"], bb["ymax"]),
#       expand = FALSE
#     ) +
#     theme_classic() +
#     theme(
#       panel.grid = element_blank(),
#       axis.text = element_blank(),
#       axis.ticks = element_blank(),
#       axis.line = element_blank()
#     ) +
#     labs(x = NULL, y = NULL)
# }
# 
# plot_species_heatmap("Passiflora foetida", "Passiflora foetida")
# 
# plot_species_heatmap("Acraea terpsicore", "Acraea terpsicore")
# 
# plot_species_heatmap("Passiflora edulis","Passiflora edulis")
# 
# g_foetida <- plot_species_heatmap("Passiflora foetida", "Passiflora foetida")
# g_terpsicore <- plot_species_heatmap("Acraea terpsicore", "Acraea terpsicore")
# g_edulis <- plot_species_heatmap("Passiflora edulis","Passiflora edulis")
# 
# combined_vertical_map <- g_foetida + g_terpsicore + g_edulis
#   plot_layout(ncol = 1, guides = "collect")  # ncol = 1 → vertical, guides = "collect" → shared legend
# 
# combined_vertical_map
# 
# ggsave(
#   "combined_species_heatmap_vertical_AT_PF.pdf",
#   combined_vertical_map,
#   width = 8,   # adjust width/height as needed
#   height = 12, 
#   dpi = 1200
# )
