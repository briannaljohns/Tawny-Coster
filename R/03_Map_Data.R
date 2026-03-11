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
library(geosphere)

###### CREATE MAP
# Load occurence data
analysis_df <- read_csv("output/gridsummary.csv", show_col_types = FALSE)

### Use a standard CRS (metres), avoids km/metre confusion
crs_proj <- 20353  # EPSG:20353 (UTM zone 53, metres)

# Get polygon (Australian states only) and transform to UTM
countries_utm <- ne_states(
  country = "Australia",
  returnclass = "sf"
) %>%
  st_transform(crs_proj) %>%
  st_sf()

#check that it is the correct map
plot(countries_utm["geometry"])

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

###### MAP WITH ALL SPECIES
# Presence distribution
grid_map_plot <- grid %>%
  left_join(analysis_df, by = "grid_id") %>%
  mutate(
    `Acraea andromacha` = replace_na(`Acraea andromacha`, 0),
    `Acraea terpsicore` = replace_na(`Acraea terpsicore`, 0),
    `Passifloraceae` = replace_na(`Passifloraceae`, 0),
  )

# Bounding box for cropping
bb <- st_bbox(countries_utm)

# Find viridis colors I want to use
viridis_colors <- viridis::viridis(5, option = "D")
print(viridis_colors)

###### CREATE ONE MAP OF SPECIES PRESENCE
grid_map_plot <- grid_map_plot %>%
  mutate(
    presence_category = case_when(
      `Passifloraceae` > 0 & `Acraea andromacha` > 0 & `Acraea terpsicore` > 0 ~"All species present",
      `Passifloraceae` == 0 & `Acraea andromacha` > 0 & `Acraea terpsicore` > 0 ~ "Both butterflies present and Passifloraceae absent",
      `Passifloraceae` > 0 & `Acraea andromacha` == 0 & `Acraea terpsicore` > 0 ~ "A. terpsicore and Passifloraceae present",
      `Passifloraceae` > 0 & `Acraea andromacha` > 0 & `Acraea terpsicore` == 0 ~ "A. andromacha and Passifloraceae present",
      `Passifloraceae` > 0 & `Acraea andromacha` == 0 & `Acraea terpsicore` == 0 ~"Only Passifloraceae present",
      TRUE ~ "None of the species present"
    )
  )

species_presence_map <- ggplot() +
  geom_sf(
    data = grid_map_plot,
    aes(fill = presence_category),
    colour = "white",
    linewidth = 0.1
  ) +
  geom_sf(
    data = countries_utm,
    fill = NA,
    colour = "black",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "A. terpsicore and Passifloraceae present" = "#440154FF", #using viridis color blind friendly palette
      "A. andromacha and Passifloraceae present" = "#3B528BFF",
      "Both butterflies present and Passifloraceae absent" = "#21908CFF", 
      "All species present" = "#5DC863FF",
      "Only Passifloraceae present" = "#FDE725FF",
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
ggsave("output/alloccmap.pdf", species_presence_map, width = 12, height = 6, dpi = 1200)

##### CREATE POLYGON OF FIELDWORK AREA

## Determine midpoint between Wollongong and Melbourne; Data acquired from LatLong.net

point1 <- c(150.893143, -34.425072) #Wollongong
point2 <- c(144.946457, -37.840935) #Melbourne

midpoint_coords <- midPoint(point1, point2)
print(midpoint_coords)

#convert midpoints to sf
midpoint_sf <- st_sfc(st_point(midpoint_coords), crs = 4326)  
midpoint_utm <- st_transform(midpoint_sf, crs_proj)

# # build grid
# grid <- st_make_grid(
#   st_as_sfc(st_bbox(countries_utm)),
#   cellsize = 25000, # 25 x 25 km grid
#   square = TRUE
# ) %>%
#   st_sf(grid_id = seq_along(.))
# 
# # Filter grid to Australia
# grid <- grid[lengths(st_intersects(grid, countries_utm)) > 0, ] %>%
#   mutate(grid_id = row_number())
# 
# # Bounding box for cropping
# bb <- st_bbox(countries_utm)
# 
# blank_map <- ggplot() +
#   geom_sf(
#     data = grid,
#     colour = "white",
#     linewidth = 0.1
#   ) +
#   geom_sf(
#     data = countries_utm,
#     fill = NA,
#     colour = "black",
#     linewidth = 0.5
#   ) +
#   coord_sf(
#     xlim = c(bb["xmin"], bb["xmax"]),
#     ylim = c(bb["ymin"], bb["ymax"]),
#     expand = FALSE
#   ) +
#   theme_classic() +
#   theme(
#     panel.grid = element_blank(),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     axis.line = element_blank()
#   ) +
#   labs(x = NULL, y = NULL)

#create circular polygon 
radius_m <- 420000  # 420 km Wollongong and Melbourne are roughly 860 km from each other
fieldwork_area <- st_buffer(midpoint_utm, dist = radius_m) 

#overlay onto grid map
fieldsites <- species_presence_map +
  geom_sf(data = fieldwork_area, fill = NA, color = "black", size = 1)

plot(fieldsites)
ggsave("output/fieldsites.pdf", fieldsites, width = 12, height = 6, dpi = 1200)
