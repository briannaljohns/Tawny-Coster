library(galah)
library(dplyr)
library(stringr)

# add your email to access data
galah_config(email="briannaljohns@gmail.com")
galah_config(atlas="Australia")

# need more info on package or function? 
?galah

# if I need to search for a field or values
search_fields("country") %>% show_values()

# records for Acraea terpsicore, Acraea Andromacha, and shared host plant species in Australia since 2020 - 2026 grouped by year and basis of record
search_taxa("Acraea terpsicore")
search_taxa("Acraea andromacha")
search_taxa("Afrohybanthus enneaspermus")
search_taxa("Adenia heterophylla")

occurrence_data<-galah_call() %>% 
  galah_identify("Acraea terpsicore","Acraea andromacha","Afrohybanthus enneaspermus","Adenia heterophylla") %>% 
  galah_filter(
    year>2020,
    country=="Australia"
  ) %>% 
  atlas_occurrences()

#clean up data rows (correct naming)

data_clean <- occurrence_data %>%
  mutate(
    scientificName = case_when(
      str_detect(scientificName, "^Acraea andromacha") ~ "Acraea andromacha",
      str_detect(scientificName, "^Afrohybanthus enneaspermus") ~ "Afrohybanthus enneaspermus",
      str_detect(scientificName, "^Acraea terpsicore") ~ "Acraea terpsicore",
      str_detect(scientificName, "^Adenia heterophylla") ~ "Adenia heterophylla",
      TRUE ~ scientificName
    )
  )

write.csv(data_clean, file = "ATAAhostplants_occ.csv")

