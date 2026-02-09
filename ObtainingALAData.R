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

# records for Acraea terpsicore and Acraea Andromacha species in Australia since 2020 - 2026 grouped by year and basis of record
search_taxa("Acraea terpsicore")
search_taxa("Passiflora foetida")
search_taxa("Passiflora edulis")

occurrence_data<-galah_call() %>% 
  galah_identify("Acraea terpsicore","Passiflora foetida","Passiflora edulis") %>% 
  galah_filter(
    year>2020,
    country=="Australia"
  ) %>% 
  atlas_occurrences()

#clean up data rows (correct naming)

data_clean <- occurrence_data %>%
  mutate(
    scientificName = case_when(
      str_detect(scientificName, "^Passiflora foetida") ~ "Passiflora foetida",
      str_detect(scientificName, "^Passiflora edulis") ~ "Passiflora edulis",
      str_detect(scientificName, "^Acraea terpsicore") ~ "Acraea terpsicore",
      TRUE ~ scientificName
    )
  )

write.csv(data_clean, file = "ATPFPE_occ.csv")

