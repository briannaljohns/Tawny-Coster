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
search_taxa("Acraea Andromacha")

ATAA_occurence_data<-galah_call() %>% 
  galah_identify("Acraea terpsicore","Acraea Andromacha") %>% 
  galah_filter(
    year>2020,
    country=="Australia"
  ) %>% 
  atlas_occurrences()

#clean up data rows (correct naming)

ATAA_clean <- ATAA_occurence_data %>%
  mutate(
    scientificName = case_when(
      str_detect(scientificName, "^Acraea andromacha") ~ "Acraea andromacha",
      str_detect(scientificName, "^Acraea terpsicore") ~ "Acraea terpsicore",
      TRUE ~ scientificName
    )
  )

write.csv(ATAA_clean, file = "ATAA_occ.csv")

