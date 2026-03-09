library(galah)
library(dplyr)
library(stringr)

# add your email to access ALA data
galah_config(email="briannaljohns@gmail.com")
galah_config(atlas="Australia")

# galah help
?galah

# records for Acraea terpsicore, Acraea Andromacha, and Passiflora species in Australia since 2012 - 2026 grouped by year and basis of record

search_taxa("Passifloraceae")
search_taxa("Acraea andromacha")
search_taxa("Acraea terpsicore")


occurrence_data<-galah_call() %>% 
  galah_identify("Acraea terpsicore","Acraea andromacha","Passifloraceae") %>% 
  galah_filter(
    year>2012,
    country=="Australia"
  ) %>% 
  atlas_occurrences()

write.csv(occurrence_data, file = "output/allocc.csv")

