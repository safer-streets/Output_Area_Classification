library(sf)
library(janitor)

# Read Scotland OA Centoids (extracted from WFS service - https://www.spatialdata.gov.scot/)
C_S_OA <- st_read("./data/OA_Centroid_22_Scotland.gpkg")

# Read 2022 Ward Boundaries
ward <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Wards_December_2022_Boundaries_UK_BFE/FeatureServer/0/query?where=1%3D1&outFields=WD22CD,WD22NM&outSR=27700&f=json")

# Subset to Scotland
ward %<>%
  filter(startsWith(WD22CD, "S"))


# Amend some ward WD22NM to match data table
ward %<>%
  mutate(WD22NM = if_else(WD22CD == "S13002737", "North Isles (Orkney Islands)", WD22NM)) %>%
  mutate(WD22NM = if_else(WD22CD == "S13002772", "North Isles (Shetland Islands)", WD22NM)) %>%
  mutate(WD22NM = if_else(WD22CD == "S13002830", "North East (Dundee City)", WD22NM)) %>%
  mutate(WD22NM = if_else(WD22CD == "S13003133", "North East (Glasgow City)", WD22NM)) %>%
  mutate(WD22NM = if_else(WD22CD == "S13002936", "Bo’ness and Blackness", WD22NM)) %>%
  mutate(WD22NM = if_else(WD22CD == "S13002999", "Eilean a' Chèo", WD22NM)) %>%
  mutate(WD22NM = if_else(WD22CD == "S13003138", "Sgìr’ Ùige agus Càrlabhagh", WD22NM))


# Read Scotland Migration Data (ward 2022 level from census table builder)
# https://www.scotlandscensus.gov.uk/webapi/jsf/dataCatalogueExplorer.xhtml

migration <- read_csv("./data/Migration_Scotland.csv")
migration %<>%
  clean_names()

# Append migration data
ward %<>%
  left_join(migration,by = c("WD22NM" = "ward")) %>%
  mutate(UV2130001_PCT = same_as_the_address_of_enumeration/total*100)


# Spatial join - links OA centroid and ward value

C_S_OA <- st_transform (C_S_OA, 27700)
ward <- st_transform (ward, 27700)

C_S_OA %<>% st_join(ward)

#Subset
UV213 <- C_S_OA %>%
  st_drop_geometry() %>%
  mutate(OA = code) %>%
  select(OA,UV2130001_PCT) %>%
  as_tibble()

write_csv(UV213,"./data/UV213.csv")



