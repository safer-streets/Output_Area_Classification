# Output Areas (Dec 2021) Boundaries Full Clipped EW (BFC) - E&W
# Manually download https://drive.google.com/uc?export=download&id=1y1CHipxAZ0NOrmd8iSDkOqm4oTqUwtSJ
OA_2021_Boundary_EW <- st_read("Output_Areas_2021_EW_BFC_V8.gpkg")
OA_2021_Boundary_EW %<>%
  rename(OA = OA21CD)

# UK 2011 Boundary
temp_zip <- tempfile(fileext = ".zip")
download.file(
  url      = "https://borders.ukdataservice.ac.uk/ukborders/easy_download/prebuilt/shape/infuse_oa_lyr_2011_clipped.zip",
  destfile = temp_zip,
  mode     = "wb"
)
temp_dir <- tempdir()
unzip(zipfile = temp_zip,  exdir = temp_dir)
shp_path <- file.path(temp_dir, "infuse_oa_lyr_2011_clipped.shp")
OA_2011 <- st_read(shp_path)

# NI
NI_SA_Centroids_11 <- OA_2011 %>%
  filter(startsWith(geo_code, "N")) %>%
  st_centroid() %>%
  mutate(OA_OAC_Preview = geo_code) %>%
  select(OA_OAC_Preview)

NI_SA_Centroids_11 <- st_transform (NI_SA_Centroids_11, 27700)

# Scotland
S_OA_Centroids_11 <- OA_2011 %>%
  filter(startsWith(geo_code, "S")) %>%
  st_centroid() %>%
  mutate(OA_OAC_Preview = geo_code) %>%
  select(OA_OAC_Preview)

S_OA_Centroids_11 <- st_transform (S_OA_Centroids_11, 27700)

# get boundaries

# Output Areas 2022 Scotland (extract from https://spatialdata.gov.scot/geonetwork/srv/api/records/a0a643ba-c0ed-4860-93a9-5fee11ac492a)

OA_2022_Boundary_S <- st_read("https://drive.google.com/uc?export=download&id=1I2-apnfFPix5pjpiWrqWkR4h3-ryLKPu")
OA_2022_Boundary_S %<>%
  rename(OA = code) %>%
  select(OA)

OA_2022_Boundary_S <- st_transform (OA_2022_Boundary_S, 27700)

# Data Zones 2021 Northern Ireland
temp_zip <- tempfile(fileext = ".zip")
download.file(
  url      = "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/geography-dz2021-geojson.zip",
  destfile = temp_zip,
  mode     = "wb"
)
temp_dir <- tempdir()
unzip(zipfile = temp_zip, files = "DZ2021.geojson", exdir = temp_dir)
geojson_path <- file.path(temp_dir, "DZ2021.geojson")
OA_2021_Boundary_NI <- st_read(geojson_path)

OA_2021_Boundary_NI %<>%
  rename(OA =   DZ2021_cd) %>%
  select(OA)
st_geometry(OA_2021_Boundary_NI) <- "geom"

OA_2021_Boundary_NI <- st_transform (OA_2021_Boundary_NI, 27700)


#joins

S_OA_Centroids_11 %<>%
  st_join(OA_2022_Boundary_S) %>%
  st_drop_geometry()

NI_SA_Centroids_11 %<>%
  st_join(OA_2021_Boundary_NI) %>%
  st_drop_geometry()

OA_11_21_22_lookup <- OA_2021_Boundary_EW %>%
                      st_drop_geometry() %>%
                      mutate(OA_OAC_Preview = OA) %>%
                      bind_rows(S_OA_Centroids_11) %>%
                      bind_rows(NI_SA_Centroids_11) %>%
                      as_tibble()


write_parquet(OA_11_21_22_lookup, "./data/OA_11_21_22_lookup.parquet")









