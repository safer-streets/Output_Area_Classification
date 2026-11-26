#   ░▒▓██████▓▒░ ░▒▓██████▓▒░ ░▒▓██████▓▒░  
#  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ 
#  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░        
#  ░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░▒▓█▓▒░        
#  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░        
#  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ 
#   ░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░  
#                                           
#                                         
#                                                        
###############################################################################     
# This code was used to update to the final version of the UK Output 
# Area Classification
# Created by the Geographic Data Service:
# Alex Singleton, University of Liverpool
# Owen Goodwin, University of Liverpool
# Paul Longley, University College London
###############################################################################     

########################################################
# Setup and Data Import
########################################################

#---------
# Load packages
#---------

library(tidyverse)
library(sf)
library(magrittr)
library(janitor)
library(scales)
library(arrow)
library(purrr)
library(ggalluvial)
library(h2o)

#---------
# Import spatial data
#---------

# Output Areas (Dec 2021) Boundaries Full Clipped EW (BFC) - E&W
# Manually download https://drive.google.com/uc?export=download&id=1y1CHipxAZ0NOrmd8iSDkOqm4oTqUwtSJ
OA_2021_Boundary_EW <- st_read("Output_Areas_2021_EW_BFC_V8.gpkg")
OA_2021_Boundary_EW %<>%
  rename(OA = OA21CD)

# Output Areas 2022 Scotland (extract from https://spatialdata.gov.scot/geonetwork/srv/api/records/a0a643ba-c0ed-4860-93a9-5fee11ac492a)
OA_2022_Boundary_S <- st_read("https://drive.google.com/uc?export=download&id=1I2-apnfFPix5pjpiWrqWkR4h3-ryLKPu")
OA_2022_Boundary_S %<>%
  rename(OA = code) %>%
  select(OA)

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

# Combine OA and calculate area

OA_2021_Boundary_EW <- st_transform(OA_2021_Boundary_EW, 27700)
OA_2022_Boundary_S <- st_transform(OA_2022_Boundary_S, 27700)
OA_2021_Boundary_NI <- st_transform(OA_2021_Boundary_NI, 27700)

OA_2021_22_Boundaries <- bind_rows(
  OA_2021_Boundary_EW,
  OA_2022_Boundary_S,
  OA_2021_Boundary_NI
) %>%
  mutate( area_ha = as.numeric(st_area(geom)) / 10000)
  

# Clean Up
rm("OA_2021_Boundary_EW","OA_2022_Boundary_S","OA_2021_Boundary_NI")
file.remove("Output_Areas_2021_EW_BFC_V8.gpkg")

# save UK OA file
st_write(OA_2021_22_Boundaries,"OA_2021_22_Boundaries.gpkg")

#---------
#  Pull in census 2021 data, calculate PCT
#---------

############################################################
# Create Input Measures ####################################
############################################################


# Import variables lookup - This table contains all the variables used in OAC 2021
variable_lookup <- read_csv("./data/lookup/Variables_OAC.csv")


# -----------------------------------
# GET ENGLAND AND WALES CENSUS TABLES
# -----------------------------------

# Get England and Wales Census Table List
census_tables <- read_csv("https://github.com/alexsingleton/Census_2021_Output_Areas/raw/main/Table_Metadata.csv",show_col_types = FALSE)

# Read Census Table (Remove ts041 - number of households,ts006 - density)
C_Table_Name_List <- census_tables %>% select(Table_ID) %>% unique() %>% pull()
C_Table_Name_List %<>% setdiff(c("ts041","ts006"))

for (ct_tmpID in C_Table_Name_List) {
  
  # Download Census Table
  CT_tmp <- read_csv(paste0("https://github.com/alexsingleton/Census_2021_Output_Areas/blob/main/output_data/csv/",ct_tmpID,".csv?raw=true"),show_col_types = FALSE)
  
  # Calculate Percentages
  CT_tmp %<>%

    mutate_at(vars(-1:-2), list(PCT = ~(. / !!sym(paste0(ct_tmpID,"0001"))*100)))
  
  assign(ct_tmpID,CT_tmp)
  rm(ct_tmpID,CT_tmp)
  
}


# --------------------------
# GET SCOTLAND CENSUS TABLES
# --------------------------

# Get Scotland Table List
census_tables <- read_csv("https://github.com/Geographic-Data-Service/Scotland_Census_2022_OA/raw/main/output_data/Table_Metadata.csv",show_col_types = FALSE)

# Read Census Table
C_Table_Name_List <- census_tables %>% select(Table_ID) %>% unique() %>% pull()

for (ct_tmpID in C_Table_Name_List) {
  
  # Download Census Table
  CT_tmp <- read_csv(paste0("https://github.com/Geographic-Data-Service/Scotland_Census_2022_OA/blob/main/output_data/csv/",ct_tmpID,".csv?raw=true"),show_col_types = FALSE)
  
  # Calculate Percentages
  CT_tmp %<>%
    mutate_at(vars(-1:-2), list(PCT = ~(. / !!sym(paste0(ct_tmpID,"0001"))*100))) %>%
   mutate_at(vars(ends_with("PCT")), ~ ifelse(is.nan(.), 0, .)) # this is needed to catch 4 OA (S00150454,S00175452,S00176350,S00178567) where denominators are 0 in UV604 and ts063
  
  assign(ct_tmpID,CT_tmp)
  rm(ct_tmpID,CT_tmp)
  
}




# -----------------------------------
# GET NORTHERN IRELAND CENSUS TABLES
# -----------------------------------

# Get Northern Ireland Table List
census_tables <- read_csv("https://github.com/Geographic-Data-Service/Northern_Ireland_Census_2022_Data_Zone/raw/main/Table_Metadata.csv",show_col_types = FALSE)

# Read Census Table
C_Table_Name_List <- census_tables %>% select(Table_ID) %>% unique() %>% pull()

for (ct_tmpID in C_Table_Name_List) {
  
  # Download Census Table
  CT_tmp <- read_csv(paste0("https://github.com/Geographic-Data-Service/Northern_Ireland_Census_2022_Data_Zone/blob/main/output_data/csv/",ct_tmpID,".csv?raw=true"),show_col_types = FALSE)
  
  # Calculate Percentages
  CT_tmp %<>%
    rename(OA = DZ) %>%    # not acurate but renamed for consistency
    mutate_at(vars(-1:-2), list(PCT = ~(. / !!sym(paste0(ct_tmpID,"0001"))*100)))
  
  assign(ct_tmpID,CT_tmp)
  rm(ct_tmpID,CT_tmp)
  
}


# -----------------------------------------------
# Non-Percentage Variables
# ----------------------------------------------

# v01 - Usual Residents per Square Hectare
# =======================================

# E&W
Non_PCT_EW <- ts001 %>%
  mutate(tot_pop = ts0010001) %>%
  select(OA,tot_pop)
# NI
Non_PCT_NI <- ni001 %>%
  mutate(tot_pop = ni0010001) %>%
  select(OA,tot_pop)
# S
Non_PCT_S <- UV103 %>%
  mutate(tot_pop = UV1030001) %>%
  select(OA,tot_pop)
# UK
Density_UK <-Non_PCT_EW %>%
  bind_rows(Non_PCT_NI, Non_PCT_S) %>%
  left_join(OA_2021_22_Boundaries) %>%
  mutate(v01 = tot_pop/area_ha) %>%
  select(OA, v01)


# UK
Pop_UK <-Non_PCT_EW %>%
  bind_rows(Non_PCT_NI, Non_PCT_S)

write_parquet(Pop_UK,"./data/Pop_UK.parquet")

rm(Non_PCT_EW,Non_PCT_NI,Non_PCT_S)

Density_UK %<>%
mutate(v01 = if_else(OA == "E00187556", 0, v01)) #Zero E00187556 to match ONS official density estimates


# SDR - Import Disability / Health and Population Data
# ====================================================

# England & Wales (Files from https://www.ons.gov.uk/datasets/create)
# disability_EW <- read_csv("./data/census/E_W_Disability.csv")
# For size, these data were chunked using ./extra_code/Chunk_CSV.R
# All usual residents; Age: 6 Categories; Disability: 3 Categories
# 183,765 out of 188,880 areas available
disability_EW <- map_dfr(c("./data/census/E_W_Disability_chunk_1.csv",
                            "./data/census/E_W_Disability_chunk_2.csv",
                            "./data/census/E_W_Disability_chunk_3.csv"
                            ), read_csv)

pop_EW <- read_csv("./data/census/E_W_Age_6_Cat.csv") # Population Data

disability_EW %<>%
  rename(disability = Observation) %>%
  clean_names() %>%
  select(output_areas,age_6_categories,disability_3_categories,disability) %>%
  filter(disability_3_categories == "Disabled under the Equality Act")

pop_EW %<>%
  rename(population = Observation) %>%
  clean_names() %>%
  select(output_areas,age_6_categories,population)

disability_EW<- pop_EW %<>%
  left_join(disability_EW, by = c("output_areas","age_6_categories")) %>%
        select(output_areas,age_6_categories,disability,population) %>%
  mutate(disability = replace_na(disability, 0)) %>%# this is needed as for some OA these were supressed for disclosure
  rename(OA  = output_areas)


# Scotland
# Disability
disability_S <- read_parquet("https://github.com/Geographic-Data-Service/Scotland_Census_2022_OA/raw/refs/heads/main/output_data/parquet/UV303b.parquet")

# Create the disability counts by age bands
disability_S %<>%
mutate(
Aged_15_years_and_under = UV303b0002 - UV303b0023,
Aged_16_to_24_years = UV303b0003 - UV303b0024,
Aged_25_to_34_years = UV303b0004 - UV303b0025,
Aged_35_to_49_years = UV303b0005 - UV303b0026,
Aged_50_to_64_years = UV303b0006 - UV303b0027,
Aged_65_years_and_over = UV303b0007 - UV303b0028) %>%
select(OA, starts_with("Aged"))

# Pivot table into longer format and match E&W format
disability_S %<>%
pivot_longer(
  cols = starts_with("Aged"),
  names_to = "age_6_categories",
  values_to = "disability"
) %>%
  mutate(age_6_categories = str_replace_all(age_6_categories, "_", " "))

# Population

population_S <- read_parquet("https://github.com/Geographic-Data-Service/Scotland_Census_2022_OA/raw/refs/heads/main/output_data/parquet/UV101b.parquet")

# Create the population counts by age bands
population_S %<>%
  mutate(
    Aged_15_years_and_under = UV101b0002,
    Aged_16_to_24_years = UV101b0003,
    Aged_25_to_34_years = UV101b0004,
    Aged_35_to_49_years = UV101b0005,
    Aged_50_to_64_years = UV101b0006,
    Aged_65_years_and_over = UV101b0007) %>%
  select(OA, starts_with("Aged"))

# Pivot table into longer format and match E&W format
population_S %<>%
  pivot_longer(
    cols = starts_with("Aged"),
    names_to = "age_6_categories",
    values_to = "population"
  ) %>%
  mutate(age_6_categories = str_replace_all(age_6_categories, "_", " "))

disability_S <- population_S %<>%
  left_join(disability_S, by = c("OA","age_6_categories")) %>%
  select(OA,age_6_categories,disability,population)


# Northern Ireland
# For closest comparability, 8 category age bands were used and concatenated
# 3,757 out of 3,780 areas pass confidentiality checks.
# Disability - https://build.nisra.gov.uk/en/custom/data?d=PEOPLE&v=DZ21&v=DISABILITY_DVO&v=AGE_BAND_AGG8

disability_NI <- read_csv("./data/census/NI_Disability.csv")

disability_NI %<>%
  clean_names() %>%
  select(census_2021_data_zone_code,health_problem_or_disability_long_term_label,age_8_categories_label ,count) %>%
  filter(health_problem_or_disability_long_term_label %in% c("Day-to-day activities limited a lot","Day-to-day activities limited a little")) %>%
  mutate(age_8_categories_label = case_when(
    age_8_categories_label == "0-15 years" ~ "Aged 15 years and under",
    age_8_categories_label == "16-24 years" ~ "Aged 16 to 24 years",
    age_8_categories_label == "25-34 years" ~ "Aged 25 to 34 years",
    age_8_categories_label == "35-44 years" ~ "Aged 35 to 49 years", # Not an exact age match
    age_8_categories_label == "45-54 years" ~ "Aged 50 to 64 years", # Not an exact age match
    age_8_categories_label == "55-64 years" ~ "Aged 50 to 64 years", # Not an exact age match
    age_8_categories_label == "65-74 years" ~ "Aged 65 years and over",
    age_8_categories_label == "75+ years" ~ "Aged 65 years and over"
    )) %>%
  select(!health_problem_or_disability_long_term_label) %>% #remove to combine the two disability measures
  group_by(census_2021_data_zone_code,age_8_categories_label) %>%
  summarise(disability = sum(count, na.rm = TRUE)) %>%
  rename(age_6_categories_label = age_8_categories_label) %>%
  rename(DZ = census_2021_data_zone_code)

population_NI <- read_parquet("https://github.com/Geographic-Data-Service/Northern_Ireland_Census_2022_Data_Zone/raw/refs/heads/main/output_data/parquet/ni023.parquet")

# Create the population counts by age bands
population_NI %<>%
  mutate(
    V0_15_years = ni0230002,
    V16_24_years = ni0230003,
    V25_34_years = ni0230004,
    V35_44_years = ni0230005,
    V45_54_years = ni0230006,
    V55_64_years = ni0230007,
    V65_74_years = ni0230008,
    V75_years = ni0230009) %>%
  select(DZ, starts_with("V"))

# Pivot table into longer format and match E&W format
population_NI %<>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "age_8_categories_label",
    values_to = "population"
  ) %>%
  mutate(age_8_categories_label = case_when(
    age_8_categories_label == "V0_15_years" ~ "Aged 15 years and under",
    age_8_categories_label == "V16_24_years" ~ "Aged 16 to 24 years",
    age_8_categories_label == "V25_34_years" ~ "Aged 25 to 34 years",
    age_8_categories_label == "V35_44_years" ~ "Aged 35 to 49 years", # Not an exact age match
    age_8_categories_label == "V45_54_years" ~ "Aged 50 to 64 years", # Not an exact age match
    age_8_categories_label == "V55_64_years" ~ "Aged 50 to 64 years", # Not an exact age match
    age_8_categories_label == "V65_74_years" ~ "Aged 65 years and over",
    age_8_categories_label == "V75_years" ~ "Aged 65 years and over"
  )) %>%
  group_by(DZ,age_8_categories_label) %>%
  summarise(population = sum(population, na.rm = TRUE)) %>%
  rename(age_6_categories_label = age_8_categories_label)


disability_NI <- population_NI %<>%
  left_join(disability_NI, by = c("DZ","age_6_categories_label")) %>%
  select(DZ,age_6_categories_label,disability,population) %>%
  rename(OA = DZ) %>%
  rename(age_6_categories = age_6_categories_label)

# Create a combined UK file

disability_UK <- bind_rows(disability_EW,disability_S,disability_NI)

# Calculate UK Age Group Rates
disability_rate <-  disability_UK %>%
          group_by(age_6_categories) %>%
          summarise(
            total_disability = sum(disability, na.rm = TRUE),
            total_population = sum(population, na.rm = TRUE)) %>%
          mutate(UK_Rate = total_disability / total_population) %>%
          select(age_6_categories,UK_Rate)

# Append UK Rates and Calculate Expected
disability_UK %<>%
  left_join(disability_rate) %>%
  mutate(expected = population * UK_Rate)

# Calculate SDR (v42)
disability_UK %<>%
  group_by(OA) %>%
  summarise(
    total_observed = sum(disability),
    total_expected = sum(expected)) %>%
  mutate(v42 = total_observed / total_expected) %>%
  select(OA,v42)

# 
# ====================================================


# -----------------------------------------------
# Single Variables
# ----------------------------------------------
# These use the census tables and % that were calculated earlier

# England and Wales

# Get a list of input variables that are not combinations or non percentages
v_s <- variable_lookup %>%
        filter((!str_detect(engwals_codes, "&")) & is.na(Non_PCT)) %>%
        select(engwals_codes) %>%
        pull()

v_t <- unique(sub("....$", "", v_s)) # table list
v_t <- map(v_t, get) # convert to a list of objects
v_s <- paste0(v_s,"_PCT") # Append _PCT to select percentages

# Create the temporary table to hold the percentages
PCT_EW <- v_t %>% 
  reduce(full_join, by = "OA") %>%
  select(OA, all_of(v_s)) %>%
  rename_all(~str_replace_all(., "_PCT", "")) # remove the _PCT from column names

# Get the OAC variable names from the lookup table
V_name <- variable_lookup %>%
            filter(engwals_codes %in% colnames(PCT_EW)) %>%
            select(No.) %>%
            pull()
          
# Change the census variable names to the OAC variable names
PCT_EW %<>% 
  rename_with(~ c("OA",V_name), everything())


# Scotland


# This is a surrogate variable included at Ward
# level using the Scotland table builder.
# The OA level variable is created in ./extra_code/Scotland_Ward.R

UV213 <- read_csv("./data/UV213.csv")

# Get a list of input variables that are not combinations or non percentages
v_s <- variable_lookup %>%
  filter((!str_detect(scot_codes, "&")) & is.na(Non_PCT)) %>%
  select(scot_codes) %>%
  pull()

v_t <- unique(sub("....$", "", v_s)) # table list
v_t <- map(v_t, get) # convert to a list of objects
v_s <- paste0(v_s,"_PCT") # Append _PCT to select percentages

# Create the temporary table to hold the percentages
PCT_S <- v_t %>% 
  reduce(full_join, by = "OA") %>%
  select(OA, all_of(v_s)) %>%
  rename_all(~str_replace_all(., "_PCT", "")) # remove the _PCT from column names

# Get the OAC variable names from the lookup table
V_name <- variable_lookup %>%
  filter(scot_codes %in% colnames(PCT_S)) %>%
  select(No.) %>%
  pull()

# Change the census variable names to the OAC variable names
PCT_S %<>% 
  rename_with(~ c("OA",V_name), everything())


# Northern Ireland

# Get a list of input variables that are not combinations or non percentages
v_s <- variable_lookup %>%
  filter((!str_detect(ni_codes, "&")) & is.na(Non_PCT)) %>%
  select(ni_codes) %>%
  pull()

v_t <- unique(sub("....$", "", v_s)) # table list
v_t <- map(v_t, get) # convert to a list of objects
v_s <- paste0(v_s,"_PCT") # Append _PCT to select percentages

# Create the temporary table to hold the percentages
PCT_NI <- v_t %>% 
  reduce(full_join, by = "OA") %>%
  select(OA, all_of(v_s)) %>%
  rename_all(~str_replace_all(., "_PCT", "")) # remove the _PCT from column names

# Get the OAC variable names from the lookup table
V_name <- variable_lookup %>%
  filter(ni_codes %in% colnames(PCT_NI)) %>%
  select(No.) %>%
  pull()

# Change the census variable names to the OAC variable names
PCT_NI %<>% 
  rename_with(~ c("OA",V_name), everything())




#--------------------------------------------------------------------
# Combined Variables (England and Wales, Scotland, Northern Ireland)
#--------------------------------------------------------------------

# England and Wales

# v03 - Aged 5 to 14 years
v03 <- ts007a %>% select(OA, ts007a0003_PCT,ts007a0004_PCT) %>% 
  mutate(v03 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v03)

# v04 - Aged 25 to 44 years
v04 <- ts007a %>% select(OA, ts007a0007_PCT,ts007a0008_PCT,ts007a0009_PCT,ts007a0010_PCT) %>% 
  mutate(v04 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v04)

# v05 - Aged 45 to 64 years
v05 <- ts007a %>% select(OA, ts007a0011_PCT,ts007a0012_PCT,ts007a0013_PCT,ts007a0014_PCT) %>% 
  mutate(v05 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v05)

# v06 - Aged 65 to 84 years
v06 <- ts007a %>% select(OA, ts007a0015_PCT,ts007a0016_PCT,ts007a0017_PCT,ts007a0018_PCT) %>% 
  mutate(v06 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v06)

# v20 - Cannot speak English well or at all
v20 <- ts029 %>% select(OA,ts0290006_PCT, ts0290007_PCT) %>%
  mutate(v20 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v20)

# v26 - Separated or divorced
v26 <- ts002 %>% select(OA, ts0020010_PCT,ts0020013_PCT) %>% 
  mutate(v26 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v26)

# v28 - Families with no children
v28 <- ts003 %>% select(OA,  ts0030008_PCT, ts0030012_PCT) %>% 
  mutate(v28 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v28)

# v29 - Families with dependent children
v29 <- ts003 %>% select(OA, ts0030009_PCT,ts0030013_PCT,ts0030016_PCT) %>% 
  mutate(v29 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v29)

# v36 - Flat, maisonette or apartment
v36 <- ts044 %>% select(OA, ts0440005_PCT,ts0440006_PCT,ts0440007_PCT) %>% 
  mutate(v36 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v36)
 
# v37 - Ownership or shared ownership
v37 <- ts054 %>% select(OA,ts0540002_PCT,ts0540005_PCT) %>% 
  mutate(v37 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v37)

# v43 - Provides unpaid care
v43 <- ts039 %>% select(OA,ts0390003_PCT, ts0390004_PCT, ts0390005_PCT, ts0390006_PCT, ts0390007_PCT, ts0390008_PCT, ts0390009_PCT) %>% 
  mutate(v43 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v43)

# v44 - 2 or more cars or vans in household
v44 <- ts045 %>% select(OA,ts0450004_PCT, ts0450005_PCT) %>% 
  mutate(v44 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v44)

# v45 - Highest level of qualification: Level 1- 2 or Apprenticeship
v45 <- ts067 %>% select(OA,ts0670003_PCT, ts0670004_PCT, ts0670005_PCT) %>% 
  mutate(v45 = rowSums(across(where(is.numeric)))) %>%
  select(OA,v45)

# Combined variables for England and Wales
# List of data
list_of_dfs <- list(v03,v04,v05,v06,v20,v26,v28,v29,v36,v37,v43,v44,v45)

# Combine
combined_England_Wales <- list_of_dfs %>%
  reduce(left_join, by = "OA")

# Remove unwanted tibble
rm(list=c("v03","v04", "v05", "v06", "v20", "v26", "v28", "v29", "v36", "v37", "v43", "v44", "v45"))
rm(list_of_dfs)




# Scotland

# v02 - Aged 4 years and under
v02 <- UV103 %>% select(OA, UV1030002_PCT, UV1030003_PCT, UV1030004_PCT, UV1030005_PCT, UV1030006_PCT) %>% 
  mutate(v02 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v02)

# v03 - Aged 5 - 14 years
v03 <- UV103 %>% select(OA, UV1030007_PCT, UV1030008_PCT, UV1030009_PCT, UV1030010_PCT, UV1030011_PCT, UV1030012_PCT, UV1030013_PCT, UV1030014_PCT, UV1030015_PCT, UV1030016_PCT) %>% 
  mutate(v03 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v03)

# v04 - Aged 25 - 44 years
v04 <- UV103 %>% select(OA, UV1030027_PCT, UV1030028_PCT, UV1030029_PCT, UV1030030_PCT, UV1030031_PCT, UV1030032_PCT, UV1030033_PCT, UV1030034_PCT, UV1030035_PCT, UV1030036_PCT, UV1030037_PCT, UV1030038_PCT, UV1030039_PCT, UV1030040_PCT, UV1030041_PCT, UV1030042_PCT, UV1030043_PCT, UV1030044_PCT, UV1030045_PCT, UV1030046_PCT) %>% 
  mutate(v04 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v04)

# v05 - Aged 45 - 64 years
v05 <- UV103 %>% select(OA, UV1030047_PCT, UV1030048_PCT, UV1030049_PCT, UV1030050_PCT, UV1030051_PCT, UV1030052_PCT, UV1030053_PCT, UV1030054_PCT, UV1030055_PCT, UV1030056_PCT, UV1030057_PCT, UV1030058_PCT, UV1030059_PCT, UV1030060_PCT, UV1030061_PCT, UV1030062_PCT, UV1030063_PCT, UV1030064_PCT, UV1030065_PCT, UV1030066_PCT) %>% 
  mutate(v05 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v05)

# v06 - Aged 65 - 84 years
v06 <- UV103 %>% select(OA, UV1030067_PCT, UV1030068_PCT, UV1030069_PCT, UV1030070_PCT, UV1030071_PCT, UV1030072_PCT, UV1030073_PCT, UV1030074_PCT, UV1030075_PCT, UV1030076_PCT, UV1030077_PCT, UV1030078_PCT, UV1030079_PCT, UV1030080_PCT, UV1030081_PCT, UV1030082_PCT, UV1030083_PCT, UV1030084_PCT, UV1030085_PCT, UV1030086_PCT) %>% 
  mutate(v06 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v06)

# v07 - Aged 85 years and over
v07 <- UV103 %>% select(OA, UV1030087_PCT, UV1030088_PCT, UV1030089_PCT, UV1030090_PCT, UV1030091_PCT, UV1030092_PCT, UV1030093_PCT, UV1030094_PCT, UV1030095_PCT, UV1030096_PCT, UV1030097_PCT, UV1030098_PCT, UV1030099_PCT, UV1030100_PCT, UV1030101_PCT, UV1030102_PCT) %>% 
  mutate(v07 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v07)

# v09 - Country of birth: Europe: EU countries (this has a slightly different
# calculation to E, NI, W)
v09 <- UV204b %>% select(OA, UV204b0022_PCT, UV204b0043_PCT) %>% 
  mutate(v09 = UV204b0022_PCT - UV204b0043_PCT) %>%
  select(OA, v09)

# v17 - 	Ethnic group: Black
v17 <- UV201 %>% select(OA, UV2010016_PCT, UV2010019_PCT) %>% 
  mutate(v17 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v17)

# v20 - Cannot speak English well or at all
v20 <- UV210 %>% select(OA, UV2100002, UV2100006,UV2100007,UV2100008,UV2100010,UV2100011) %>% 
  mutate(v20 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v20)

# v22 - Christian
v22 <- UV205 %>% select(OA, UV2050002_PCT, UV2050003_PCT, UV2050004_PCT) %>% 
  mutate(v22 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v22)

# v26 - Separated or divorced
v26 <- UV104 %>% select(OA, UV1040004_PCT, UV1040005_PCT) %>% 
  mutate(v26 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v26)

# v28 - Families with no children
v28 <- UV113 %>% select(OA, UV1130008_PCT, UV1130013_PCT) %>% 
  mutate(v28 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v28)

# v29 - Families with dependent children
v29 <- UV113 %>% select(OA, UV1130009_PCT, UV1130010_PCT, UV1130014_PCT, UV1130015_PCT, UV1130018_PCT, UV1130019_PCT, UV1130022_PCT, UV1130023_PCT) %>% 
  mutate(v29 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v29)

# v36 - Flat maisonette or apartment
v36 <- UV402 %>% select(OA, UV4020007_PCT, UV4020008_PCT, UV4020009_PCT) %>% 
  mutate(v36 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v36)

# v37 - Ownership or shared ownership
v37 <- UV404 %>% select(OA, UV4040002_PCT, UV4040005_PCT, UV4040006_PCT) %>% 
  mutate(v37 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v37)

# v44 - 2 or more cars or vans in household
v44 <- UV405 %>% select(OA, UV4050004_PCT, UV4050005_PCT, UV4050006_PCT) %>% 
  mutate(v44 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v44)

# v45 - Highest level of qualification: Level 1- 2 or Apprenticeship
v45 <- UV501 %>% select(OA, UV5010003_PCT, UV5010005_PCT) %>% 
  mutate(v45 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v45)

# v47 - Highest level of qualification: Level 4 qualifications or above
v47 <- UV501 %>% select(OA, UV5010006_PCT, UV5010007_PCT) %>% 
  mutate(v47 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v47)

# v48 - Hours worked: Part-time
v48 <- UV604 %>% select(OA, UV6040002_PCT, UV6040003_PCT) %>% 
  mutate(v48 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v48)

# v49 - Hours worked: Full-time
v49 <- UV604 %>% select(OA, UV6040004_PCT, UV6040005_PCT) %>% 
  mutate(v49 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v49)

# Combined variables for Scotland
  # List of data
  list_of_dfs <- list(v02, v03,v04,v05,v06,v07,v09,v17,v20,v22,v26,v28,v29,v36,v37,v44,v45,v47,v48,v49)
  
  # Combine
  combined_Scotland <- list_of_dfs %>%
    reduce(left_join, by = "OA")
  
  # Remove unwanted tibble
  rm(list=c("v02", "v03","v04","v05","v06","v07","v09","v17","v20","v22","v26","v28","v29","v36","v37","v44","v45","v47","v48","v49"))
  
  
  
  
# Northern Ireland
  
# v02 - Aged 4 years and under
v02 <- ni025 %>% select(OA, ni0250002_PCT, ni0250003_PCT, ni0250004_PCT, ni0250005_PCT, ni0250006_PCT) %>% 
  mutate(v02 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v02)

# v03 - Aged 5 - 14 years
v03 <- ni025 %>% select(OA, ni0250007_PCT, ni0250008_PCT, ni0250009_PCT, ni0250010_PCT, ni0250011_PCT, ni0250012_PCT, ni0250013_PCT, ni0250014_PCT, ni0250015_PCT, ni0250016_PCT) %>% 
  mutate(v03 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v03)  
  
# v04 - Aged 25 - 44 years
v04 <- ni025 %>% select(OA, ni0250027_PCT, ni0250028_PCT, ni0250029_PCT, ni0250030_PCT, ni0250031_PCT, ni0250032_PCT, ni0250033_PCT, ni0250034_PCT, ni0250035_PCT, ni0250036_PCT, ni0250037_PCT, ni0250038_PCT, ni0250039_PCT, ni0250040_PCT, ni0250041_PCT, ni0250042_PCT, ni0250043_PCT, ni0250044_PCT, ni0250045_PCT, ni0250046_PCT) %>% 
  mutate(v04 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v04)  

# v05 - Aged 45 - 64 years
v05 <- ni025 %>% select(OA, ni0250047_PCT, ni0250048_PCT, ni0250049_PCT, ni0250050_PCT, ni0250051_PCT, ni0250052_PCT, ni0250053_PCT, ni0250054_PCT, ni0250055_PCT, ni0250056_PCT, ni0250057_PCT, ni0250058_PCT, ni0250059_PCT, ni0250060_PCT, ni0250061_PCT, ni0250062_PCT, ni0250063_PCT, ni0250064_PCT, ni0250065_PCT, ni0250066_PCT) %>% 
  mutate(v05 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v05)  

# v06 - Aged 65 - 84 years
v06 <- ni025 %>% select(OA, ni0250067_PCT, ni0250068_PCT, ni0250069_PCT, ni0250070_PCT, ni0250071_PCT, ni0250072_PCT, ni0250073_PCT, ni0250074_PCT, ni0250075_PCT, ni0250076_PCT, ni0250077_PCT, ni0250078_PCT, ni0250079_PCT, ni0250080_PCT, ni0250081_PCT, ni0250082_PCT, ni0250083_PCT, ni0250084_PCT, ni0250085_PCT, ni0250086_PCT) %>% 
  mutate(v06 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v06)  

# v07 - Aged 85 years and over
v07 <- ni025 %>% select(OA, ni0250087_PCT, ni0250088_PCT, ni0250089_PCT, ni0250090_PCT, ni0250091_PCT, ni0250092_PCT, ni0250093_PCT, ni0250094_PCT, ni0250095_PCT, ni0250096_PCT, ni0250097_PCT, ni0250098_PCT, ni0250099_PCT, ni0250100_PCT, ni0250101_PCT, ni0250102_PCT) %>% 
  mutate(v07 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v07) 

# v08 - Country of birth: Europe: United Kingdom
v08 <- ni033 %>% select(OA, ni0330002_PCT, ni0330003_PCT) %>% 
  mutate(v08 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v08) 


# v09 -Country of birth: Europe: EU countries
v09 <- ni033 %>% select(OA, ni0330004_PCT, ni0330005_PCT) %>% 
  mutate(v09 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v09)

# v12 - Bangladeshi (these are not recorded for NI as the population is so low - replaced by zeros)
v12 <- ni033 %>% select(OA) %>% 
  mutate(v12 = 0) %>%
  select(OA, v12)

# v20 - Cannot speak English well or at all
v20 <- ni056 %>% select(OA, ni0560005_PCT, ni0560006_PCT) %>% 
  mutate(v20 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v20) 

# v26 -Separated or divorced
v26 <- ni137 %>% select(OA, ni1370005_PCT, ni1370006_PCT) %>% 
  mutate(v26 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v26) 

# v29 - Families with dependent children
v29 <- ni252 %>% select(OA, ni2520005_PCT, ni2520007_PCT) %>% 
  mutate(v29 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v29)

# v36 - Flat maisonette or apartment
v36 <- ni199 %>% select(OA,ni1990005_PCT, ni1990006_PCT, ni1990007_PCT) %>% 
  mutate(v36 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v36)

# v45 - Highest level of qualification: Level 1- 2 or Apprenticeship
v45 <- ni110 %>% select(OA,ni1100003_PCT, ni1100004_PCT, ni1100005_PCT) %>% 
  mutate(v45 = rowSums(across(where(is.numeric)))) %>%
  select(OA, v45)


# Combined variables for Northern Ireland
# List of data
list_of_dfs <- list(v02, v03,v04,v05,v06,v07,v08,v09,v12,v20,v26,v29,v36,v45)

# Combine
combined_Northern_Ireland <- list_of_dfs %>%
  reduce(left_join, by = "OA")

# Remove unwanted tibble
rm(list=c("v02", "v03","v04","v05","v06","v07","v08","v09","v12","v20","v26","v29","v36","v45"))








############################################################
# Assemble and Prepare Cluster Inputs
############################################################

#---------
# Combine All the Variables
#---------

England_and_Wales <- combined_England_Wales %>%
  left_join(PCT_EW)

Scotland <- combined_Scotland %>%
  left_join(PCT_S)

Northern_Ireland <- combined_Northern_Ireland %>%
  left_join(PCT_NI)

# Order the columns consistently (this excludes V40 and V41 which are not included in the UK model)
England_and_Wales %<>%
select(OA,v02, v03, v04, v05, v06, v07, v08, v09, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53, v54, v55, v56, v57, v58, v59, v60)

Scotland %<>%
select(OA,v02, v03, v04, v05, v06, v07, v08, v09, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53, v54, v55, v56, v57, v58, v59, v60)

Northern_Ireland %<>%
select(OA,v02, v03, v04, v05, v06, v07, v08, v09, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53, v54, v55, v56, v57, v58, v59, v60)

# Bind the rows and append disability and density data

OAC_Input <- England_and_Wales %>%
                bind_rows(Scotland) %>%
                bind_rows(Northern_Ireland)%>%
                left_join(Density_UK) %>%
                left_join(disability_UK)




#---------
# Calculate the Inverse hyperbolic sine and range standardize
#---------

OAC_Input_IHS <- OAC_Input %>%
  mutate_at(vars(-1), ~asinh(.)) %>%
  mutate_at(vars(-1), ~rescale(.))

# Save the raw input data
# NB: the E&W rows written here don't reproduce the E&W assignments, which are the
# Preview OAC assignments, unchanged (see below). The committed OAC_Input_IHS.parquet
# has the Preview inputs for E&W; run extra_code/preview_ew_inputs.py after this
# step to restore them.
write_parquet(OAC_Input, "./data/OAC_Input.parquet")
write_parquet(OAC_Input_IHS, "./data/OAC_Input_IHS.parquet")


# ---------------------------------------------------------
# Checks the comparability of Preview and Final OAC inputs
# ---------------------------------------------------------
# Preview OAC relates to the currently disseminated ONS model

# Creates lookups to translate Preview and Final OAC headings
old_new_lookup <- read_csv("./data/lookup/old_v_new.csv") # manually created to align variable names
OAC_Preview_Variables <- read_csv("https://github.com/jakubwyszomierski/OAC2021-2/raw/refs/heads/main/Data/Lookups/OAC_variables.csv")

OAC_Preview_Variables %<>%
  left_join(old_new_lookup, by = c("encoding" = "Preview"))

OAC_Preview_Variables %<>%
  select(Code,Final)

# Get Preview OAC input data and adjust variable names to match Final OAC

Aged_Scotland_PCT <- read_csv("./data/UKOAC/hybrid_UK_2021_aged_Scotland_prop_perc.csv")
rename_vector_PCT <- setNames(OAC_Preview_Variables$Code, OAC_Preview_Variables$Final)

Aged_Scotland_PCT %<>%
  rename(!!!rename_vector_PCT) %>%
  select(Geography_Code,starts_with("v")) %>%
  rename(OA = Geography_Code)


# Get Final OAC input data

OAC_Input <- read_parquet("./data/OAC_Input.parquet")

# Transform and Standardise both input data

OAC_Input <- OAC_Input %>%
  mutate_at(vars(-1), ~asinh(.)) %>%
  mutate_at(vars(-1), ~rescale(.))

Aged_Scotland_PCT <- Aged_Scotland_PCT %>%
  mutate_at(vars(-1), ~asinh(.)) %>%
  mutate_at(vars(-1), ~rescale(.))


# Add a new Country column to each
OAC_Input_L <- OAC_Input %>%
  mutate(Country = substring(OA, 1, 1))  %>%
  select(OA,{
    # Pick out all column names that begin with "v"
    v_cols <- grep("^v", names(.), value = TRUE)
    # Sort them, then append the rest of the columns (in original order)
    c(sort(v_cols), setdiff(names(.), v_cols))
  })

Aged_Scotland_PCT_L <- Aged_Scotland_PCT %>%
  mutate(Country = substring(OA, 1, 1))    %>%
  select(OA,{
    # Pick out all column names that begin with "v"
    v_cols <- grep("^v", names(.), value = TRUE)
    # Sort them, then append the rest of the columns (in original order)
    c(sort(v_cols), setdiff(names(.), v_cols))
  })


# Transform data to long format and combine for plotting

OAC_Input_L %<>%
  pivot_longer(cols = starts_with("v"), 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(dataset = "Final OAC")

Aged_Scotland_PCT_L %<>%
  pivot_longer(cols = starts_with("v"), 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(dataset = "Preview OAC")


df_combined_PCT <- OAC_Input_L %>%
  bind_rows(Aged_Scotland_PCT_L)


# Create comparative box and whisker plots for the two datasets and variables

for (col_name in unique(Aged_Scotland_PCT$variable)) {
  
  # Filter the long data for the current variable
  df_var <- df_combined_PCT %>%
    filter(variable == col_name)
  
  p_var <- ggplot(df_var, aes(x = dataset, y = value, fill = Country )) +
    geom_boxplot() +
    labs(title = paste("Boxplot for", col_name),
         x = "Dataset",
         y = col_name) +
    theme_minimal()
  
  ggsave(filename = paste0("./plot/comparison/","boxplot_", col_name, ".png"),
         plot = p_var, width = 6, height = 4,bg = "white")
}


# The following creates a subset of variables with comparable distributions for 
# use in the reassignment of areas in Scotland and Northern Ireland 

# These variables were deemed problematic for comparative purposes after 
# considering the distributions.
ex_variables <- c("v07","v09","v11","v23","v42","v60","v32","v40","v41")

# Preview UK OAC - this is the current ONS classification with the addition
# of lookups for Scotland and Northern Ireland which used modeled data.
# These data are available from: https://data.cdrc.ac.uk/dataset/uk-oac
UK_OAC_Preview <- read_csv("./data/UKOAC/UKOAC21_assignment.csv")

# Append preview input data to preview clusters, and remove excluded columns
UK_OAC_Preview %<>%
  left_join(Aged_Scotland_PCT, by = c("Geography_Code" = "OA")) %>%
  select(-any_of(ex_variables))

# Calculate the preview OAC sub groups means from the original data
sub_group_mean <- UK_OAC_Preview %>%
  group_by(Subgroup) %>%
  summarise(across(starts_with("v"), ~ mean(.x, na.rm = TRUE))) %>%
  select(Subgroup,{
    # Pick out all column names that begin with "v"
    v_cols <- grep("^v", names(.), value = TRUE)
    # Sort them, then append the rest of the columns (in original order)
    c(sort(v_cols), setdiff(names(.), v_cols))
  }) %>%
  select(-any_of(ex_variables))

# Prepare final input data to enable Scotland and Northern Ireland update
OAC_Input %<>%
  select(OA,{
    v_cols <- grep("^v", names(.), value = TRUE)
    c(sort(v_cols), setdiff(names(.), v_cols))
  }) %>%
  select(-any_of(ex_variables))

# Create Scotland and Northern Ireland final input
OAC_Input_S_NI <- OAC_Input %>%
  filter(!grepl("^[EW]", OA))

# Function to compute the row with smallest difference for one row of OAC_Input_S_NI
find_closest_row <- function(input_row, sub_group_mean) {
  # Compute row-wise difference
  differences <- apply(sub_group_mean[,-1], 1, function(row) sum((row - input_row)^2))
  # Get the row index with the minimum difference
  which.min(differences)
}

# Apply the function to all rows in OAC_Input
closest_rows <- apply(OAC_Input_S_NI[,-1], 1, function(row) find_closest_row(row, sub_group_mean))

# New Values for Northern Ireland and Scotland
new_values <- sub_group_mean %>%
  slice(closest_rows) %>%
  select(Subgroup) %>%
  bind_cols(OAC_Input_S_NI %>% select(OA)) %>%
  mutate(Group = substr(Subgroup, 1, 2)) %>%
  mutate(Supergroup = substr(Subgroup , 1, 1)) %>%
  mutate(Geography_Code = OA) %>%
  select(Geography_Code,Supergroup,Group,Subgroup)

# This reads the preview lookup and replaces Scotland and Northern Ireland
# results with the updated clusters

UK_OAC_Preview <- read_csv("./data/UKOAC/UKOAC21_assignment.csv",col_types = cols(.default = "c"))

UK_OAC_Final <- UK_OAC_Preview %>%
  filter(grepl("^[EW]", Geography_Code))%>%
  bind_rows(new_values)

write_parquet(UK_OAC_Final, "./data/UK_OAC_Final.parquet") # Final Lookup

# Final lookup geopackage
cluster_assignments_SF <- OA_2021_22_Boundaries %>%
  left_join(UK_OAC_Final, by = c("OA" = "Geography_Code"))

st_write(cluster_assignments_SF, "./map/OAC_SuperGroup.gpkg", driver = "GPKG")

# --------------------------------
# % Alluvial Plot
# --------------------------------

UK_OAC_Final %<>%
  select(Geography_Code,Supergroup) %>%
  rename(Supergroup_New = Supergroup)

UK_OAC_Preview %<>%
  select(Geography_Code,Supergroup) %>%
  rename(Supergroup_Old = Supergroup)

# This file enables linkage of preview (which includes 2011 geography for Scotland
# and Northern Ireland) with 2021 OA. This was created using the code in
# 2011_2021_22_Comparison.R and is based on centroids. Some NA values are created
# in Northern Ireland as the new Data Zones are clipped rather than the extent of
# the realm.

OA_11_21_22_lookup <- read_parquet("./data/OA_11_21_22_lookup.parquet") 

OA_11_21_22_lookup %<>%
  left_join(UK_OAC_Final, by = c("OA" = "Geography_Code")) %>%
  left_join(UK_OAC_Preview, by = c("OA_OAC_Preview" = "Geography_Code" ))

# Define the labels for Supergroups
supergroup_labels <- c(
  "1: Retired Professionals",
  "2: Suburbanites\n& Peri-Urbanites",
  "3: Multicultural\n& Educated Urbanites",
  "4: Low-Skilled Migrant\n& Student Communities",
  "5: Ethnically Diverse\nSuburban Professionals",
  "6: Baseline UK",
  "7: Semi &\nUn-Skilled Workforce",
  "8: Legacy Communities"
)

# Replace Supergroup numbers with labels
OA_11_21_22_lookup <- OA_11_21_22_lookup %>%
  mutate(Supergroup_New  = factor(Supergroup_New , levels = 1:8, labels = supergroup_labels)) %>%
  mutate(Supergroup_Old = factor(Supergroup_Old, levels = 1:8, labels = supergroup_labels))

# Create cross-tabulation
cross_tab <- OA_11_21_22_lookup %>%
  count(Supergroup_Old, Supergroup_New) %>%
  rename(Freq = n) %>%
  filter(!is.na(Supergroup_New))


# Plot flows using ggalluvial
plot <- ggplot(cross_tab, aes(
  axis1 = Supergroup_Old,
  axis2 = Supergroup_New,
  y = Freq
)) +
  geom_alluvium(aes(fill = Supergroup_Old)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size= 2.5) +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_fill_manual(values = c("#C7BB9D","#6DAB57","#E23921","#E38ABC","#E88631","#F2CB4C","#5881F6","#8C569E")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis tick labels
    axis.text.y = element_blank(),  # Remove y-axis tick labels
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave("./plot/Comparison.png", plot = plot, device = "png", width = 6, height = 4, dpi = 300)


# Create cross-tabulation (Scotland & NI Only)
cross_tab_S_NI <- OA_11_21_22_lookup %>%
  filter(!grepl("^[EW]", OA))%>%
  count(Supergroup_Old, Supergroup_New) %>%
  rename(Freq = n) %>%
  filter(!is.na(Supergroup_New)) %>%
  mutate(Supergroup_New = str_replace_all(Supergroup_New, "\n", " ")) %>%
  mutate(Supergroup_Old = str_replace_all(Supergroup_Old, "\n", " "))

# Plot flows using ggalluvial
plot <- ggplot(cross_tab_S_NI, aes(
  axis1 = Supergroup_Old,
  axis2 = Supergroup_New,
  y = Freq
)) +
  geom_alluvium(aes(fill = Supergroup_Old)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size= 1) +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_fill_manual(values = c("#C7BB9D","#6DAB57","#E23921","#E38ABC","#E88631","#F2CB4C","#5881F6","#8C569E")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis tick labels
    axis.text.y = element_blank(),  # Remove y-axis tick labels
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

ggsave("./plot/Comparison_S_NI.png", plot = plot, device = "png", width = 6, height = 4, dpi = 300)


























# Map Old & New #
#####################
# Output Areas (Dec 2021) Boundaries Full Clipped EW (BFC) - E&W
# Manually download https://drive.google.com/uc?export=download&id=1y1CHipxAZ0NOrmd8iSDkOqm4oTqUwtSJ
OA_2021_Boundary_EW <- st_read("Output_Areas_2021_EW_BFC_V8.gpkg")
OA_2021_Boundary_EW %<>%
  rename(OA = OA21CD) %>%
  rename(geometry = geom)

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
NI_SA <- OA_2011 %>%
  filter(startsWith(geo_code, "N"))%>%
  rename(OA = geo_code)

# Scotland
S_SA <- OA_2011 %>%
  filter(startsWith(geo_code, "S"))%>%
  rename(OA = geo_code)

OA_2021_Boundary_EW <- st_transform (OA_2021_Boundary_EW, 27700)
NI_SA <- st_transform (NI_SA, 27700)
S_SA <- st_transform (S_SA, 27700)


UK <- OA_2021_Boundary_EW %>%
  bind_rows(NI_SA,S_SA) %>%
  left_join(UK_OAC_Preview, by = c("OA" = "Geography_Code"))

st_write(UK, "./map/OAC_SuperGroup_Old.gpkg", driver = "GPKG")











