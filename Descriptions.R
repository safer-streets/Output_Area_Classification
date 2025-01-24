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
# This code was created to describe the final version of the UK Output 
# Area Classification
# Created by the Geographic Data Service:
# Alex Singleton, University of Liverpool
# Owen Goodwin, University of Liverpool
# Paul Longley, University College London
###############################################################################

############################################################
# Create Descriptive Material
############################################################

library(tidyverse)
library(arrow)
library(magrittr)
library(janitor)

# Import variables lookup - This table contains all the variables used in OAC 2021
variable_lookup <- read_csv("./data/lookup/Variables_OAC.csv")

# Read input data and lookups

OAC_Input <- read_parquet("./data/OAC_Input.parquet")

UK_OAC_Preview <- read_csv("./data/UKOAC/UKOAC21_assignment.csv",col_types = cols(.default = "c"))
UK_OAC_Preview %<>%
  rename( OA = Geography_Code)

UK_OAC_Final <- read_parquet("./data/UK_OAC_Final.parquet")
UK_OAC_Final %<>%
rename( OA = Geography_Code)

# Append Clusters
preview_OAC <- OAC_Input %>%
  left_join(UK_OAC_Preview)

final_OAC <- OAC_Input %>%
  left_join(UK_OAC_Final)

# Final OAC

# Calculate Index Scores and transpose Subgroup
IS_Final_Subgroup <- final_OAC %>%
  group_by(Subgroup) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), \(x) x / mean(x, na.rm = TRUE) * 100)) # index score

IS_Final_Subgroup %<>%
  pivot_longer(-Subgroup, names_to = "no", values_to = "Value") %>%
  pivot_wider(names_from = Subgroup, values_from = Value)

# Append variable names 
Index_Scores_Final_Subgroups <- variable_lookup %>%
  clean_names() %>%
  select(no, variable_name,domain) %>%
  left_join(IS_Final_Subgroup)


# Calculate Index Scores and transpose Group
IS_Final_Group <- final_OAC %>%
  group_by(Group) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), \(x) x / mean(x, na.rm = TRUE) * 100)) # index score

IS_Final_Group %<>%
  pivot_longer(-Group, names_to = "no", values_to = "Value") %>%
  pivot_wider(names_from = Group, values_from = Value)

# Append variable names 
Index_Scores_Final_Groups <- variable_lookup %>%
  clean_names() %>%
  select(no, variable_name,domain) %>%
  left_join(IS_Final_Group)


write_csv(Index_Scores_Final_Groups,"Index_Scores_Final_Groups.csv")


# Calculate Index Scores and transpose Supergroup
IS_Final_Supergroup <- final_OAC %>%
  group_by(Supergroup) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), \(x) x / mean(x, na.rm = TRUE) * 100)) # index score

IS_Final_Supergroup %<>%
  pivot_longer(-Supergroup, names_to = "no", values_to = "Value") %>%
  pivot_wider(names_from = Supergroup, values_from = Value)

# Append variable names 
Index_Scores_Final_Supergroup <- variable_lookup %>%
  clean_names() %>%
  select(no, variable_name,domain) %>%
  left_join(IS_Final_Supergroup)


write_csv(Index_Scores_Final_Supergroup,"Index_Scores_Final_Supergroup.csv")


################################
# Comparison Table #############
################################

# LAD Counts

# This requires the ONSPD (https://geoportal.statistics.gov.uk/datasets/b54177d3d7264cd6ad89e74dd9c1391d/about)


ONSPD <- read_csv("ONSPD_NOV_2024_UK.csv")


ONSPD %<>%
  select(oa21, oslaua) %>%
  filter(! oa21 %in% c("L99999999","M99999999")) %>%
  filter(! is.na(oa21)) %>% 
  rename(OA = oa21) %>%
  left_join(UK_OAC_Final)

LAD_Frq <- ONSPD %>%
  group_by(Subgroup) %>%
  summarise(
    distinct_oslaua = n_distinct(oslaua)
  )



# OA and Population Counts

Pop_UK <- read_parquet("./data/Pop_UK.parquet")

Pop_UK <- UK_OAC_Final %>%
  left_join(Pop_UK)



Pop_UK %<>%
group_by(Subgroup) %>%
  summarise(
    count_OA  = n(),
    sum_pop   = sum(tot_pop)
  ) %>%
  mutate(
    pct_OA    = 100 * count_OA / sum(count_OA),
    pct_pop   = 100 * sum_pop / sum(sum_pop)
  )

descriptive_stats <- LAD_Frq %>%
  left_join(Pop_UK)


write_csv(descriptive_stats,"./data/descriptive_stats.csv")











