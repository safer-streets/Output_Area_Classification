library(tidyverse)


# ---------------------------------------------------------
# Checks the comparability of Preview and Final OAC inputs
# ---------------------------------------------------------


# Creates lookups to translate Preview and Final OAC headings

old_new_lookup <- read_csv("./data/lookup/old_v_new.csv")

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


# These variables were deemed problematic for comparative purposes
ex_variables <- c("v07","v09","v11","v23","v42","v60","v32","v40","v41")

#select(-any_of(ex_variables))
  
# Preview UK OAC  
UK_OAC_Preview <- read_csv("./data/UKOAC/UKOAC21_assignment.csv")

  

  
  
  
Aged_Scotland_PCT
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  