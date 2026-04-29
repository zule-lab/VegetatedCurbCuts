# === Targets -------------------------------------------------------------

# Source ------------------------------------------------------------------
library(targets)
tar_source('R')



# Options -----------------------------------------------------------------
# Targets
tar_option_set(format = 'qs')
options(timeout=100)


# Renv --------------------------------------------------------------------
activate()
snapshot()
restore()


# Targets -----------------------------------------------------------------
c(
  
  tar_file_read(
    veg_raw,
    'raw-data/vegetative-data-2024.csv',
    read.csv(!!.x)
  ),
  
  tar_file_read(
    bee_raw, 
    'raw-data/bee-data-2024.csv',
    read.csv(!!.x)
  ),
  
  tar_file_read(
    mobile_raw,
    'raw-data/CR350Series_seconds.dat',
    read.table(!!.x, header = TRUE, skip = 1, sep =',', stringsAsFactors = FALSE)
  ),

  tar_files(
    temp_files,
    dir('raw-data/temperature-data/', full.names = TRUE)
    ),
    
    tar_target(
      temp_raw, 
      # skip problematic lines in dataset including column names
      read_csv(temp_files, skip = 5, col_types = cols(.default = col_character()), col_names = F) %>%  
        # add back in column names
        rename(date_time = X1,
               temp_F = X2,
               rel_humidity_per = X3,
               heat_index_F = X4,
               dew_point_F = X5,
               point_type = X6) %>% 
        # add plot ID column based on file name 
        mutate(InfrastructureID = str_replace(basename(xfun::sans_ext(temp_files)), ".*_", '')) %>%
        # replace commas with decimals for numeric columns
        mutate(across(c("temp_F", "rel_humidity_per", "heat_index_F", "dew_point_F"), ~as.numeric(str_replace(.x, ",", ".")))) %>% 
        # remove unnecessary column 
        select(-point_type),
      pattern = map(temp_files)
    ),

    tar_file_read(
      sites,
      'raw-data/InfraVertes_2024.kml',
      read_sf(!!.x)
    ),

    tar_target(
      temp_clean, 
      clean_temp(temp_raw, sites)
    ),
  
  tar_target(
    mobile_clean, 
    clean_mobile(mobile_raw)
  ),

  tar_target(
    model_data,
    clean_model_data(veg_raw, bee_raw, temp_clean, mobile_clean)
  ),

  zar_brms(
    temp_veg_pres,
    formula = temp_C_s ~ 1 + type + tod + doy + type:tod + type:doy + tod:doy + (1 | date) + (1 | InfrastructureID),
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 1), class = "Intercept"),
      prior(exponential(1), class = "sd"),
      prior(exponential(1), class = "sigma")
      ),
      backend = 'cmdstanr',
      data = model_data[['temp_fixed']],
      chains = 4,
      iter = 2000,
      cores = 4
    )
  
)