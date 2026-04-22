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
  )   
  
  
)