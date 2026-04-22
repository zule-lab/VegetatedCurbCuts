# === Targets -------------------------------------------------------------

# install.packages(c('targets','renv'))

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
    'raw-data/donnees_vegetation.csv',
    read.csv(!!.x)
  ),
  
  tar_file_read(
    paysage_raw, 
    'raw-data/architecte_paysage.csv',
    read.csv(!!.x)
  )   
  
  
)