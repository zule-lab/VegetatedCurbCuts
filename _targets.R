# === Targets -------------------------------------------------------------

# Source ------------------------------------------------------------------
library(targets)
tar_source('R')


# Options -----------------------------------------------------------------
# Targets
tar_option_set(format = 'qs')
options(timeout = 100)


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
    read.table(
      !!.x,
      header = TRUE,
      skip = 1,
      sep = ',',
      stringsAsFactors = FALSE
    )
  ),

  tar_files(
    temp_files,
    dir('raw-data/temperature-data/', full.names = TRUE)
  ),

  tar_target(
    temp_raw,
    # skip problematic lines in dataset including column names
    read_csv(
      temp_files,
      skip = 5,
      col_types = cols(.default = col_character()),
      col_names = F
    ) %>%
      # add back in column names
      rename(
        date_time = X1,
        temp_F = X2,
        rel_humidity_per = X3,
        heat_index_F = X4,
        dew_point_F = X5,
        point_type = X6
      ) %>%
      # add plot ID column based on file name
      mutate(
        InfrastructureID = str_replace(
          basename(xfun::sans_ext(temp_files)),
          ".*_",
          ''
        )
      ) %>%
      # replace commas with decimals for numeric columns
      mutate(across(
        c("temp_F", "rel_humidity_per", "heat_index_F", "dew_point_F"),
        ~ as.numeric(str_replace(.x, ",", "."))
      )) %>%
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
    temp_pres,
    formula = temp_C_s ~ 1 +
      type +
      tod +
      doy +
      type:tod +
      type:doy +
      tod:doy +
      (1 | date) +
      (1 | InfrastructureID),
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
  ),

  zar_brms(
    temp_config,
    formula = temp_C_s ~ 1 +
      config +
      tod +
      doy +
      config:tod +
      config:doy +
      tod:doy +
      (1 | date) +
      (1 | InfrastructureID),
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
  ),

  zar_brms(
    bees_config,
    formula = Interactions ~ 1 + config,
    family = negbinomial(),
    prior = c(
      prior(normal(0, 0.2), class = "b"),
      prior(normal(0, 0.5), class = "Intercept")
    ),
    backend = 'cmdstanr',
    data = model_data[['veg']],
    chains = 4,
    iter = 1000,
    cores = 4
  ),

  zar_brms(
    veg_config,
    formula = veg_spon_per_s ~ 1 + config,
    family = gaussian(),
    prior = c(
      prior(normal(0, 0.5), class = "b"),
      prior(normal(0, 1), class = "Intercept")
    ),
    backend = 'cmdstanr',
    data = model_data[['veg']] %>%
      select(c(veg_spon_per, config)) %>%
      mutate(
        veg_spon_per = case_when(
          veg_spon_per == "<5" ~ "1",
          .default = veg_spon_per
        ),
        veg_spon_per_s = scale(as.numeric(veg_spon_per))
      ),
    chains = 4,
    iter = 2000,
    cores = 4
  ),

  tar_target(
    model_list,
    list(
      temp_pres_brms_sample,
      temp_config_brms_sample,
      bees_config_brms_sample,
      veg_config_brms_sample
    ) %>%
      setNames(., c('temp_pres', 'temp_config', 'bees_config', 'veg_config'))
  ),

  tar_target(
    model_list_prior,
    list(
      temp_pres_brms_sample_prior,
      temp_config_brms_sample_prior,
      bees_config_brms_sample_prior,
      veg_config_brms_sample_prior
    ) %>%
      setNames(
        .,
        c(
          'temp_pres_prior',
          'temp_config_prior',
          'bees_config_prior',
          'veg_config_prior'
        )
      )
  ),

  # prior checks
  tar_render(
    prior_predictive,
    'graphics/diagnostics/prior_predictive.qmd'
  ),

  # model diagnostics
  tar_render(
    model_diagnostics,
    'graphics/diagnostics/model_diagnostics.qmd'
  ),

  # model figures
  tar_target(
    temp_pres_plot,
    create_temp_pres_plot(model_list[['temp_pres']], model_data[['temp_fixed']])
  )
)
