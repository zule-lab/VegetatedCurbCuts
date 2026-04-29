clean_model_data <- function(veg_raw, bee_raw, temp_clean, mobile_clean){

 bee_veg <- inner_join(bee_raw, veg_raw, by = 'InfrastructureID', suffix = c("_bee", "_veg"))

 temp_type <- temp_clean %>%  
                mutate(type = case_when(str_detect(InfrastructureID, 'C') == T ~ 'Control',
                                        .default = "Vegetated"),
                       date = as.Date(date_time),
                       temp_C_s = scale(temp_C)[,1]) %>% 
                drop_na(temp_C_s) %>% 
                select(c(temp_C_s, type, doy, tod, date, InfrastructureID))

 model_data <- list(bee_veg, temp_type)
 names(model_data) <- c('veg', 'temp_fixed')

 return(model_data)

}

