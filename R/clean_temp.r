clean_temp <- function(temp_raw, sites){

  # convert from farenheit to celsius 
  temp_cel <- temp_raw %>% 
    mutate(temp_F = str_replace_all(temp_F, ',', '.'), 
           heat_index_F = str_replace_all(heat_index_F, ',', '.'), 
           dew_point_F = str_replace_all(dew_point_F, ',', '.'), 
           temp_C = round(((as.numeric(temp_F) - 32)*5)/9, 1),
           heat_index_C = round(((as.numeric(heat_index_F) - 32)*5)/9, 1),
           dew_point_C = round(((as.numeric(dew_point_F) - 32)*5)/9, 1))
  
  # format date time / two formats in one dataset - am/pm and 24h
  dt_am <- temp_cel %>% 
    filter(str_detect(date_time, 'a.m.') | str_detect(date_time, 'p.m.')) %>% 
    mutate(date_time = str_replace_all(date_time, '[\\.]',''),
           date_time = strptime(date_time, format = "%Y-%m-%d %I:%M:%S %p"))
  
  dt_24 <- temp_cel %>% 
    filter(!str_detect(date_time, 'a.m.')) %>%
    filter(!str_detect(date_time, 'p.m.')) %>% 
    mutate(date_time = strptime(date_time, format = "%Y-%m-%d %H:%M:%S"))
  
  temp_date <- rbind(dt_am, dt_24)
  
  # select for study period Jun 11 12:00 - Aug 8 9:00 
  temp_study <- temp_date %>% 
    filter(date_time > "2024-06-11 12:00:00 EDT" & date_time < "2024-08-08 9:00:00 EDT")
  
  # calculate daytime hours
   temp_coords <- temp_study %>% 
     left_join(., sites %>% select(c(Name, geometry)), by = join_by(InfrastructureID == Name)) %>% 
     st_as_sf() %>% 
     st_centroid() %>% 
     st_transform(4326) %>%
     mutate(date = date(date_time),
            doy = yday(date),
            lon = st_coordinates(geometry)[,1],
            lat = st_coordinates(geometry)[,2])
  
  # calculate for each entry if it is during the daytime or nighttime based on the tod + sunrise/sunset
  temp_tod <- temp_coords %>% 
    select(c(date, lat, lon)) %>% 
    getSunlightTimes(data = ., tz = 'America/Toronto', keep = c('sunrise', 'sunset')) %>% 
    select(c(sunrise, sunset)) %>% 
    cbind(temp_coords) %>%
    mutate(tod = case_when(date_time >= sunrise & date_time <= sunset ~ 'day',
                           date_time < sunrise | date_time > sunset ~ 'night')) %>% 
    select(-c(sunrise, sunset, date, lat, lon))

}
