clean_mobile <- function(mobile_raw){
  
  # code by Johanna Arnet 
  
  data <- mobile_raw[-c(1,2), ] #get rid of rows with unimportant values
  
  #convert timestamp data
  data$TIMESTAMP <- as.POSIXct(data$TIMESTAMP, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  
  #subset data for only the day or route in question
  data_dates <- data %>% 
    filter(TIMESTAMP >= '2024-06-12 08:00:00' & TIMESTAMP <= '2024-06-14 23:00:00')
  
  #### MAKE SUBSET WITH ONLY NMEA DATA AND TEMPERATURE ####
  
  subset_data <- data_dates[c(1:6)]
  
  #parse NMEA data 
  subset_data_new <- subset_data %>% 
    separate(NMEASent.1., 
             c('ID', 'utc_time', 'status', 'lat (dmm)', 'lat dir', 'long (dmm)', 'long dir', 'speed_knots', 'course_degrees', 'utc_date', 'mag_degrees', 'mag_dir'), 
             sep=",")
  
  #get rid of rows with invalid GPS data
  subset_data_new <-subset(subset_data_new, subset_data_new$status=="A")
  
  # subset to have the lat and long (these are in degrees and decimal minutes DMM)
  subset_data_new$lat_degrees <- left(subset_data_new$`lat (dmm)`,n=2)
  subset_data_new$lat_decimal_minutes <-right(subset_data_new$`lat (dmm)`,n=7)
  
  subset_data_new$long_degrees <- left(subset_data_new$`long (dmm)`,n=3)
  subset_data_new$long_decimal_minutes <-right(subset_data_new$`long (dmm)`,n=7)
  
  #convert decimal degrees to DD and add back in
  
  subset_data_new$lat_degrees <- as.numeric(subset_data_new$lat_degrees)
  subset_data_new$long_degrees <- as.numeric(subset_data_new$long_degrees)
  subset_data_new$lat_decimal_minutes <- as.numeric(subset_data_new$lat_decimal_minutes)
  subset_data_new$long_decimal_minutes <- as.numeric(subset_data_new$long_decimal_minutes)
  
  subset_data_new$lat_dd <- subset_data_new$lat_decimal_minutes/60
  subset_data_new$long_dd <- subset_data_new$long_decimal_minutes/60
  
  subset_data_new$LATITUDE <- subset_data_new$lat_degrees +subset_data_new$lat_dd
  subset_data_new$LONGITUDE <- subset_data_new$long_degrees +subset_data_new$long_dd
  subset_data_new$LONGITUDE <- (subset_data_new$LONGITUDE)*(-1)
  
  
  subset_data_new$LATITUDE <- as.numeric(subset_data_new$LATITUDE)
  subset_data_new$LONGITUDE <- as.numeric(subset_data_new$LONGITUDE)
  subset_data_new$T109_C <- as.numeric(subset_data_new$T109_C)
  
  subset_data_new <- na.omit(subset_data_new) #omit the rows where data is NA
  
  
  return(subset_data_new)
  
}