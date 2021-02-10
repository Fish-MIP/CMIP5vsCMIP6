
# Multimodel analyses
library(ncdf4)
library(zoo)

# get_model_type ----

get_model_type <- function(filename)
{
  model_type = substr(strsplit(filename, "/")[[1]][length(strsplit(filename, "/")[[1]])],1,4)
  model_list = list(APECOSM = FALSE, BOATS = FALSE, DBEM = FALSE, DBPM = FALSE, ECOOCEAN = FALSE, MACROECOLOGICAL = FALSE, SSDBEM = FALSE, FEISTY = FALSE, ZOOMSS = FALSE, ECOTROPH = FALSE, future = FALSE)
  
  if (model_type == "apec") {  
    model_list$APECOSM = TRUE
  } else if (model_type == "boat") {
    model_list$BOATS = TRUE
  } else if (model_type == "dbem") {
    model_list$DBEM = TRUE
  } else if (model_type == "dbpm") {
    model_list$DBPM = TRUE
  } else if (model_type == "ecoo") {
    model_list$ECOOCEAN = TRUE
  } else if (model_type == "macr") {
    model_list$MACROECOLOGICAL = TRUE
  } else if (model_type == "ecot") {
    model_list$ECOTROPH = TRUE
  } else if (model_type == "feis") {
    model_list$FEISTY = TRUE
  } else if (model_type == "zoom") {
    model_list$ZOOMSS = TRUE
  } else {
    # TEMPORARY TO HAVE SSDBEM HERE WHILE FILENAME IS NOT CORRECT
    model_list$SSDBEM =TRUE
    print("Filename does not indicate marine ecosystem model type. Have you change netcdf filename?")
  }
  
    if(any(grep("2005", filename)) || any(grep("2014", filename)) == TRUE){
      model_list$future = FALSE
    }else {
      model_list$future = TRUE}

  return(model_list)
}

# convert_model_dates ----

# Convert model dates to standard format. Note that for annual data we use January of each year as a marker.
# CN dates below refer to the time specification in the netcdf e.g. units: years since 1860-1-1 00:00:00. they can be the same for the fut and hist files or they can change. 

convert_model_dates <- function(netcdf_file, model_type)
{
  
  # trial 
  # netcdf_file = nc
  # model_type = model_type
  
  time_months <- ncvar_get(netcdf_file, "time")
  length(time_months)
  
  # Convert time to a sensible set of values (for our purposes)
  if (model_type$ECOOCEAN)
  {
    if (model_type$future == FALSE)
    {
      yearmonth = as.yearmon("1971-01") + time_months[1] / 12
    } else
    {
      yearmonth = as.yearmon("2006-01") + time_months[1] / 12
    }
    print(yearmonth)
  } else if (model_type$APECOSM)
  {
    yearmonth = as.yearmon("1900-01") + time_months[1] / 12
    print(yearmonth) 
  } else if (model_type$DBPM)
  {
    yearmonth = as.yearmon("1950-01") + time_months[1] / 12
    print(yearmonth) 
  } else if (model_type$DBEM) 
  {
    yearmonth = as.yearmon("1951-01") + time_months[1]
    print(yearmonth)      
  } else if (model_type$MACROECOLOGICAL)
    {
    if (model_type$future == FALSE)
    {
      yearmonth = as.yearmon("1900-01") + time_months[1]
    } else
    {
      if(any(grep("ipsl-cm5a-lr_nobc_rcp8p5", filename)) == TRUE){
        yearmonth = as.yearmon("1860-01") + time_months[1] # or 1860!? only for ipsl
      }else {
        yearmonth = as.yearmon("1900-01") + time_months[1]}
    }
    print(yearmonth)
  }else if (model_type$ZOOMSS)
    {
    if (model_type$future == FALSE){
      yearmonth = as.yearmon("1949-01") + 1
    } else{
      yearmonth = as.yearmon("2005-01") + 1
    } 
  } else if (model_type$SSDBEM) 
  {
    yearmonth = as.yearmon("1950-01") + time_months[1]
    print(yearmonth) 
  } else
  {
     yearmonth = as.yearmon("1901-01") + time_months[1] / 12
     print(yearmonth)
  }

  # Convert to months or years
  if (model_type$DBEM || model_type$MACROECOLOGICAL || model_type$SSDBEM || model_type$ZOOMSS)
  {
    time_months = floor(as.numeric(as.yearmon(yearmonth + seq(0, (length(time_months) - 1)))))
  } else
  {
    time_months = as.yearmon(yearmonth + seq(0, (length(time_months) - 1)) / 12)      
  }
  print(time_months[1])
  print(time_months[length(time_months)])
  
  return(time_months)
}

# convert_model_dates_CMIP6 ----

# CN same as above but given CMIP6 variations 

convert_model_dates_CMIP6 <- function(netcdf_file, model_type)
{
  
  # trial 
  # netcdf_file = nc
  # model_type = model_type
  
  time_months <- ncvar_get(netcdf_file, "time")
  
  # CMIP 6.....
  print(netcdf_file$dim$time$units)
  
  # Convert time to a sensible set of values (for our purposes)
  if (model_type$BOATS){
    yearmonth = as.yearmon("1950-01") + time_months[1] / 12
    print(yearmonth)
  } else if (model_type$APECOSM) # to fix  when data available 
  {
    yearmonth = as.yearmon("1601-01") + time_months[1] / 12
    print(yearmonth) 
  } else if (model_type$DBPM)
  {
    # time is not properly given in this netcdf (it starts from 1)
    # but all good if you do the below
    if(model_type$future == FALSE){
      yearmonth = as.yearmon("1850-01") + 0.08333333
      print(yearmonth) 
    }else {
      yearmonth = as.yearmon("2015-01") + 0.08333333
      print(yearmonth) 
    }
  } else if (model_type$DBEM) # to fix when data available 
  {
    yearmonth = as.yearmon("1951-01") + time_months[1]
    print(yearmonth)      
  } else if (model_type$MACROECOLOGICAL)
  {
    if (model_type$future == FALSE)
    {
      yearmonth = as.yearmon("1950-01") + 1
    } else
    {
      yearmonth = as.yearmon("2015-01") + 1
    }
    print(yearmonth)
  } else if (model_type$ZOOMSS) # need to fix..... 
  {
    if (model_type$future == FALSE)
    {
      yearmonth = as.yearmon("1950-01") + 1
    } else
    {
      yearmonth = as.yearmon("2015-01") + 1
    }
    print(yearmonth)
  } else if (model_type$SSDBEM) # not needed 
  {
    yearmonth = as.yearmon("1950-01") + time_months[1]
    print(yearmonth) 
  }else if (model_type$ECOTROPH) # 
  {
    yearmonth = as.yearmon("1950-01")
    print(yearmonth) 
  } else
  {
    yearmonth = as.yearmon("1601-01") + time_months[1] / 12
    print(yearmonth)
  }
  
  # Convert to months or years
  if (model_type$DBEM || model_type$MACROECOLOGICAL || model_type$SSDBEM || model_type$ZOOMSS || model_type$ECOTROPH) # these are the  annual model 
  {
    time_months = floor(as.numeric(as.yearmon(yearmonth + seq(0, (length(time_months) - 1)))))
  } else
  {
    time_months = as.yearmon(yearmonth + seq(0, (length(time_months) - 1)) / 12)      
  }
  print(time_months[1])
  print(time_months[length(time_months)])
  
  return(time_months)
}

# time_period_to_extract ----

time_period_to_extract <- function(time1, time2, time_vector)
{

  # trial 
  # time_period_to_extract(time1, time2, time_vector)
  
  # Extract the correct time period
  if (((class(time1) == "yearmon") && (class(time2) == "yearmon")) || ((class(time1) == "numeric") && (class(time2) == "numeric")))
  {
    start_point_to_extract = which(time1 == time_vector)
    end_point_to_extract = which(time2 == time_vector)
  } else
  {
    print("Inconsistent class of the two yearmonth parameters: they should both either be class yearmon or numeric")
  }
  
  if ((length(start_point_to_extract) == 0) || (length(end_point_to_extract) == 0))
  {
    print("Start or end month / year not found. Please check them")
    print(start_point_to_extract)
    print(end_point_to_extract)
  }
  
  return(list(start_point_to_extract = start_point_to_extract, end_point_to_extract = end_point_to_extract))
}

# can_model_units_be_standardized ----

# Test whether model units can be standardized
can_model_units_be_standardized <- function(data_units, convert_to_kg_km)
{    
  
  print(data_units)
  can_be_converted = FALSE
  
  if (length(data_units) > 0)
  {
    if (convert_to_kg_km)
    {
      if ((data_units == "gC m-2") ||
          (data_units == "g m-2 month-1") || (data_units == "g C m-2") ||
          (data_units == "gC.m-2") || (data_units == "g C / m^2")
          # # CN Added for CMIP6 - EcoThrop is the one in C - need to check  
          || (data_units == "g m-2") || (data_units == "g/m^2") || (data_units == "g m^-2") || (data_units == "grams wet weight m-2") || (data_units == "gC/m^2"))  
      {
        can_be_converted = TRUE
      } else
      {
        print("Unknown units: Cannot convert data units to kg C km-2")
      }
    }
  } else
  {
    print("Unknown units: Cannot convert data units to kg C km-2")
  }
  
  return(can_be_converted)
}

# averageFishCDF ----

# Extracts data for a particular period from a FISH-MIP netcdf. time1 and time2 have to be 'yearmon' variables (see "zoo" package).
# Assumes yearly averaging unless average_whole_period is set to true. When yearly averaging, times have to begin in Jan and end in Dec 
# (i.e. can only average actually yearly periods)

averageFishCDF <- function(directory,
           filename,
           variable,
           time1,
           time2,
           average_whole_period = FALSE,
           convert_to_kg_km = TRUE, 
           CMIP = 5)
  {
    
  
    # # trial
    # variable = variable_to_extract[1]
    # average_whole_period = FALSE
    # time1 = yearmonth1[i]
    # time2 = yearmonth2[i]
    # convert_to_kg_km = TRUE # THIS NEEDS TO BE CHECKED FOR CMIP6!!!!!
    # CMIP = 6
    
    model_type <- get_model_type(filename)
    
    # Open the netcdf file
    nc <- nc_open(paste(directory, filename, sep = ""))

    # Look at the attributes and dimensions
    # print(nc)
    
    # Extract important information
    data_attributes <- ncatt_get(nc, variable)
    
    main_title <- data_attributes$long_field_name
    data_units <- data_attributes$units
    
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    
    # NEED IF STATEMENT HERE - CMIP5 or 6 
    if (CMIP==5){
      time_vector <- convert_model_dates(nc, model_type) 
    }else{
      time_vector <- convert_model_dates_CMIP6(nc, model_type) 
    }

    # Extract averages
    if (!average_whole_period){
      
      # CN if you are extracting all periods you can set this as starting and ending values and NA as function arguments
      if(is.na(time1)){
        time1<-time_vector[1]
        time2<-time_vector[length(time_vector)] 
      }
      
      # Extract the correct time period
      if ((class(time1) == "yearmon") && (class(time2) == "yearmon")){
        # Check to make sure starts in January and ends in Dec. Doesn't do stupidity checks (e.g. end date before start date)
        if ((as.numeric(time1) == as.numeric(floor(time1))) && ((as.numeric(time2) - floor(as.numeric(time2))) > 0.9)){ 
          ; } else {
          print("When taking yearly averages of monthly data, dates given should start in Jan and end in Dec")
        }
      }
      
      # Get the correct time period to extract
      tpte = time_period_to_extract(time1, time2, time_vector)
      
      # Calculate number of years
      num_years = (floor(as.numeric(time2)) - floor(as.numeric(time1)) + 1)
      
      # Create a new matrix to hold the results
      new_var_mat = array(rep(0, num_years * length(lon) * length(lat)),
                          c(length(lon), length(lat), num_years))
      
      # Now get the variable of interest
      if (names(nc$dim)[length(nc$dim)] != "time")
        print("Warning: might be assuming wrong dimension for time from NetCDF file")
      ta1 <- ncvar_get(nc, variable)[, , ]
      temp_array1 <- ncvar_get(nc, variable)[, , tpte$start_point_to_extract:tpte$end_point_to_extract]
       
        # Average data
        for (ii in 1:length(lon))
        {
          for (jj in 1:length(lat))
          {
            # Extract values per grid cell
            temp_vec = temp_array1[ii, jj, ]
            
            if ((class(time1) == "yearmon") && (class(time2) == "yearmon"))
            {
              # Extract yearly averages
              year_values = vector(length = num_years)
              for (hh in 1:num_years)
              {
                year_values[hh] = mean(temp_vec[((hh - 1) * 12 + 1):(hh * 12)])
              }
            } else
            {
              year_values = temp_vec
            }
            
            # Fill in 3D matrix of annual averages
            new_var_mat[ii, jj, ] = year_values
          }
        }
        
        years = floor(as.numeric(time1)):floor(as.numeric(time2))
        
        
    } else # opposite of averaging whole period?
    {
      # Create a new matrix to hold the results
      new_var_mat = array(rep(0, length(lon) * length(lat)), c(length(lon), length(lat)))
      
      # Get the correct time period to extract
      tpte = time_period_to_extract(time1, time2, time_vector)

      # Now get the variable of interest
      if (names(nc$dim)[length(nc$dim)] != "time")
        print("Warning: might be assuming wrong dimension for time from NetCDF file")
      temp_array1 <- ncvar_get(nc, variable)[, , tpte$start_point_to_extract:tpte$end_point_to_extract]

      # Extract averages
      for (ii in 1:length(lon))
      {
        for (jj in 1:length(lat))
        {
          # Fill in 2D matrix of whole time-period averages
          new_var_mat[ii, jj] = mean(temp_array1[ii, jj, ])
        }
      }
      
      years = paste(as.character(time1), "to", as.character(time2), sep=" ")
    }

    if(can_model_units_be_standardized(data_units, convert_to_kg_km))
    {
      new_var_mat = new_var_mat * 1000
      
      # CN : also why is this g to kg and m to km?  
      if(CMIP==5){
        data_units = "kg C km-2" 
      } else{
        data_units = "kg km-2" 
      }
      
    }
    
    return_list <-
      list(
        main_title = main_title,
        data_units = data_units,
        lon = lon,
        lat = lat,
        years = years,
        fishvar = new_var_mat
      )
  }

# extractFishCDF ----

# CN trial as above but tailored to time series calculation. first you extract all data and then you calculate the time series you need using get_timeSeries() below  

extractFishCDF <- function(directory,
                           filename,
                           variable,
                           time1,
                           time2,
                           average_whole_period = FALSE,
                           convert_to_kg_km = TRUE, 
                           CMIP = 5)
{
  
  
  # # trial
  # variable = "Band1"
  # average_whole_period = FALSE
  # time1 = yearmonth1[2]
  # time2 = yearmonth2[2]
  # convert_to_kg_km = TRUE
  # CMIP = 6
  
  model_type <- get_model_type(filename)
  
  # Open the netcdf file
  nc <- nc_open(paste(directory, filename, sep = ""))
  
  # Extract important information
  data_attributes <- ncatt_get(nc, variable)
  
  main_title <- data_attributes$long_field_name
  data_units <- data_attributes$units
  
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  
  if (CMIP==5){
    time_vector <- convert_model_dates(nc, model_type) 
  }else{
    time_vector <- convert_model_dates_CMIP6(nc, model_type) 
  }
  
  # extract all 
  new_var_mat <- ncvar_get(nc, variable)
  years = time_vector
  
  if(can_model_units_be_standardized(data_units, convert_to_kg_km))
  {
    new_var_mat = new_var_mat * 1000
    
    # CN : also why is this g to kg and m to km?  
    if(CMIP==5){
      data_units = "kg C km-2" 
    } else{
      data_units = "kg km-2" 
    }
    
  }
  
  return_list <-
    list(
      main_title = main_title,
      data_units = data_units,
      lon = lon,
      lat = lat,
      years = years,
      fishvar = new_var_mat
    )
}

# get_timeSeries ---- 

get_timeSeries<-function(hist, fut126, fut585){
  
  # hist$fishvar
  # output[i] == "annula"
  
  dimnames(hist$fishvar)<-list(hist$lon, hist$lat, hist$years)
  dim(hist$fishvar)
  if(output[i] == "monthly"){
    hist$fishvar<-hist$fishvar[,,which(hist$years >= "Jan 1971")] # consider only what's needed 
    }else{
    hist$fishvar<-hist$fishvar[,,which(hist$years >= "1971")] 
  }
  hist_2<-as.data.frame.table(hist$fishvar) %>%
    `colnames<-`(c("lon", "lat", "year1", "tcb")) %>% 
    # weight by grid area (smaller at higher lats)
    mutate(lat = as.numeric(as.character(lat))) %>% 
    mutate(lat2 = cos(abs(lat) * (pi/180))) %>% # transom lat in radiant and calculate cosine. These values are not 0-1 because lats are not 0-90 but 0.5-89.5 (midpoint of the grid cell)
    mutate(tcb = tcb * lat2) %>% 
    mutate(year = round(as.numeric(as.character(year1)))) %>%
    group_by(year) %>% 
    dplyr::summarise(tcb = mean(tcb, na.rm=TRUE))
  
  # ref decade
  refDecade <- hist_2 %>% 
    filter(year >= 1990, year <=2000) %>% 
    dplyr::summarize(value = mean(tcb, na.rm = TRUE))
  
  # ssp126
  dimnames(fut126$fishvar)<-list(fut126$lon, fut126$lat, fut126$years)
  if(output[i] == "monthly"){
    fut126$fishvar<-fut126$fishvar[,,which(fut126$years <= "Jan 2099")]
  }else{
    fut126$fishvar<-fut126$fishvar[,,which(fut126$years <= "2099")]
  }
  fut126_2<-as.data.frame.table(fut126$fishvar) %>% 
    `colnames<-`(c("lon", "lat", "year1", "tcb")) %>% 
    mutate(lat = as.numeric(as.character(lat))) %>% 
    mutate(lat2 = cos(abs(lat) * (pi/180))) %>% 
    mutate(tcb = tcb * lat2) %>% 
    mutate(year = round(as.numeric(as.character(year1)))) %>%
    group_by(year) %>% 
    dplyr::summarise(tcb = mean(tcb, na.rm=TRUE))
  
  # projections start at different times for the 2 cmips 
  if(cmip == 5){
    all126<-hist_2 %>% 
      full_join(fut126_2) %>% 
      mutate(TcbChange = (tcb - refDecade$value)/refDecade$value * 100) %>% 
      mutate(color = ifelse(year <= 2005, "hist", "ssp126"))
  }else{
    all126<-hist_2 %>% 
      full_join(fut126_2) %>% 
      mutate(TcbChange = (tcb - refDecade$value)/refDecade$value * 100) %>% 
      mutate(color = ifelse(year <= 2015, "hist", "ssp126"))
  }
  
  # ssp585
  dimnames(fut585$fishvar)<-list(fut585$lon, fut585$lat, fut585$years)
  if(output[i] == "monthly"){
    fut585$fishvar<-fut585$fishvar[,,which(fut585$years <= "Jan 2099")]
  }else{
    fut585$fishvar<-fut585$fishvar[,,which(fut585$years <= "2099")]
  }
  fut585_2<-as.data.frame.table(fut585$fishvar) %>%
    `colnames<-`(c("lon", "lat", "year1", "tcb")) %>% 
    mutate(lat = as.numeric(as.character(lat))) %>% 
    mutate(lat2 = cos(abs(lat) * (pi/180))) %>% 
    mutate(tcb = tcb * lat2) %>% 
    mutate(year = round(as.numeric(as.character(year1)))) %>%
    group_by(year) %>% 
    dplyr::summarise(tcb = mean(tcb, na.rm=TRUE)) %>% 
    mutate(TcbChange = (tcb - refDecade$value)/refDecade$value * 100) %>%
    mutate(color = "ssp585")
  
  all<-all126 %>%
    full_join(fut585_2)
  
  rm(hist_2, refDecade, fut126_2, all126, fut585_2)
  return(all)
  
}




# averageEnvironmentalCDF ----

# Extracts data for a particular period from a particular environmental netcdf. time1 and time2 have to be 'yearmon' variables (see "zoo" package).
# Assumes yearly averaging unless average_whole_period is set to true. When yearly averaging, times have to begin in Jan and end in Dec 
# (i.e. can only average actualy yearly periods)

averageEnvironmentalCDF <-
  function(directory,
           filename,
           variable,
           time1,
           time2,
           average_whole_period = FALSE,
           convert_to_kg_km = TRUE, hist = TRUE, ipsl = FALSE, cmip = 5)
  {
    
    # directory = "/Users/camillan/fishmip_inputs/marine-fishery_global_ISIMIP2b/GFDL_ESM2M/"
    # filename = "spp_gfdl-esm2m_rcp26_zs_annual_200601-210012.nc4"
    # variable = "TO_ZS"
    # time1 = as.yearmon("1990-01")
    # time2 = as.yearmon("1999-01")
    # average_whole_period = TRUE
    # convert_to_kg_km = FALSE # I don't understand this ....
    # hist = TRUE
    # ipsl = FALSE
    # cmip = 5
    
    
    # model_type <- get_model_type(filename) # I don't understand this as it's not an ecosystem model but an earth model ... 
    
    # Open the netcdf file
    nc <- nc_open(paste0(directory, filename))
    
    # Look at the attributes and dimensions
    # print(nc)
    # names(nc$var)
    
    # Extract important information
    data_attributes <- ncatt_get(nc, variable)
    
    main_title <- data_attributes$long_field_name
    data_units <- data_attributes$units
    
    if(cmip == 5){
      if(!ipsl){
        lon <- ncvar_get(nc, "LONGITUDE")
        lat <- ncvar_get(nc, "LATITUDE")
        time_vector <-  ncvar_get(nc, "TIME")
      }else{
        lon <- ncvar_get(nc, "longitude")
        lat <- ncvar_get(nc, "latitude")
        time_vector <-  ncvar_get(nc, "time")} 
    }else{
      lon <- ncvar_get(nc, "lon")
      lat <- ncvar_get(nc, "lat")
      time_vector <- ncvar_get(nc, "time")}
    

    if(cmip == 5){
      if (hist)
      {
        yearmonth = as.yearmon("1950-01")
      } else
      {
        yearmonth = as.yearmon("2006-01")
      }
      time_months = floor(as.numeric(as.yearmon(yearmonth + seq(0, (length(time_vector) - 1)))))
      time_vector <- time_months
    }else{
      if(hist == TRUE){
        yearmonth = as.yearmon("1850-1-1") + 0.08333333 # is this 0.0833 necessary? should we just start extracting from Jan? 
      }else{
        yearmonth = as.yearmon("2015-1-1") + 0.08333333
      }
      time_months = as.yearmon(yearmonth + seq(0, (length(time_vector) - 1)) / 12)
      time_vector <- time_months
    }
    
    # Extract averages
    if (!average_whole_period){
      # Extract the correct time period
      if ((class(time1) == "yearmon") && (class(time2) == "yearmon")){
        # Check to make sure starts in January and ends in Dec. Doesn't do stupidity checks (e.g. end date before start date)
        if ((as.numeric(time1) == as.numeric(floor(time1))) && ((as.numeric(time2) - floor(as.numeric(time2))) > 0.9))
        { ; }else{
          print("When taking yearly averages of monthly data, dates given should start in Jan and end in Dec")
        }
      }
      
      # Get the correct time period to extract
      tpte = time_period_to_extract(time1, time2, time_vector)
      
      # Calculate number of years
      num_years = (floor(as.numeric(time2)) - floor(as.numeric(time1)) + 1)
      
      # Create a new matrix to hold the results
      new_var_mat = array(rep(0, num_years * length(lon) * length(lat)),
                          c(length(lon), length(lat), num_years))
      
      # Now get the variable of interest
      if (names(nc$dim)[length(nc$dim)] != "TIME")
        print("Warning: might be assuming wrong dimension for time from NetCDF file") # CN don't understand this!
      temp_array1 <- ncvar_get(nc, variable)[, , tpte$start_point_to_extract:tpte$end_point_to_extract]
  
      if(num_years>1){
      # Average data
      for (ii in 1:length(lon))
      {
        for (jj in 1:length(lat))
        {
          # Extract values per grid cell
          temp_vec = temp_array1[ii, jj, ]
          
          if ((class(time1) == "yearmon") && (class(time2) == "yearmon"))
          {
            # Extract yearly averages
            year_values = vector(length = num_years)
            for (hh in 1:num_years)
            {
              year_values[hh] = mean(temp_vec[((hh - 1) * 12 + 1):(hh * 12)])
            }
          } else
          {
            year_values = temp_vec
          }
          
          # Fill in 3D matrix of annual averages
          new_var_mat[ii, jj, ] = year_values
        }
      }
      } else {
        new_var_mat = temp_array1
        }
      
      years = floor(as.numeric(time1)):floor(as.numeric(time2))
      
    } else # CN average_whole_period == TRUE
    {
      # Create a new matrix to hold the results
      new_var_mat = array(rep(0, length(lon) * length(lat)), c(length(lon), length(lat)))
      
      # Get the correct time period to extract
      tpte = time_period_to_extract(time1, time2, time_vector)
      
      # Now get the variable of interest
      if (names(nc$dim)[length(nc$dim)] != "time")
        print("Warning: might be assuming wrong dimension for time from NetCDF file")
      # temp_array1 <- ncvar_get(nc, variable)
      # dim(temp_array1)
      temp_array1 <- ncvar_get(nc, variable)[, , tpte$start_point_to_extract:tpte$end_point_to_extract]

      # Extract averages
      for (ii in 1:length(lon))
      {
        for (jj in 1:length(lat))
        {
          # Fill in 2D matrix of whole time-period averages
          new_var_mat[ii, jj] = mean(temp_array1[ii, jj, ])
        }
      }
      
      years = paste(as.character(time1), "to", as.character(time2), sep=" ")
    }
    
    # CN don't understand this - maybe for phy, zoo and PP?
    # if(can_model_units_be_standardized(data_units, convert_to_kg_km))
    # {
    #   new_var_mat = new_var_mat * 1000
    #   data_units = "kg C km-2"
    # }
    
    return_list <-
      list(
        main_title = main_title,
        data_units = data_units,
        lon = lon,
        lat = lat,
        years = years,
        fishvar = new_var_mat
      )
  }

# get_mean_change ----

get_mean_change<-function(var2000s, var1970s)
{
  # Get average change by ocean
  change = vector(length = length(uniquelocs))
  for (zz in 1:length(uniquelocs))
  {
    temp_sum_2000s = 0
    temp_sum_1970s = 0
    
    for (ii in 1:360)
    {
      for(jj in 1:180)
      {
        if(!is.na(histcomparison$oceanlocs[ii,jj]))
        {
          if (strcmp(histcomparison$oceanlocs[ii,jj],uniquelocs[zz]))
          {
            if ((!is.na(var2000s$fishvar[ii,jj])) && (!is.na(var1970s$fishvar[ii,jj])))
            {
              temp_sum_1970s = temp_sum_1970s + var1970s$fishvar[ii,jj]
              temp_sum_2000s = temp_sum_2000s + var2000s$fishvar[ii,jj]
            }
          }
        }
      }
    }
    
    change[zz] = temp_sum_2000s / temp_sum_1970s * 100 - 100
  }
  
  differencevals = vector(length = dim(stockchanges)[1])
  for (ii in 1:dim(stockchanges)[1])
  {
    val = which(uniquelocs == as.character(stockchanges[ii,2]))
    differencevals[ii] = change[val]
  }
  return(differencevals)
}