
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
# Dates below refer to the time dimension and reference in the netcdf e.g. units: years since 1860-1-1 00:00:00. 
# the reference can be the same for the historical and future files or it can change. 

convert_model_dates <- function(netcdf_file, model_type)
{
  
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
    } else # THIS IS BOATS
    {
      if(any(grep("ipsl-cm5a-lr_nobc_rcp8p5", filename)) == TRUE){
        yearmonth = as.yearmon("1860-01") + time_months[1] 
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
  
  time_months <- ncvar_get(netcdf_file, "time")
  print(netcdf_file$dim$time$units)
  
  # Convert time to a sensible set of values (for our purposes)
  if (model_type$BOATS){
    # this means that the time dimension starts at 1 which is jan 1950 for hist 
    # and at 781 which are months between 1950 and 2015 for future 
    # yearmonth = as.yearmon("1950-01") + time_months[1] / 12 - 0.08333333
    # print(yearmonth)
    
    # new file formatting as per OutputData folder 
    
    # this means that the time dimension starts at 4188 which is 4188 between 
    # jan 1601 (the convention) and jan 1850 (when the model actually starts) 
    # the number of months are different for the future protocols and that's 
    # given by time_months[1]
    yearmonth = as.yearmon("1601-01") + time_months[1] / 12
    # if (model_type$future == FALSE){
    #   yearmonth = as.yearmon("1950-01") 
    # }else{
    #   yearmonth = as.yearmon("2015-01") 
    # }
    print(yearmonth) 
    
  } else if (model_type$APECOSM)  
  {
    # this means that the time dimension starts at 4188 which is 4188 between 
    # jan 1601 (the convention) and jan 1850 (when the model actually starts) 
    # the number of months are different for the future protocols and that's 
    # given by time_months[1]
    # yearmonth = as.yearmon("1601-01") + time_months[1] / 12 
    # new version - NICOLAS:
    
    if (model_type$future == FALSE){
      yearmonth = as.yearmon("1850-01") # the above works too as it gives Jan 1850
    }else{
      yearmonth = as.yearmon("2015-01") 
    }
    print(yearmonth) 
  } else if (model_type$DBPM)
  {
    # see APECOSM
    yearmonth = as.yearmon("1601-01") + time_months[1] / 12 
    print(yearmonth) 
  } else if (model_type$DBEM) # to fix when data available 
  {
    yearmonth = as.yearmon("1951-01") + time_months[1]
    print(yearmonth)      
  } else if (model_type$MACROECOLOGICAL)
  {
    if (model_type$future == FALSE)
    {
      yearmonth = as.yearmon("1950-01")
    } else
    {
      yearmonth = as.yearmon("2015-01")
    }
    print(yearmonth)
  } else if (model_type$ZOOMSS) 
  {
    if (model_type$future == FALSE)
    {
      yearmonth = as.yearmon("1950-01") 
    } else
    {
      yearmonth = as.yearmon("2015-01")
    }
    print(yearmonth)
  } else if (model_type$SSDBEM) # not needed 
  {
    yearmonth = as.yearmon("1950-01") + time_months[1]
    print(yearmonth) 
  }else if (model_type$ECOTROPH)  
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
          (data_units == "g m-2 month-1") || 
          (data_units == "g C m-2") ||
          (data_units == "gC.m-2") || 
          (data_units == "g C / m^2")
          # # CN Added for CMIP6 
          || (data_units == "g m-2") || 
          (data_units == "g/m^2") || 
          (data_units == "g m^-2") || 
          (data_units == "grams wet weight m-2") || 
          (data_units == "gC/m^2")) # EcoThrop in C? - need to check    
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

averageFishCDF <- function(directory,
           filename,
           variable,
           time1,
           time2,
           average_whole_period = FALSE,
           convert_to_kg_km = TRUE, 
           CMIP = 5)
  {
    
  
  # # # trial
  # variable = variable_to_extract[1]
  # time1 = yearmonth1[i]
  # time2 = yearmonth2[i]
  # average_whole_period = TRUE
  # convert_to_kg_km = TRUE
  # CMIP =  6
  
    model_type <- get_model_type(filename)
    
    # Open the netcdf file
    nc <- nc_open(paste(directory, filename, sep = ""))
    
    # Extract important information
    data_attributes <- ncatt_get(nc, variable)
    
    main_title <- data_attributes$long_field_name
    data_units <- data_attributes$units
    
    if(CMIP == 6 & model_type$APECOSM == TRUE){
      data_units = "g m-2"
    }
    
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    
    # CMIP 5 or 6 
    if (CMIP==5){
      time_vector <- convert_model_dates(nc, model_type) 
    }else{
      time_vector <- convert_model_dates_CMIP6(nc, model_type) 
    }

    # Extract averages
    if (!average_whole_period){
      
      # CN if you are extracting all periods you can set this as starting 
      # and ending values and NA as function arguments
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
      
      # # check time needs to be the 3rd dimension ...
      # temp_array1 <- ncvar_get(nc, variable)
      # dim(temp_array1)
      
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
        
        
    } else # NOT averaging whole period
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

# CN as above but tailored to time series calculation. 
# first we extract all data, then we calculate the time series using get_timeSeries() below  

extractFishCDF <- function(directory,
                           filename,
                           variable,
                           time1,
                           time2,
                           average_whole_period = FALSE,
                           convert_to_kg_km = TRUE, 
                           CMIP = 5)
{
  
  model_type <- get_model_type(filename)
  
  # Open the netcdf file
  nc <- nc_open(paste(directory, filename, sep = ""))
  
  # Extract important information
  data_attributes <- ncatt_get(nc, variable)
  
  main_title <- data_attributes$long_field_name
  data_units <- data_attributes$units
  
  # data units for APECOSM is NULL - checked with Nicolas 
  if(CMIP == 6 & model_type$APECOSM == TRUE){
    data_units = "g m-2"
  }
  
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  
  if (CMIP==5){
    time_vector <- convert_model_dates(nc, model_type) 
  }else{
    time_vector <- convert_model_dates_CMIP6(nc, model_type) 
  }
  
  # extract all 
  new_var_mat <- ncvar_get(nc, variable)
  # dim(new_var_mat)
  
  years = time_vector
  
  if(can_model_units_be_standardized(data_units, convert_to_kg_km))
  {
    new_var_mat = new_var_mat * 1000
    
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

get_timeSeries<-function(hist, fut126, fut585, output = "yearly"){
  
  dimnames(hist$fishvar)<-list(hist$lon, hist$lat, hist$years)
  if(output == "monthly"){
    hist$fishvar<-hist$fishvar[,,which(hist$years >= "Jan 1971")] # REVISION: changed from 1971 to 1970 (or keep 1971?) 
    }else{
    hist$fishvar<-hist$fishvar[,,which(hist$years >= "1971")] # REVISION: changed from 1971
  }
  
  # # OPTION 1 
  # # hist
  # hist_2<-as.data.frame.table(hist$fishvar) %>%
  #   `colnames<-`(c("lon", "lat", "year1", "tcb")) %>% 
  #   mutate(year = as.numeric(as.character(gsub("\\..*", "", year1))))
  # 
  # # ref decade
  # refDecade <- hist_2 %>% 
  #   filter(year >= 1990, year <=2000) %>%
  #   # filter(year >= 1995, year <= 2014) %>% # prepare data and fig for IPCC report: Laurent Bopp ref year 1995-2014
  #   group_by(lon, lat) %>% 
  #   dplyr::summarize(refValue = mean(tcb, na.rm = TRUE)) # CHECK na.rm makes difference (IT DOES!) and it is in temp below too
  # 
  # hist_2<-hist_2 %>% 
  #   full_join(refDecade)
  # 
  # # calculate change 
  # hist_2<-hist_2 %>%
  #   mutate(TcbChange = (tcb - refValue)/refValue * 100)
  # 
  # # weight by grid cell 
  # hist_2<-hist_2 %>% 
  #   # weight by grid area (smaller at higher lats)
  #   mutate(lat = as.numeric(as.character(lat))) %>% 
  #   mutate(lat2 = cos(abs(lat) * (pi/180))) # %>% # transform lat in radiant and calculate cosine. These values are not 0-1 because lats are not 0-90 but 0.5-89.5 (midpoint of the grid cell)
  #   
  # # mean weight over years 
  # hist_2<-hist_2 %>% 
  #   group_by(year) %>% 
  #   dplyr::summarise(TcbChange = weighted.mean(TcbChange, lat2, na.rm=TRUE))
  # 
  # hist_2<-hist_2 %>% 
  #   mutate(color = "hist")
  # 
  # filter(hist_2, year == 1971)
  # 
  # # ssp126
  # dimnames(fut126$fishvar)<-list(fut126$lon, fut126$lat, fut126$years)
  # if(output == "monthly"){
  #   fut126$fishvar<-fut126$fishvar[,,which(fut126$years <= "Dec 2099")] # REVISION: changed from Jan 2099 to Dec 2100 (or keep 2099 but Dec?)
  # }else{
  #   fut126$fishvar<-fut126$fishvar[,,which(fut126$years <= "2099")] # REVISION: changed from 2099 to 2100 (or keep 2099?)
  # }
  # 
  # fut126_2<-as.data.frame.table(fut126$fishvar) %>%
  #   `colnames<-`(c("lon", "lat", "year1", "tcb")) %>% 
  #   mutate(year = as.numeric(as.character(gsub("\\..*", "", year1))))
  # 
  # fut126_2<-fut126_2 %>% 
  #   full_join(refDecade)
  # 
  # # calculate change 
  # fut126_2<-fut126_2 %>%
  #   mutate(TcbChange = (tcb - refValue)/refValue * 100)
  # 
  # # weight by grid cell 
  # fut126_2<-fut126_2 %>% 
  #   # weight by grid area (smaller at higher lats)
  #   mutate(lat = as.numeric(as.character(lat))) %>% 
  #   mutate(lat2 = cos(abs(lat) * (pi/180))) # %>% # transform lat in radiant and calculate cosine. These values are not 0-1 because lats are not 0-90 but 0.5-89.5 (midpoint of the grid cell)
  # 
  # # mean weight over years 
  # fut126_2<-fut126_2 %>% 
  #   group_by(year) %>% 
  #   dplyr::summarise(TcbChange = weighted.mean(TcbChange, lat2, na.rm=TRUE))
  # 
  # fut126_2<-fut126_2 %>% 
  #   mutate(color = "ssp126")
  # 
  # filter(fut126_2, year == 2099)
  # 
  # # ssp585
  # dimnames(fut585$fishvar)<-list(fut585$lon, fut585$lat, fut585$years)
  # if(output == "monthly"){
  #   fut585$fishvar<-fut585$fishvar[,,which(fut585$years <= "Dec 2099")] # REVISION see above 
  # }else{
  #   fut585$fishvar<-fut585$fishvar[,,which(fut585$years <= "2099")] # REVISION see above
  # }
  # 
  # fut585_2<-as.data.frame.table(fut585$fishvar) %>%
  #   `colnames<-`(c("lon", "lat", "year1", "tcb")) %>% 
  #   mutate(year = as.numeric(as.character(gsub("\\..*", "", year1))))
  # 
  # fut585_2<-fut585_2 %>% 
  #   full_join(refDecade)
  # 
  # # calculate change 
  # fut585_2<-fut585_2 %>%
  #   mutate(TcbChange = (tcb - refValue)/refValue * 100)
  # 
  # # weight by grid cell 
  # fut585_2<-fut585_2 %>% 
  #   # weight by grid area (smaller at higher lats)
  #   mutate(lat = as.numeric(as.character(lat))) %>% 
  #   mutate(lat2 = cos(abs(lat) * (pi/180))) # %>% # transform lat in radiant and calculate cosine. These values are not 0-1 because lats are not 0-90 but 0.5-89.5 (midpoint of the grid cell)
  # 
  # # mean weight over years 
  # fut585_2<-fut585_2 %>% 
  #   group_by(year) %>% 
  #   dplyr::summarise(TcbChange = weighted.mean(TcbChange, lat2, na.rm=TRUE))
  # 
  # fut585_2<-fut585_2 %>% 
  #   mutate(color = "ssp585")
  # 
  # filter(fut585_2, year == 2099)
  # 
  # all<-hist_2 %>% 
  #   full_join(fut126_2) %>% 
  #   full_join(fut585_2)
  # 
  # rm(hist_2, refDecade, fut126_2, fut585_2)
  
  # OPTION 2 - see below for differences
  # A # mutate(tcb = tcb * lat2) %>% dplyr::summarise(tcb = mean(tcb, na.rm=TRUE))
  # B # dplyr::summarise(tcb = weighted.mean(tcb, lat2, na.rm=TRUE))
  
  hist_2<-as.data.frame.table(hist$fishvar) %>%
    `colnames<-`(c("lon", "lat", "year1", "tcb")) %>%
    # weight by grid area (smaller at higher lats)
    mutate(lat = as.numeric(as.character(lat))) %>%
    mutate(lat2 = cos(abs(lat) * (pi/180))) %>% # transform lat in radiant and calculate cosine. These values are not 0-1 because lats are not 0-90 but 0.5-89.5 (midpoint of the grid cell)
    # mutate(tcb = tcb * lat2) %>%
    # mutate(year = round(as.numeric(as.character(year1)))) %>% # NOTE Dec 2014 becomes 2015. That's how we end up with this year.
    mutate(year = as.numeric(as.character(gsub("\\..*", "", year1)))) %>% # REVISION: changed from above line
    group_by(year) %>%
    dplyr::summarise(tcb = weighted.mean(tcb, lat2, na.rm=TRUE)) # REVISION - weighed mean - NO
    # dplyr::summarise(tcb = mean(tcb, na.rm=TRUE))

  # ref decade
  refDecade <- hist_2 %>%
    filter(year >= 1990, year <=2000) %>%
    # filter(year >= 1995, year <= 2014) %>% # prepare data and fig for IPCC report: Laurent Bopp ref year 1995-2014
    dplyr::summarize(value = mean(tcb, na.rm = TRUE))

  # ssp126
  dimnames(fut126$fishvar)<-list(fut126$lon, fut126$lat, fut126$years)
  if(output == "monthly"){
    fut126$fishvar<-fut126$fishvar[,,which(fut126$years <= "Dec 2099")] # REVISION: changed from Jan 2099 to Dec 2100 (or keep 2099 but Dec?)
  }else{
    fut126$fishvar<-fut126$fishvar[,,which(fut126$years <= "2099")] # REVISION: changed from 2099 to 2100 (or keep 2099?)
  }
  fut126_2<-as.data.frame.table(fut126$fishvar) %>%
    `colnames<-`(c("lon", "lat", "year1", "tcb")) %>%
    mutate(lat = as.numeric(as.character(lat))) %>%
    mutate(lat2 = cos(abs(lat) * (pi/180))) %>%
    # mutate(tcb = tcb * lat2) %>%
    # mutate(year = round(as.numeric(as.character(year1)))) %>%
    mutate(year = as.numeric(as.character(gsub("\\..*", "", year1)))) %>% # REVISION: changed from above line
    group_by(year) %>%
    dplyr::summarise(tcb = weighted.mean(tcb, lat2, na.rm=TRUE)) # REVISION - weighed mean - NO
    # dplyr::summarise(tcb = mean(tcb, na.rm=TRUE))

  # projections start at different times for the 2 cmips
  if(cmip == 5){
    all126<-hist_2 %>%
      full_join(fut126_2) %>%
      mutate(TcbChange = (tcb - refDecade$value)/refDecade$value * 100) %>%
      mutate(color = ifelse(year < 2006, "hist", "ssp126")) # REVISION: changed from 2005 because 2005 is actually hist
  }else{
    all126<-hist_2 %>%
      full_join(fut126_2) %>%
      mutate(TcbChange = (tcb - refDecade$value)/refDecade$value * 100) %>%
      mutate(color = ifelse(year < 2015, "hist", "ssp126"))
  }

  filter(all126, year == 1971)
  filter(all126, year == 2099)

  # ssp585
  dimnames(fut585$fishvar)<-list(fut585$lon, fut585$lat, fut585$years)
  if(output == "monthly"){
    fut585$fishvar<-fut585$fishvar[,,which(fut585$years <= "Dec 2099")] # REVISION see above
  }else{
    fut585$fishvar<-fut585$fishvar[,,which(fut585$years <= "2099")] # REVISION see above
  }

  fut585_2<-as.data.frame.table(fut585$fishvar) %>%
    `colnames<-`(c("lon", "lat", "year1", "tcb")) %>%
    mutate(lat = as.numeric(as.character(lat))) %>%
    mutate(lat2 = cos(abs(lat) * (pi/180))) %>%
    # mutate(tcb = tcb * lat2) %>%
    # mutate(year = round(as.numeric(as.character(year1)))) %>%
    mutate(year = as.numeric(as.character(gsub("\\..*", "", year1)))) %>% # REVISION: changed from above line
    group_by(year) %>%
    # dplyr::summarise(tcb = mean(tcb, na.rm=TRUE)) %>%
    dplyr::summarise(tcb = weighted.mean(tcb, lat2, na.rm=TRUE)) %>% # REVISION - weighed mean - NO
    mutate(TcbChange = (tcb - refDecade$value)/refDecade$value * 100) %>%
    mutate(color = "ssp585")

  all<-all126 %>%
    full_join(fut585_2)

  rm(hist_2, refDecade, fut126_2, all126, fut585_2)
  return(all)
  
}

# get_timeSeries temperature ---- 

get_timeSeries_temperature<-function(hist, fut126, fut585, output = "yearly"){
  
  dimnames(hist$fishvar)<-list(hist$lon, hist$lat, hist$years)
  dim(hist$fishvar)
  if(output == "monthly"){
    hist$fishvar<-hist$fishvar[,,which(hist$years >= "Jan 1971")] 
  }else{
    hist$fishvar<-hist$fishvar[,,which(hist$years >= "1971")] 
  }
  
  # # OPTION 1 
  # 
  # # hist
  # hist_2<-as.data.frame.table(hist$fishvar) %>%
  #   `colnames<-`(c("lon", "lat", "year1", "tcb")) %>% 
  #   mutate(year = round(as.numeric(as.character(year1))))
  # 
  # refDecade <- hist_2 %>% 
  #   filter(year >= 1990, year <=2000) %>% 
  #   group_by(lon, lat) %>% 
  #   dplyr::summarise(refValue = mean(tcb, na.rm=TRUE))
  # 
  # hist_2<-hist_2 %>% 
  #   full_join(refDecade)
  # 
  # hist_2<-hist_2 %>% 
  #   mutate(TcbChange = tcb - refValue) 
  # 
  # hist_2<-hist_2 %>% 
  #   mutate(lat = as.numeric(as.character(lat))) %>%
  #   mutate(lat2 = cos(abs(lat) * (pi/180))) # %>% # transform lat in radiant and calculate cosine
  #   # mutate(TcbChange = TcbChange * lat2) # should this be part of the weighted.mean below? 
  # 
  # hist_2<-hist_2 %>% 
  #   group_by(year) %>% 
  #   dplyr::summarise(TcbChange = weighted.mean(TcbChange, lat2, na.rm=TRUE)) 
  # # revision weighted mean, should this be lat2 or lat2/sum(lat2)? it does not seem to make any difference...
  # 
  # hist_2<-hist_2 %>%
  #   mutate(color = "hist")
  # 
  # # ssp126
  # dim(fut126$fishvar)
  # dimnames(fut126$fishvar)<-list(fut126$lon, fut126$lat, fut126$years)
  # if(output == "monthly"){
  #   fut126$fishvar<-fut126$fishvar[,,which(fut126$years <= "Jan 2099")]
  # }else{
  #   fut126$fishvar<-fut126$fishvar[,,which(fut126$years <= "2099")]
  # }
  # 
  # fut126_2<-as.data.frame.table(fut126$fishvar) %>% 
  #   `colnames<-`(c("lon", "lat", "year1", "tcb")) %>% 
  #   mutate(year = round(as.numeric(as.character(year1)))) 
  # 
  # fut126_2<-fut126_2 %>% 
  #   full_join(refDecade)
  # 
  # fut126_2<-fut126_2 %>% 
  #   mutate(TcbChange = tcb - refValue) 
  # 
  # fut126_2<-fut126_2 %>%
  #   mutate(lat = as.numeric(as.character(lat))) %>%
  #   mutate(lat2 = cos(abs(lat) * (pi/180))) # %>%
  #   # mutate(TcbChange = TcbChange * lat2) 
  # 
  # fut126_2<-fut126_2 %>% 
  #   group_by(year) %>% 
  #   dplyr::summarise(TcbChange = weighted.mean(TcbChange, lat2, na.rm=TRUE)) 
  # # revision weighted mean, 
  # 
  # filter(fut126_2, year ==2099)
  # 
  # fut126_2<-fut126_2 %>% 
  #   mutate(color = "ssp126")
  # 
  # # ssp585
  # dimnames(fut585$fishvar)<-list(fut585$lon, fut585$lat, fut585$years)
  # if(output == "monthly"){
  #   fut585$fishvar<-fut585$fishvar[,,which(fut585$years <= "Jan 2099")]
  # }else{
  #   fut585$fishvar<-fut585$fishvar[,,which(fut585$years <= "2099")]
  # }
  # 
  # fut585_2<-as.data.frame.table(fut585$fishvar) %>%
  #   `colnames<-`(c("lon", "lat", "year1", "tcb")) %>% 
  #   mutate(year = round(as.numeric(as.character(year1)))) 
  # 
  # fut585_2<-fut585_2 %>% 
  #   full_join(refDecade)
  # 
  # fut585_2<-fut585_2 %>% 
  #   mutate(TcbChange = tcb - refValue)
  # 
  # fut585_2<-fut585_2 %>% 
  #   mutate(lat = as.numeric(as.character(lat))) %>%
  #   mutate(lat2 = cos(abs(lat) * (pi/180))) #%>%
  #   # mutate(TcbChange = TcbChange * lat2) 
  # 
  # fut585_2<-fut585_2 %>% 
  #   group_by(year) %>% 
  #   dplyr::summarise(TcbChange = weighted.mean(TcbChange, lat2, na.rm=TRUE)) # revision weighted mean - NO
  # 
  # filter(fut585_2, year ==2099)
  # 
  # fut585_2<-fut585_2 %>% 
  #   mutate(color = "ssp585")
  # 
  # # ?weighted.mean
  # 
  # all<-hist_2 %>% 
  #   full_join(fut126_2) %>% 
  #   full_join(fut585_2)
  # 
  # rm(hist_2, refDecade, fut126_2, fut585_2)
  
  # OPTION 2
  # A # mutate(tcb = tcb * lat2) %>% dplyr::summarise(tcb = mean(tcb, na.rm=TRUE))
  # B # dplyr::summarise(tcb = weighted.mean(tcb, lat2, na.rm=TRUE))
  # no difference in results if we calcaulte relative changes before weighting.
  # The difference between the two approached is only due to the weighting function used:
  # mutate(tcb = tcb * lat2) %>% dplyr::summarise(tcb = mean(tcb, na.rm=TRUE)) OR
  # dplyr::summarise(tcb = weighted.mean(tcb, lat2, na.rm=TRUE))

  hist_2<-as.data.frame.table(hist$fishvar) %>%
    `colnames<-`(c("lon", "lat", "year1", "tcb")) %>%
    # weight by grid area (smaller at higher lats)
    mutate(lat = as.numeric(as.character(lat))) %>%
    mutate(lat2 = cos(abs(lat) * (pi/180))) %>% # transform lat in radiant and calculate cosine
    # mutate(tcb = tcb * lat2) %>%
    mutate(year = round(as.numeric(as.character(year1))))  %>% # TRIAL HERE
    group_by(year) %>%
    # dplyr::summarise(tcb = mean(tcb, na.rm=TRUE))
    dplyr::summarise(tcb = weighted.mean(tcb, lat2, na.rm=TRUE)) # revision weighted mean - NO

  # ref decade
  refDecade <- hist_2 %>%
    filter(year >= 1990, year <=2000) %>%
    dplyr::summarize(value = mean(tcb, na.rm = TRUE))

  # ssp126
  dimnames(fut126$fishvar)<-list(fut126$lon, fut126$lat, fut126$years)
  if(output == "monthly"){
    fut126$fishvar<-fut126$fishvar[,,which(fut126$years <= "Jan 2099")]
  }else{
    fut126$fishvar<-fut126$fishvar[,,which(fut126$years <= "2099")]
  }
  fut126_2<-as.data.frame.table(fut126$fishvar) %>%
    `colnames<-`(c("lon", "lat", "year1", "tcb")) %>%
    mutate(lat = as.numeric(as.character(lat))) %>%
    mutate(lat2 = cos(abs(lat) * (pi/180))) %>%
    # mutate(tcb = tcb * lat2) %>%
    mutate(year = round(as.numeric(as.character(year1)))) %>% # TRIAL HERE
    group_by(year) %>%
    # dplyr::summarise(tcb = mean(tcb, na.rm=TRUE))
    dplyr::summarise(tcb = weighted.mean(tcb, lat2, na.rm=TRUE))  # revision weighted mean - NO

  # projections start at different times for the 2 cmips
  if(cmip == 5){
    all126<-hist_2 %>%
      full_join(fut126_2) %>%
      # mutate(TcbChange = (tcb - refDecade$value)/refDecade$value * 100) %>% # percentage
      mutate(TcbChange = tcb - refDecade$value) %>% # celsius change as per map: diff_2p6 = fut126$fishvar - hist$fishvar # 20C - 15C = 5C (celsius increase)
      mutate(color = ifelse(year <= 2005, "hist", "ssp126"))
  }else{
    all126<-hist_2 %>%
      full_join(fut126_2) %>%
      # mutate(TcbChange = (tcb - refDecade$value)/refDecade$value * 100) %>%
      mutate(TcbChange = tcb - refDecade$value) %>% # celsius change
      mutate(color = ifelse(year <= 2015, "hist", "ssp126"))
  }

  # ssp585
  dimnames(fut585$fishvar)<-list(fut585$lon, fut585$lat, fut585$years)
  if(output == "monthly"){
    fut585$fishvar<-fut585$fishvar[,,which(fut585$years <= "Jan 2099")]
  }else{
    fut585$fishvar<-fut585$fishvar[,,which(fut585$years <= "2099")]
  }
  fut585_2<-as.data.frame.table(fut585$fishvar) %>%
    `colnames<-`(c("lon", "lat", "year1", "tcb")) %>%
    mutate(lat = as.numeric(as.character(lat))) %>%
    mutate(lat2 = cos(abs(lat) * (pi/180))) %>%
    # mutate(tcb = tcb * lat2) %>%
    mutate(year = round(as.numeric(as.character(year1)))) %>%
    group_by(year) %>% # TRIAL HERE
    # dplyr::summarise(tcb = mean(tcb, na.rm=TRUE)) %>%
    dplyr::summarise(tcb = weighted.mean(tcb, lat2, na.rm=TRUE)) %>% # revision weighted mean - NO
    # # mutate(TcbChange = (tcb - refDecade$value)/refDecade$value * 100) %>%
    mutate(TcbChange = tcb - refDecade$value) %>% # celsius change
    mutate(color = "ssp585")

  all<-all126 %>%
    full_join(fut585_2)

  # filter(all, year == 2099) # option 1
  # filter(all1, year == 2099)
  # # year   tcb TcbChange color
  # # <dbl> <dbl>     <dbl> <chr>
  # # 1  2099  12.5     0.634 ssp126
  # # 2  2099  14.5     2.58  ssp585
  #
  # # instead mean the difference here - after having calculated the difference in each grid cell
  # # all<-all %>%
  # #   group_by(year,color) %>%
  # #   dplyr::summarise(TcbChange = mean(TcbChange, na.rm=TRUE))
  # #
  # # check difference
  # # filter(all, year == 2099) # option 2
  # # year color  TcbChange
  # # <dbl> <chr>      <dbl>
  # # 1  2099 ssp126     0.634
  # # 2  2099 ssp585     2.58

  rm(hist_2, refDecade, fut126_2, all126, fut585_2)
  return(all)
  
}

# averageEnvironmentalCDF ----

# Extracts data for a particular period from a particular environmental netcdf. time1 and time2 have to be 'yearmon' variables (see "zoo" package).
# Assumes yearly averaging unless average_whole_period is set to true. When yearly averaging, times have to begin in Jan and end in Dec 

averageEnvironmentalCDF <-
  function(directory,
           filename,
           variable,
           time1,
           time2,
           average_whole_period = FALSE,
           hist = TRUE, 
           ipsl = FALSE, 
           cmip = 5)
  {
    
    # Open the netcdf file
    nc <- nc_open(paste0(directory, filename))
    
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
        yearmonth = as.yearmon("1850-1-1") 
      }else{
        yearmonth = as.yearmon("2015-1-1")
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
      # if (names(nc$dim)[length(nc$dim)] != "TIME")
      #   print("Warning: might be assuming wrong dimension for time from NetCDF file") 
      # temp_array1 <- ncvar_get(nc, variable) # CN dimnames and variable order do not match - time is always the last dimention
      # dim(temp_array1)
      
      temp_array1 <- ncvar_get(nc, variable)[, , tpte$start_point_to_extract:tpte$end_point_to_extract]

      if(num_years>1 & output == "monthly"){ 
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

    } else # average_whole_period == TRUE
    {
      # Create a new matrix to hold the results
      new_var_mat = array(rep(0, length(lon) * length(lat)), c(length(lon), length(lat)))
      
      # Get the correct time period to extract
      tpte = time_period_to_extract(time1, time2, time_vector)
      
      # Now get the variable of interest
      # if (names(nc$dim)[length(nc$dim)] != "time")
      #   print("Warning: might be assuming wrong dimension for time from NetCDF file")
      
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

# model average ----

# 3 options: ipsl, gfdl, or combined depending on data_ipsl_CMIP5
# old version of Derek's code, here vectorised 

model_stat<-function(data, esm){
  
  X <- lapply(data, `[[`, esm)
  X <- lapply(X, function(x) replace(x, !is.finite(x), NA))
  Y <- do.call(cbind, X)
  Y <- array(Y, dim=c(dim(X[[1]]), length(X)))
  model_average_new<-apply(Y, c(1, 2), mean, na.rm = TRUE)
  
  # model_average_new[!is.finite(model_average_new)]<-NA # just to make sure that all NaN 
  # and Inf resulting from mean() are treated as NA 
  model_sd_new<-apply(Y, c(1, 2), sd, na.rm = TRUE)
  # model_sd_new[!is.finite(model_sd_new)]<-NA
  
  X <- lapply(data, `[[`, esm)
  Y <- do.call(cbind, X)
  Y <- array(Y, dim=c(dim(X[[1]]), length(X)))
  
  # takes into account agreement on both positive or negative changes - i.e. if most models show positive change, agreement will be high and vice versa. 
  temp<-apply(Y, c(1,2), function(x) length(x[which(x>=0)])) # sum the number of values that are > = 0 (increase or stable) 
  temp2<-apply(Y, c(1,2), function(x) length(x[which(!is.na(x))])) # sum the number of values that are different from NA and NaN
  
  temp4<-temp/temp2
  model_agreement_new<-ifelse(is.na(temp4), NA, ifelse(temp4>0.5, temp4*100, (1-temp4)*100)) 
  # interpretation: 
  # if 0/7 = 0 -> 100 # [1,88]
  # if 1/7 = 0.14 it means that 1 over 7 models show increases hence model strongly agree on the decrease -> (1-temp4)*100) = 86%
  # if 2/7 = 0.28 -> 72% on decreases
  # if 3/7 = 0.42 -> 57% on decreases 
  # if 4/7 = 0.57 -> 0.57*100 = 57% on increases 
  # if 5/7 = 0.71 -> 71% on increases 
  # if 6/7 = 0.85 -> 85% on increases 
  # if 7/7 = 100% on increases 
  
  return(list(model_average = model_average_new, model_sd = model_sd_new, model_agreement = model_agreement_new))
}

