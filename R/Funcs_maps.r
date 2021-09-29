
# Inputs: a 360 x 180 matrix, with lon from -180 to 180, lat from 90 to -90
# a string representing the data title

plot_FISH_MIP <- function(data_to_plot, 
                          data_title, 
                          plot_limits = c(min(min(data_to_plot, na.rm=T),na.rm=T), 
                                          max(max(data_to_plot, na.rm=T)),na.rm=T), 
                          colour_scheme = colour_scheme1,
                          coltitle = "",
                          latlon_limits = NA, 
                          model_type = NA, 
                          legend_ticks = 5,
                          num_dp = 0,
                          contour = FALSE){
  
  # data_to_plot = flip_sign_data$data_negative
  # data_title = maptitle_sign_negative
  # plot_limits = c(-50, 0) 
  # colour_scheme = colour_scheme4
  # coltitle = "Difference \nin (%) \nchange \n"
  # latlon_limits = NA 
  # model_type = NA 
  # legend_ticks = 3
  # num_dp = 0
  # contour = FALSE
 
  # Set up the raster
  e <- extent(c(-180, 180, -90, 90))
  if (!is.na(model_type)){ # DBEM is 0.5 degree - here taken into account
    r2 <- raster(nrows = 360, ncols = 720, ext = e)
  }else {
    r2 <- raster(nrows = 180, ncols = 360, ext = e)
  }
  r2[] <- t(data_to_plot)
  
  # Set up projections   
  robCRS <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

  # use ggplot2 for mapping
  theme_opts <- list(theme(panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank(),
                           panel.background = element_blank(),
                           plot.background = element_rect(fill="white"),
                           panel.border = element_blank(),
                           legend.key.height = unit(0.8, "cm"),
                           plot.title = element_text(size=12, hjust = 0.5)))
    
  # convert r2 (rasterLayer) to geom_sf object. 
  r.1.sf <- rasterToPolygons(r2) # convert Rasterlayer to spatial polygon dataframe 
  r.1.sf <- st_as_sf(r.1.sf) # convert dataframe to sf object 
  r.1.sf <- st_transform(r.1.sf, crs = st_crs(robCRS)) # convert sf object to Robinson Projection  
    
  r1_gg <- ggplot() + 
    geom_sf(data = r.1.sf, aes(fill = layer), colour = NA)+
    geom_sf(data = world_sf, size = 0.05, fill = "grey20")+
    scale_fill_gradient2(low = colour_scheme[1], 
                         mid = colour_scheme[2],  
                         high = colour_scheme[3],
                         limits = c(plot_limits[1], plot_limits[2]),
                         midpoint = ((plot_limits[2]-plot_limits[1])/2 + plot_limits[1]),
                         oob = scales::squish, 
                         guide = guide_colorbar(label.position = "right", 
                                                title = coltitle),
                         space = "Lab",
                         name = "", 
                         na.value='lightgrey',
                         breaks = seq(plot_limits[1],plot_limits[2],length.out = legend_ticks),
                         labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = legend_ticks), digits = num_dp), nsmall = num_dp)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = data_title) +
    theme_opts 
  
  rm(r.1.sf)
  return(r1_gg)

}

