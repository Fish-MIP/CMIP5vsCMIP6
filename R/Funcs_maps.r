library(raster)
library(maptools)
library(RColorBrewer)
library(ggplot2)
library(scales)

# Inputs: a 360 x 180 matrix, with lon from -180 to 180, lat from 90 to -90
# a string representing the data title
plot_FISH_MIP <- function(data_to_plot, 
                          data_title, 
                          plot_limits = c(min(min(data_to_plot, na.rm=T),na.rm=T), 
                                          max(max(data_to_plot, na.rm=T)),na.rm=T), 
                          colour_scheme = colour_scheme1,
                          # show_coast = TRUE, 
                          coltitle = "",
                          latlon_limits = NA, 
                          model_type = NA, 
                          legend_ticks = 5){
  
  # # trial
  # data_ipsl_CMIP5[[1]]$diff_8p5, tit, delta_plot_colour_values, colour_scheme = colour_scheme2, coltitle = "Biomass \n Change \n (%)"
  # data_to_plot = data_ipsl_CMIP6$BOATS$diff_8p5
  # data_title = "A"
  # colour_scheme = colour_scheme2
  # coltitle = ""
  # latlon_limits = NA 
  # model_type = NA 
  # legend_ticks = 5

  # overwrite limits  
  # plot_limits = c(-50, 50)
  
  # no coastlines 
  # colRam <- colorRampPalette(c("blue","yellow","red"))
  # WL <- readShapePoly("c:/users/derekt/work/research/data/coastline/gshhg-shp-2.3.6/gshhs_shp/c/GSHHS_c_L1.shp")
  # WL2 <- readShapePoly("c:/users/derekt/work/research/data/coastline/gshhg-shp-2.3.6/gshhs_shp/c/GSHHS_c_L6.shp")
  # WL <- rbind(WL, WL2)
  # wgs1984.proj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  # proj4string(WL) <- wgs1984.proj

  # Assume that the data are coming in starting from -180 and the mapping script expects to start at 0, so fix here
  #temp_array3 = data_to_plot
  #data_to_plot[1:180,] = data_to_plot[181:360,]
  #data_to_plot[181:360,] = temp_array3[1:180,]
  #remove(temp_array3)
  
  # Set up the raster
  # model_type <- get_model_type(filename)
  e <- extent(c(-180, 180, -90, 90))
  if (!is.na(model_type)){ # DBEM is 0.5 degree!
    r2 <- raster(nrows = 360, ncols = 720, ext = e)
  }else {
    r2 <- raster(nrows = 180, ncols = 360, ext = e)
  }
  r2[] <- t(data_to_plot)
  
  dim(data_to_plot)
  dim(r2)
  
  # Set up projections   
  robCRS <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  robCRS_no <- 54030
  
  lonlatCRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  lonlatCRS_no <- 4326
  
  # specify projections for input data (latlong)
  projection(r2) <- lonlatCRS
  
  # option 1: convert RasterLayer to a data.frame. this gives strange maps
  # cc_rob <-projectRaster(r2, crs = robCRS, over = T) # Convert RasterLayer object to Robinson Projection
  # r.1 <- rasterToPoints(cc_rob) # convert RasterLayer to SpatialPixelsDataFrame 
  # r.1.df <- as.data.frame(r.1) # convert SpatialPixelsDataFrame to dataframe
  # colnames(r.1.df) <-c("x","y","Sp")
  
  # alternative: convert r2 (rasterLayer) to geom_sf object. This gives better maps 
  r.1.sf<-rasterToPolygons(r2) # convert Rasterlayer to spatial polygon dataframe 
  r.1.sf <- st_as_sf(r.1.sf) # convert dataframe to sf object 
  r.1.sf <- st_transform(r.1.sf, crs = st_crs(robCRS)) # convert sf object to Robinson Projection

  num_dp = 0
  
  # then you can use ggplot2 to plot that object
  theme_opts <- list(theme(panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank(),
                           panel.background = element_blank(),
                           plot.background = element_rect(fill="white"),
                           panel.border = element_blank(),
                           legend.key.height = unit(0.8, "cm"),
                           # axis.line = element_blank(),
                           # axis.text.x = element_blank(),
                           # axis.text.y = element_blank(),
                           # axis.ticks = element_blank(),
                           # axis.title.x = element_blank(),
                           # axis.title.y = element_blank(),
                           plot.title = element_text(size=12, hjust = 0.5)))

  r1_gg <- ggplot() + 
    geom_sf(data = r.1.sf, aes(fill = layer), colour = NA)+
    geom_sf(data = world_sf, size = 0.05, fill = "grey20")+
    # geom_tile(data = r.1.df, aes(x=x, y=y, fill = Sp)) + # this is if you plot the dataframe but not looking good
    scale_fill_gradient2(low = colour_scheme[1], 
                         mid = colour_scheme[2],  
                         high = colour_scheme[3], 
                         # na.value = "yellow",
                         limits = c(plot_limits[1], plot_limits[2]),
                         midpoint = ((plot_limits[2]-plot_limits[1])/2 + plot_limits[1]),
                         oob = scales::squish, 
                         guide = guide_colorbar(label.position = "right", 
                                                # title.hjust = 0.5, 
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

#Inputs: a 360 x 180 matrix, with lon from -180 to 180, lat from 90 to -90
# a string representing the data title
plot_FISH_MIP_halfdeg <- function(data_to_plot, 
                                  data_title, 
                                  plot_limits = c(min(min(data_to_plot, na.rm=T),na.rm=T), 
                                                  max(max(data_to_plot, na.rm=T)),na.rm=T), 
                                  flip_latitudes = FALSE, 
                                  colour_scheme = 1, 
                                  show_coast = TRUE, 
                                  coltitle = "")
{
  
  # # trial
  # data_to_plot = boats_diff_ipsl_8p5
  # data_title = maptitle_delta_ipsl_8p5
  # flip_latitudes = FALSE 
  # colour_scheme = 1 
  # show_coast = TRUE 
  # coltitle = ""
  # plot_limits =c(-50,50)
  
  # TO DELETE 
  # colRam <- colorRampPalette(c("blue","yellow","red"))
  # #WL <- readShapePoly("c:/users/abuchholz/Dropbox/Phd/Analysis/fishMip/analysis/fishMip/shapefiles/10m_physical/ne_10m_land.shp")#download from 'naturalearth.com'
  # WL <- readShapePoly("c:/users/derekt/work/research/data/coastline/ne_10m_land/ne_10m_land.shp")#download from 'naturalearth.com'
  # 
  # wgs1984.proj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  # proj4string(WL) <- wgs1984.proj
  # END TO DELETE
  
  # Assume that the data are coming in starting from -180 and the mapping script expects to start at 0, so fix here
  #temp_array3 = data_to_plot
  #data_to_plot[1:180,] = data_to_plot[181:360,]
  #data_to_plot[181:360,] = temp_array3[1:180,]
  #remove(temp_array3)
  
  # Set up the raster
  if (flip_latitudes)
    e <- extent(c(-180,180, 90,-90))  
 else
    e <- extent(c(-180, 180, -90, 90))
    r2 <- raster(nrows = 360, ncols = 720, ext = e)
    r2[] <- t(data_to_plot)
  
  dim(data_to_plot)  
  dim(r2)  
  
  # Set up projections   
  robinson_proj <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  # worldRobinson <- spTransform(WL, robinson_proj)
  wgs1984<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  projection(r2) <-wgs1984
  cc_rob <-projectRaster(r2, crs = robinson_proj, over = T)
  
  # To convert your RasterLayer to a data.frame, you need to convert it to
  # a SpatialPixelsDataFrame first
  r.1 <- rasterToPoints(cc_rob)
  r.1.df <- as.data.frame(r.1)
  colnames(r.1.df) <-c("x","y","Sp")
  
  num_dp = 0
  
  
  # then you can use ggplot2 to plot that object
  library(ggplot2)
  library(scales)
  theme_opts <- list(theme(panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank(),
                           panel.background = element_blank(),
                           plot.background = element_rect(fill="white"),
                           panel.border = element_blank(),
                           legend.key.height = unit(0.7, "cm"),
                           axis.line = element_blank(),
                           axis.text.x = element_blank(),
                           axis.text.y = element_blank(),
                           axis.ticks = element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           plot.title = element_text(size=12)))
  
  if (colour_scheme == 1)
  {
    if (show_coast)
    {
      r1_gg <- ggplot(r.1.df, aes(x=x, y=y)) + 
      geom_tile(aes(fill = Sp)) +
     # scale_fill_gradient2(low = 'royalblue1', mid = "yellow1",  high = 'red', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left"),
        scale_fill_gradient2(low = 'white', mid = "skyblue1",  high = 'royalblue1', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "right", title.hjust = 0.5, title = coltitle),
                             midpoint = ((plot_limits[2]-plot_limits[1])/2 + plot_limits[1]),
                             space = "Lab",name = "", na.value='lightgrey',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
                           labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) +
      # geom_polygon(data = worldRobinson, aes(x = long, y = lat, group = group),col=c('black'),fill='black')+ 
      labs(title = data_title) +  
      coord_equal()+
      theme_opts 
    } else
    {
      r1_gg <- ggplot(r.1.df, aes(x=x, y=y)) + 
        geom_tile(aes(fill = Sp)) +
      #  scale_fill_gradient2(low = 'royalblue1', mid = "yellow1",  high = 'red', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left"),
        scale_fill_gradient2(low = 'white', mid = "skyblue1",  high = 'royalblue1', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "right", title.hjust = 0.5, title = coltitle),
                             midpoint = ((plot_limits[2]-plot_limits[1])/2 + plot_limits[1]),
                             space = "Lab",name = "", na.value='lightgrey',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
                             labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) +
        labs(title = data_title) +  
        coord_equal()+
        theme_opts      
    }
  } else
  {
    if (show_coast)
    {
      r1_gg <- ggplot(r.1.df, aes(x=x, y=y)) + 
      geom_tile(aes(fill = Sp)) +
     # scale_fill_gradient2(low = 'royalblue1', mid = "white",  high = 'red', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left"),
        scale_fill_gradient2(low = 'red', mid = "white",  high = 'royalblue1', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "right", title.hjust = 0.5, title = coltitle),
                             midpoint = ((plot_limits[2]-plot_limits[1])/2 + plot_limits[1]), 
                           space = "Lab",name = "", na.value='lightgrey',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
                           labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) +
      geom_polygon(data = worldRobinson, aes(x = long, y = lat, group = group),col=c('black'),fill='black')+ 
      labs(title = data_title) +  
      coord_equal()+
      theme_opts 
    } else
    {
      r1_gg <- ggplot(r.1.df, aes(x=x, y=y)) + 
        geom_tile(aes(fill = Sp)) +
      #  scale_fill_gradient2(low = 'royalblue1', mid = "white",  high = 'red', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left"),
        scale_fill_gradient2(low = 'red', mid = "white",  high = 'royalblue1', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "right", title.hjust = 0.5, title = coltitle),
                             midpoint = ((plot_limits[2]-plot_limits[1])/2 + plot_limits[1]), 
                             space = "Lab",name = "", na.value='lightgrey',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
                             labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) +
        labs(title = data_title) +  
        coord_equal()+
        theme_opts   
    }
  }
}





# a string representing the data title
plot_FISH_MIP_shape <- function(data_to_plot, data_title, plot_limits = c(min(min(data_to_plot, na.rm=T),na.rm=T), 
                                                                    max(max(data_to_plot, na.rm=T)),na.rm=T), colour_scheme = 1, 
                          show_coast = TRUE, coltitle = "")
{
 # ggplot() + 
#    geom_polygon(data = ab_join, aes(x = long, y = lat, group = group, fill = difference), 
#                 colour = "black", size = 0.5)
  
  
  colRam <- colorRampPalette(c("blue","yellow","red"))
  WL <- readShapePoly("c:/users/derekt/desktop/gshhg-shp-2.3.6/gshhs_shp/c/GSHHS_c_L1.shp")
  WL2 <- readShapePoly("c:/users/derekt/desktop/gshhg-shp-2.3.6/gshhs_shp/c/GSHHS_c_L6.shp")
  WL <- rbind(WL, WL2)
  WL <- rbind(WL, WL2)
  wgs1984.proj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


  # Set up the raster
  #e <- extent(c(-180, 180, -90, 90))
  
  #r2 <- raster(nrows = 180, ncols = 360, ext = e)
  #r2[] <- t(data_to_plot)
  
  # Set up projections   
  robinson_proj <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  worldRobinson <- spTransform(WL, robinson_proj)
  data_to_plot <-spTransform(data_to_plot, robinson_proj)
  
  ab = fortify(data_to_plot)
  data_to_plot@data$id <- 0:(dim(data_to_plot@data)[1]-1) # add id field
  ab_join = plyr::join(x = ab,y = data_to_plot@data, by="id")
  
  #projection(r2) <-wgs1984
  #cc_rob <-projectRaster(r2, crs = robinson_proj, over = T)
  
  # To convert your RasterLayer to a data.frame, you need to convert it to
  # a SpatialPixelsDataFrame first
  #r.1 <- rasterToPoints(cc_rob)
  #r.1.df <- as.data.frame(r.1)
  #colnames(r.1.df) <-c("x","y","Sp")
  
  num_dp = 0
  
  
  # then you can use ggplot2 to plot that object
  library(ggplot2)
  library(scales)
  theme_opts <- list(theme(panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank(),
                           panel.background = element_blank(),
                           plot.background = element_rect(fill="white"),
                           panel.border = element_blank(),
                           legend.key.height = unit(0.7, "cm"),
                           axis.line = element_blank(),
                           axis.text.x = element_blank(),
                           axis.text.y = element_blank(),
                           axis.ticks = element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           plot.title = element_text(size=12, hjust = 0.5)))
  
  if (colour_scheme == 0)
  {
    if (show_coast)
    {
      r1_gg <- ggplot(r.1.df, aes(x=x, y=y)) + 
        geom_tile(aes(fill = Sp)) +
        scale_fill_gradient2(low = 'white', mid = "skyblue1",  high = 'royalblue1', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "right", title.hjust = 0.5, title = coltitle),
                                   midpoint = ((plot_limits[2]-plot_limits[1])/2 + plot_limits[1]),
                             space = "Lab",name = "", na.value='lightgrey',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
                             labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) +
        geom_polygon(data = worldRobinson, aes(x = long, y = lat, group = group),col=c('black'),fill='black')+
        labs(title = data_title) +
       coord_equal()+
        theme_opts 
      
      
      ggplot() + 
        scale_fill_gradient2(low = 'white', mid = "skyblue1",  high = 'royalblue1', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "right", title.hjust = 0.5, title = coltitle),
                             midpoint = ((plot_limits[2]-plot_limits[1])/2 + plot_limits[1]),
                             space = "Lab",name = "", na.value='lightgrey',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
                             labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) +
        geom_polygon(data = ab_join, aes(x = long, y = lat, group = group, fill = difference), 
                     colour = "black", size = 0.5) +
        labs(title = data_title) +
        #geom_polygon(data = worldRobinson, aes(x = long, y = lat, group = group),col=c('black'),fill='black')+
        coord_equal()+
        theme_opts 
      
      
      
      
    } else
    {
      r1_gg <- ggplot(r.1.df, aes(x=x, y=y)) + 
        geom_tile(aes(fill = Sp)) +
        #     scale_fill_gradient2(low = 'royalblue1', mid = "yellow1",  high = 'red', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left", title.hjust = 1, title = expression(paste('kg C m' ^ '-2'))),
        # scale_fill_gradient2(low = 'royalblue1', mid = "yellow1",  high = 'red', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left", title.hjust = 1),
        scale_fill_gradient2(low = 'white', mid = "skyblue1",  high = 'royalblue1', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "right", title.hjust = 0.5, title = coltitle),
                             midpoint = ((plot_limits[2]-plot_limits[1])/2 + plot_limits[1]),
                             space = "Lab",name = "", na.value='lightgrey',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
                             labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) +
        labs(title = data_title) +
        # scale_fill_viridis(option="magma",limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left", title.hjust = 1),
        #                                          space = "Lab",name = "", na.value='white',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
        #                    labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) + 
        coord_equal()+
        theme_opts       
    }
  } else if (colour_scheme == 1)
  {
    if (show_coast)
    {
      r1_gg <- ggplot(r.1.df, aes(x=x, y=y)) + 
        geom_tile(aes(fill = Sp)) +
        scale_fill_gradient2(low = 'white', mid = "darkseagreen1",  high = 'darkseagreen', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "right", title.hjust = 0.5, title = coltitle),
                             midpoint = ((plot_limits[2]-plot_limits[1])/2 + plot_limits[1]),
                             space = "Lab",name = "", na.value='lightgrey',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
                             labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) +
        geom_polygon(data = worldRobinson, aes(x = long, y = lat, group = group),col=c('black'),fill='black')+
        labs(title = data_title) +
        # scale_fill_viridis(option="magma",limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left", title.hjust = 1),
        #                                        space = "Lab",name = "", na.value='white',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
        #                    labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) + 
        coord_equal()+
        theme_opts 
    } else
    {
      r1_gg <- ggplot(r.1.df, aes(x=x, y=y)) + 
        geom_tile(aes(fill = Sp)) +
        #     scale_fill_gradient2(low = 'royalblue1', mid = "yellow1",  high = 'red', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left", title.hjust = 1, title = expression(paste('kg C m' ^ '-2'))),
        # scale_fill_gradient2(low = 'royalblue1', mid = "yellow1",  high = 'red', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left", title.hjust = 1),
        scale_fill_gradient2(low = 'white', mid = "skyblue1",  high = 'royalblue1', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "right", title.hjust = 0.5, title = coltitle),
                             midpoint = ((plot_limits[2]-plot_limits[1])/2 + plot_limits[1]),
                             space = "Lab",name = "", na.value='lightgrey',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
                             labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) +
        labs(title = data_title) +
        # scale_fill_viridis(option="magma",limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left", title.hjust = 1),
        #                                          space = "Lab",name = "", na.value='white',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
        #                    labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) + 
        coord_equal()+
        theme_opts       
    }
  } else
    
  { 
    if (show_coast)
    {
      r1_gg <- ggplot(r.1.df, aes(x=x, y=y)) + 
        geom_tile(aes(fill = Sp)) +
        # scale_fill_gradient2(low = 'royalblue1', mid = "white",  high = 'red', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left", title.hjust = 1, title = expression(paste('kg C m' ^ '-2'))),
        scale_fill_gradient2(low = 'red', mid = "white",  high = 'royalblue1', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "right", title.hjust = 1, title = coltitle),
                             #  scale_fill_gradient2(low = "#5A672D", mid = '#C59443',  high = '#6A9EC1', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left", title.hjust = 1),
                             midpoint = ((plot_limits[2]-plot_limits[1])/2 + plot_limits[1]), 
                             space = "Lab",name = "", na.value='lightgrey',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
                             labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) +
        geom_polygon(data = worldRobinson, aes(x = long, y = lat, group = group),col=c('black'),fill='black')+ 
        labs(title = data_title) + 
        # scale_fill_viridis(option="magma",limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left", title.hjust = 1),
        #                      space = "Lab",name = "", na.value='white',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
        #                    labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) + 
        # 
        coord_equal()+
        theme_opts 
    } else
    {
      r1_gg <- ggplot(r.1.df, aes(x=x, y=y)) + 
        geom_tile(aes(fill = Sp)) +
        # #    scale_fill_gradient2(low = 'royalblue1', mid = "white",  high = 'red', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left", title.hjust = 1, title = expression(paste('kg C m' ^ '-2'))),
        scale_fill_gradient2(low = 'red', mid = "white",  high = 'royalblue1', limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "right", title.hjust = 1, title = coltitle),
                             midpoint = ((plot_limits[2]-plot_limits[1])/2 + plot_limits[1]),
                             space = "Lab",name = "", na.value='lightgrey',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
                             labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) +
        labs(title = data_title) +  
        # scale_fill_viridis(option="magma",limits = c(plot_limits[1], plot_limits[2]),oob = squish, guide = guide_colorbar(label.position = "left", title.hjust = 1),
        #                     space = "Lab",name = "", na.value='white',breaks = seq(plot_limits[1],plot_limits[2],length.out = 5),
        #                    labels = format(round(seq(plot_limits[1],plot_limits[2],length.out = 5),digits = num_dp), nsmall = num_dp)) + 
        # 
        coord_equal()+
        theme_opts      
    }
  }
}
