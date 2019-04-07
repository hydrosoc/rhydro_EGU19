# devtools::install_github("mikejohnson51/AOI")
# devtools::install_github("bcgov/fasstr")
# install.packages("daymetr")
# install.packages("rgeos")
library(dplyr)
library(raster)
library(fasstr)

# library(AOI)
# library(daymetr)
# library(raster)
library(leaflet)

source("./02_rhydro2019_presentation/03_get-clean-viz_files/src/02_pub.R")


# library(ncdf4)

# Specify Regions and have a peak
# boreal_plains <- AOI::getAOI(clip = list("Atikameg", 100, 100), km = TRUE)
# boreal_plains %>% AOI::check()

# Specify Regions and have a peak
rockies <- AOI::getAOI(clip = list("Glacier National Park Canada", 100, 100), km = TRUE)
rockies %>% AOI::check()

# library(leaflet)
# # Map based on AOIs
# leaflet::leaflet(option=leafletOptions(zoomControl= FALSE, minZoom = 3, maxZoom = 5)) %>% 
#     addProviderTiles("Esri.NatGeoWorldMap", 
#                      group = "Terrain")  %>% 
#     # add bbox from AOI
#     addPolygons(data = rockies, fillColor = "transparent", color = "red", weight = 4) %>% 
#     addPolygons(data = boreal_plains, fillColor = "transparent", color = col, weight = 4) %>% 
#     # add labels
#     addLabelOnlyMarkers(lng = rockies@polygons[[1]]@labpt[1],
#                         lat = rockies@polygons[[1]]@labpt[2], 
#                         label = "Rck",
#                         labelOptions = labelOptions(noHide = T,
#                                                     direction = 'center',
#                                                     textOnly = T)) %>% 
#     addLabelOnlyMarkers(lng = boreal_plains@polygons[[1]]@labpt[1],
#                         lat = boreal_plains@polygons[[1]]@labpt[2], 
#                         label = "BP",
#                         labelOptions = labelOptions(noHide = T,
#                                                     direction = 'center',
#                                                     textOnly = T))



# either stations from day met R
# or Global Climate from weather raster data
# CHeck NWM too








# for rockies
path_rockies <- "./02_rhydro2019_presentation/03_get-clean-viz_files/data/rockies"

# download data from daymet service
params <- c("dayl", "tmin", "tmax", "prcp")
params %>% 
    purrr::walk(
        ~daymetr::download_daymet_ncss(location = c(rockies@bbox[[4]],
                                           rockies@bbox[[1]],
                                           rockies@bbox[[2]],
                                           rockies@bbox[[3]]), # top left to bottom right
                              start = 2010,
                              end = 2011,
                              param = .x,
                              frequency = "daily",
                              path = path_rockies))

# # prep for reprojecting raster
proj4.Lambert <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +a=6378137 +b=6356752.314706705 +units=km +no_defs" # Projected CS
proj4.WGS <- "+init=epsg:4326" # Geographic CS



# # function to get extent from one netcdf file
# get_ext <- function(nc_path){
#     nc <- ncdf4::nc_open(nc_path)
#     ymn <- ncdf4::ncatt_get(nc, varid = 0, attname = "geospatial_lat_min")$value
#     ymx <- ncdf4::ncatt_get(nc, varid = 0, attname = "geospatial_lat_max")$value
#     xmn <- ncdf4::ncatt_get(nc, varid = 0, attname = "geospatial_lon_min")$value
#     xmx <- ncdf4::ncatt_get(nc, varid = 0, attname = "geospatial_lon_max")$value
#     ext <- c(xmn, xmx, ymn, ymx)
#     ncdf4::nc_close(nc)
#     return(ext)
# }
# 
# # pull extent
# ext_r <- get_ext(list.files(path_rockies, 
#                              pattern = "prcp", 
#                              full.names = TRUE)[1])


# load data, reproject 
params <- c("dayl", "tmin", "tmax", "prcp")
rockies_stacks <- params %>% 
    purrr::map( function(x){
        
        list.files(path_rockies, 
                   pattern = x, 
                   full.names = TRUE) %>% 
            
            raster::stack() %>% 
            raster::`projection<-`(., proj4.Lambert) %>% 
            raster::projectRaster(crs = proj4.WGS)
            # raster::`extent<-`(ext_r)
        }
    ) %>% 
    setNames(params)


# raster::extract(prcp_stack, y = matrix(c(-110.0160, 42.5120), ncol = 2))

plot(rockies_stacks$tmin[[180]])

plot(rockies_stacks$prcp[[400]])



# custom function to calculate Hamon's PET 
et.ham <- function(tmin,tmax,dayl){
    # modified from Evapotranspiration package
    Ta <- (tmax + tmin)/2
    vs_Tmax <- 0.6108 * exp(17.27 * tmax/(tmax + 237.3))
    vs_Tmin <- 0.6108 * exp(17.27 * tmin/(tmin + 237.3))
    vas <- (vs_Tmax + vs_Tmin)/2
    ET_Hamon.Daily <- 0.55 * 25.4 * (dayl/12)^2 * (216.7 * 
                                                         vas * 10/(Ta + 273.3))/100
    return(ET_Hamon.Daily)
}

# calculate PET over all days (2 years total)
et_rockies <- et.ham(tmin = rockies_stacks$tmin,
                     tmax = rockies_stacks$tmax,
                     dayl = rockies_stacks$dayl / 3600)

# remove calc. artefacts (set to NA from previous raster)
et_rockies <- mask(et_rockies, rockies_stacks$dayl[[1]])

# set up indices for aggregating over months
year_mon <- seq(as.Date("2010-01-01"), as.Date("2011-12-31"), by = "1 day") %>% 
    format("%Y-%m")
months <- as.numeric(as.factor(year_mon))


# monthly totals of et 
et_monthly <- raster::stackApply(et_rockies, months, fun = sum)
et_monthly <- mask(et_monthly, rockies_stacks$dayl[[1]])

# plot result

plot(et_monthly) # static

# interactive for June
leaflet() %>%     
    addPolygons(data = rockies, fillColor = "transparent", color = col, weight = 4) %>% 
    addProviderTiles("Esri.NatGeoWorldMap", 
                     group = "Terrain") %>% 
    addRasterImage(x = et_monthly[[6]], opacity = .5)
    

# monthly totals of p
p_monthly <- raster::stackApply(rockies_stacks$prcp, months, fun = sum)
# plot(p_monthly)

# rough balance: p - et
pet_monthly <- p_monthly - et_monthly
# mask NA values for clean plotting
pet_monthly <- mask(pet_monthly, rockies_stacks$dayl[[1]])


plot(pet_monthly)

library(tidyhydat)
hy_set_default_db(hydat_path = "D:/ext_data_R/Hydat.sqlite3")

rockies_stns <- tidyhydat::allstations %>% 
    filter(between(LONGITUDE,
                   rockies@bbox[1,1], 
                   rockies@bbox[1,2]),
           
           between(LATITUDE,
                   rockies@bbox[2,1], 
                   rockies@bbox[2,2]),
           
           HYD_STATUS == "ACTIVE") 



# extract data from raster at station locations
# Typically would use catchment shape files
pet_stns <- raster::extract(pet_monthly,
                y = rockies_stns[ ,c("LONGITUDE","LATITUDE")]) %>% 
    
    as.data.frame() %>% 
    
    bind_cols(rockies_stns[ ,"STATION_NUMBER"]) %>% 
    
    setNames(c(year_mon %>% unique(), "stn")) %>% 
    
    tidyr::gather(-stn, key = "year_mon", value = "p_pet_mm") %>% 
    
    mutate(date_time = lubridate::ymd(paste0(year_mon, "-01"), tz = "MST"))


library(ggplot2)

pet_plot <- pet_stns %>% 
    ggplot(aes(x = date_time,
               y = p_pet_mm,
               color = stn)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    theme_bw() +
    labs(x = "Date", y = "P-pET (mm)", color = "Stn. #") +
    theme_pub(base_size = 18)


# pull Q data from tidyhydat DB:
library(tidyhydat)
hy_set_default_db(hydat_path = "D:/ext_data_R/Hydat.sqlite3")
# download_hydat(dl_hydat_here = "D:/ext_data_R/Hydat.sqlite3")

# hy_dir("D:/ext_data_R/Hydat.sqlite3")

rockies_q <- tidyhydat::hy_daily_flows(station_number = rockies_stns$STATION_NUMBER,
                                       start_date = "2000-01-01",
                                       end_date = "2015-12-31")

check_me <- rockies_q %>% 
    fasstr::screen_flow_data()

rockies_q_month <- rockies_q %>% 
    filter(Date >= "2010-01-01",
           Date <= "2011-12-31") %>% 
    
    # cleaning
    fasstr::fill_missing_dates() %>% 
    # add info
    fasstr::add_basin_area() %>% 
    
    # conversions 
    mutate(r_mm_day = Value / Basin_Area_sqkm * 86400 / 1e6 * 1e3) %>% 
    group_by(STATION_NUMBER,
             year_mon = format(Date, "%Y-%m")) %>% 
    
    # aggregate
    summarise(r_mm_month = sum(r_mm_day, na.rm = TRUE)) %>% 
    mutate(date_time = lubridate::ymd(paste0(year_mon, "-01"), tz = "MST"))
    


# plot R and P-pET

pet_r_plot <- pet_stns %>% 
    filter(stn %in% rockies_q_month$STATION_NUMBER) %>% 
    
    ggplot(aes(x = date_time,
               y = p_pet_mm)) +
    
    # add climate data
    geom_bar(stat = "identity",
             aes(fill = "P-pET"),
             alpha = 0.5) +
    
    # add R data
    geom_bar(inherit.aes = FALSE,
             data = rockies_q_month %>% 
                 rename(stn = STATION_NUMBER),
             aes(x = date_time,
                 y = r_mm_month,
                 fill = "R"),
             stat = "identity",
             alpha = 0.5) +
    
    
    geom_hline(yintercept = 0, linetype = 2) +
    
    coord_flip() +
    # geom_point() +
    # scale_color_viridis_d() +
    theme_bw() +
    labs(x = "Date", y = "Flux (mm/month)", fill = "Measure") +
    theme_pub(base_size = 18) +
    facet_wrap(~stn)



pet_r_plot






# choose a station
beaver_river <- rockies_stns %>% 
    filter(STATION_NUMBER == "08NB019")

beaver_q_stats <- fasstr::calc_daily_stats(rockies_q %>% 
                                               filter(STATION_NUMBER == beaver_river$STATION_NUMBER))


beaver_q_stats <- beaver_q_stats %>% 
    left_join(rockies_q %>% 
                  filter(STATION_NUMBER == beaver_river$STATION_NUMBER,
                         Date >= "2010-01-01",
                         Date < "2011-01-01") %>% 
                  mutate(DayofYear = lubridate::yday(Date)),
              by = "DayofYear")


## Not Run
# plot_daily_stats(station_number = beaver_river$STATION_NUMBER,
#                  start_year = 1990,
#                  end_year = 2015,
#                  log_discharge = TRUE,
#                  include_year = 2010,
#                  ignore_missing = TRUE)



# Set-up for interactive plot for P + R time series



# extract P data and make df
p_beaver <- raster::extract(rockies_stacks$prcp,
                                        y = beaver_river[ ,c("LONGITUDE","LATITUDE")]) %>% 
    as.data.frame() %>% 
    
    bind_cols(beaver_river %>% 
                  select(STATION_NUMBER, STATION_NAME)) %>% 
    
    setNames(c(seq(as.Date("2010-01-01"),
                 as.Date("2011-12-31"),
                 by = "1 day") %>% 
                   as.character(), 
             "stn",
             "name")) %>% 
    
    tidyr::gather(-stn, -name, key = "date", value = "p_mm_day") %>% 
    
    mutate(date_time = lubridate::ymd(date, tz = "MST"))


# quick and easy
library(plotly)
p_beaver_plot <- p_beaver %>% 
    ggplot(aes(x = date_time,
               y = p_mm_day)) +
    geom_bar(stat = "identity") +
    theme_pub()

plotly::ggplotly(p_beaver_plot)



# full interactive plot

precip_beaver <- p_beaver %>% 
    filter(date_time >= "2010-01-01",
           date_time < "2011-01-01") %>% 
    mutate(DayofYear = lubridate::yday(date_time)) %>% 
    
    # set up plot
    plot_ly(data = .,
            type = "bar", 
            name = "Precip (2010)") %>%
    add_bars(x = ~DayofYear,
             y = ~p_mm_day, 
             color = I("steelblue1")) %>% 
    layout(yaxis = list(title = "P (mm/day)"))

# 
q_beaver <- plot_ly(data = beaver_q_stats,
                    x = ~DayofYear) %>% 
    
    # Add max range
    add_ribbons(ymin = ~Minimum,
                ymax = ~Maximum,
                color = I("gray80"),
                name = "Max. Range (2000 - 2015)") %>% 
    
    # add percentiles
    add_ribbons(ymin = ~P5,
                ymax = ~P95,
                color = I("steelblue1"),
                name = "Q5-Q95 (2000 - 2015)") %>% 
    # add 20
    add_lines(y = ~Value,
              color = I("darkorange"),
              name = "Beaver River (2010)") %>% 
    layout(yaxis = list(title = "Q (cms/day)"))

subplot(precip_beaver, q_beaver, nrows = 2, shareX = TRUE, titleY = TRUE)


# 
# 
# 
# # for plains
# path_bp <- "./02_rhydro2019_presentation/03_get-clean-viz_files/data/plains"
# 
# params <- c("dayl", "tmin", "tmax", "prcp")
# params %>% 
#     purrr::walk(
#         
#         ~daymetr::download_daymet_ncss(location = c(boreal_plains@bbox[[4]],
#                                                     boreal_plains@bbox[[1]],
#                                                     boreal_plains@bbox[[2]],
#                                                     boreal_plains@bbox[[3]]), 
#                                        # top left to bottom right
#                                        start = 2010,
#                                        end = 2010,
#                                        param = .x,
#                                        frequency = "daily",
#                                        path = path_bp)
#     )
# 
# 
# get_ext <- function(nc_path){
#     nc <- ncdf4::nc_open(nc_path)
#     ymn <- ncdf4::ncatt_get(nc, varid = 0, attname = "geospatial_lat_min")$value
#     ymx <- ncdf4::ncatt_get(nc, varid = 0, attname = "geospatial_lat_max")$value
#     xmn <- ncdf4::ncatt_get(nc, varid = 0, attname = "geospatial_lon_min")$value
#     xmx <- ncdf4::ncatt_get(nc, varid = 0, attname = "geospatial_lon_max")$value
#     ext <- c(xmn, xmx, ymn, ymx)
#     ncdf4::nc_close(nc)
#     return(ext)
# }
# 
# ext_bp <- get_ext(list.files(path_bp, 
#           pattern = "prcp", 
#           full.names = TRUE)[1])
# 
# 
# prcp_stack_bp <- list.files(path_bp, 
#                          pattern = "prcp", 
#                          full.names = TRUE) %>% 
#     
#     raster::brick(varname = "prcp") %>%
#     # raster::`extent<-`(c(extent(r)@xmin,
#     #                      extent(r)@xmax,
#     #                      extent(r)@ymin,
#     #                      extent(r)@ymax)) %>% 
#     raster::`projection<-`(., proj4.Lambert) %>% 
#     # raster::projectRaster(crs = "+init=epsg:4326")
#     raster::projectRaster(crs = proj4.WGS) %>% 
#     raster::`extent<-`(ext_bp) 
# 
# plot(prcp_stack_bp)
# 
# raster::extract(prcp_stack_bp, y = matrix(c(-116., 56), ncol = 2))
# 
# 
# 
# 
