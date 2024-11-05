rm(list=ls())


library(rgee)
library(tidytable)

ee_Initialize()

#ee_manage_cancel_all_running_task()

image_collection <- "ECMWF/ERA5_LAND/DAILY_AGGR" #"ECMWF/ERA5_LAND/HOURLY" #"ECMWF/ERA5_LAND/DAILY_AGGR"

satellite_scale <- 11132



parameter <- c("temperature_2m", "total_precipitation_sum", "surface_pressure", "dewpoint_temperature_2m", "surface_solar_radiation_downwards_sum", "u_component_of_wind_10m", "v_component_of_wind_10m") 
#parameter <- c("temperature_2m", "surface_net_solar_radiation",  "total_precipitation", "surface_net_thermal_radiation", "surface_solar_radiation_downwards", "surface_thermal_radiation_downwards")


shape_collection <- "projects/ee-j49150822/assets/NorthAmerica"
#shape_collection <- "projects/ee-lorenzoparigi/assets/JDS_2023_central_point"


GEE_folder_name <- "GEE"
prefix <- "ERA5_data_"
continent <- "NorthAmerica"


start_date <- as.Date("2002-01-01")
end_date <- as.Date("2023-01-01")

time_interval <- "2 months"

date_list <- strftime(unique(c(seq.Date(start_date, end_date, by = time_interval), as.Date(end_date))), format = "%Y-%m-%d")

check_bands_number <- FALSE

iteration_steps <- 1:(length(date_list)-1)

for (i in iteration_steps) {
    
    file_name <- paste0(prefix, continent, "_part_", i)
    
    initial_date <- as.character(date_list[i])
    final_date <- as.character(date_list[i+1])
    
    print(paste0("Part ", i, " of ", max(iteration_steps), " Date: ",final_date))
    
    pretreatment_image_collection <- ee$ImageCollection(image_collection)$filter(ee$Filter$date(initial_date, final_date))$filterBounds(ee$FeatureCollection(shape_collection))
    
    if (check_bands_number) {
        
        number_of_images <- pretreatment_image_collection %>%
            ee$ImageCollection$size() %>%
            ee$Number$getInfo()*length(parameter)
        
        if (number_of_images >= 5000) {
            stop(paste0("Execution stopped at iteration: ", i))
        }
        
    }
    
    
    
    risultati <- pretreatment_image_collection$select(parameter) %>%
        ee$ImageCollection$toBands() %>%
        ee$Image$reduceRegions(
            collection = shape_collection,
            reducer = ee$Reducer$mean(),
            scale = satellite_scale
        )
    
    
    task_vector <- ee_table_to_drive(
        collection = risultati,
        description = file_name,
        folder = GEE_folder_name,
        fileFormat = 'CSV',
        timePrefix = FALSE
    )
    
    task_vector$start()
    
    
}


####ERA5 file aggregation####
#folder_path <- "D:/PhD/ERA5/"
folder_path <- "G:/My Drive/GEE/"

#parameter <- c("temperature_2m", "surface_net_solar_radiation", "total_precipitation")


prefix <- "ERA5_data_"
continent <- "NorthAmerica"

file_parts <- 1:126

file_list <- paste0(folder_path, prefix, continent,  "_part_", file_parts, ".csv")


complete_file <- lapply(file_list, function(x){
    print(x)
    res <- fread(x) %>%
        pivot_longer(cols = ends_with(parameter)) %>%
        #na.omit() %>%
        mutate(date = substr(name, 1, 8) %>% as.Date(., format = "%Y%m%d"),
               parameter = substr(name, 10, 50)) %>%
        select(Lake_ID, date, parameter, value) %>%
        pivot_wider(names_from = parameter,
                    values_from = value) 
}) %>%
    bind_rows() 

fwrite(complete_file,  paste0("G:/My Drive/GEE/out/ERA5_time_series_", continent, ".csv"))
