# ---The example is for the Metropolitan Area of Mendoza in Argentina. - - -


# -----------------------------------------------------------------------------
# --------------------------- ALTERNATIVE_TRAJECTORIES FUNCTION EXAMPLE ----------------------------- 
# Variable
# Latitude, Longitude of the origin and destination points.
# 01. Latitude and longitude of the origin. 
origin <-"-32.79679,-68.816" 
# 02. Latitude and longitude of the destination 
dest <- "-32.90212,-68.761" 
# 03. Transport
mode <- "car"
# 04. Date and hour of departure to destination
hour_trajectory <- "2019-08-01 15:50:00"
# 05. Check and set the time zone
Sys.setenv(TZ = "America/Argentina/Buenos_Aires")
# 06. Local path where the shapefiles are located. Example dataset
dir <-system.file("data", package = "AirExposure") 
#To run the function you must be located in the local path where the shapefiles are.
setwd(dir) 
# 07. Set the time zone. If the study time zone is different from the time zone of the computer, this is an important parameter #to configure
Sys.setenv(TZ = "America/Argentina/Buenos_Aires") 
# 08. Departure date-time
hour_time <- "2019-08-01 07:50:00"
# 09. Field name of the grid ID.
gridID <- "GRI1_ID"
# 10. Field name of the concentration value.
shapeValue <- "value"
# 11. Measurement units
units <- "μg/m3"
# 12. The pollutant name 
pollutant <- "PM2.5"

# -- Run the function
# Dataframe test 
test_df <- alternative_trajectories (origin=origin,dest=dest,mode=mode,dir=dir,
                                   key=key,output="df",hour=hour_time,gridID=gridID, shapeValue=shapeValue, units=units, pollutant=pollutant)

write.csv(test_df, "file_test_df.csv") 
# Plot test
test_plot <- alternative_trajectories (origin=origin,dest=dest,mode=mode,dir=dir,
                                       key=key,output="plot",hour=hour_time,gridID=gridID, shapeValue=shapeValue, units=units, pollutant=pollutant)
htmlwidgets::saveWidget(file_test_plot, "test_plot.html")

# Polyline test
test_poly<- alternative_trajectories (origin=origin,dest=dest,mode=mode,dir=dir,
                                      key=key,output="polyline",hour=hour_time,gridID=gridID, shapeValue=shapeValue, units=units, pollutant=pollutant)
plot(test_poly$geometry)
st_write(test_poly, "file_test_poly.shp")

