data <- data.frame (lat = c(-32.922778,-32.921596,-32.921062,-32.919904,-32.918786),
                    long = c(-68.847785,-68.847513,-68.847316,-68.847043,-68.846816),
                    sort_field = c(1,2,3,4,5))

test <- points_to_line (data, long = "long", lat = "lat", 
                        id_field = NULL, sort_field = "sort_field")

 

data2 <- data.frame (lat = c(-32.922778,-32.921596,-32.921062,-32.919904,-32.918786,
                            -32.914315,-32.914362,-32.914262,-32.914208),
                    long = c(-68.847785,-68.847513,-68.847316,-68.847043,-68.846816,
                             -68.856012, -68.855111, -68.853812, -68.852847),
                    id_field = c("line1","line1","line1","line1","line1",
                                 "line2","line2","line2","line2"),
                    sort_field = c(1,2,3,4,5,1,2,3,4))
test2 <- points_to_line (data2, long = "long", lat = "lat", 
                        id_field = "id_field", sort_field = "sort_field")

