
getNuts3Areas <-  function() {
    nuts.data <- data_frame(NUTS.Code=nuts3.spdf@data$id,id=row.names(nuts3.spdf@data))
    polygon_data <- data_frame(id=sapply(slot(nuts3.spdf, "polygons"), slot, "ID"),
                              Shape_Area= sapply(slot(nuts3.spdf, "polygons"), slot, "area"))
    nuts.area <- left_join(nuts.data,polygon_data)
    nuts.area
}


