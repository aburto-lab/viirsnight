#' Extracting lights data with a buffer shapefile
#'
#' The function will extract decompress the .gz file and extract light values within a buffer specified by the user.
#'
#' @param raster_file the raster of viirs dataset still compressed.
#' @param buffer the polygon buffer used to exract lights
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' extract_lights("data/raster_file.gz", buffer = sf::st_read("shp/buffer_to_extract.gpkg"))
#'
extract_lights <- function(raster_file, buffer) {
  R.utils::gunzip(raster_file, remove = FALSE, overwrite=TRUE)

  r <- raster::raster(stringr::str_remove_all(raster_file, ".gz"))

  lights <- as.data.frame(raster::extract(x=r, y=buffer, fun=sum))

  file.remove(stringr::str_remove_all(raster_file, ".gz"))
  gc()

  names(lights) <- "light_mean"

  lights$year <- sapply(stringr::str_extract_all(raster_file,"\\(?[0-9]+\\)?"), "[[", 2)
  print(lights)
}



