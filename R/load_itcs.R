#' Load individual tree crown data
#' 
#' @export
load_itcs <- function(site, group) {
  pattern <- paste0(group, "_", site, ".shp")
  path_to_file <- list.files(path = here::here("."), pattern = pattern, 
                             recursive = TRUE, full.names = TRUE)
  stopifnot(length(path_to_file) == 1)
  itc_data <- sf::st_read(path_to_file)
  itc_data
}
