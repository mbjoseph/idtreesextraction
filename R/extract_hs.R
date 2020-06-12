#' Extract hyperspectral data for individual tree crowns
#' 
#' @export
extract_hs <- function(site, group) {
  itc_data <- load_itcs(site, group)
  
  data_dirs <- list.dirs(path = file.path("data", group), 
                         recursive = TRUE, full.names = TRUE)
  data_dirs <- data_dirs[!grepl(pattern = "task1", data_dirs)]
  dir <- grep("HSI", data_dirs, value = TRUE)
  
  hsi_files <- list.files(path = dir, 
                          pattern = paste0("^", site), 
                          full.names = TRUE)
  
  
  outdir <- tempdir()
  dir.create(outdir, showWarnings = FALSE)
  
  cl <- parallel::makeCluster(parallel::detectCores())
  parallel::clusterExport(varlist = c("itc_data", "outdir"), cl = cl)
  pbapply::pblapply(hsi_files, 
           FUN = function(file) {
             itc_subset <- sf::st_buffer(
               sf::st_crop(itc_data, 
                           raster::raster(file)), 1)
             if (nrow(itc_subset) == 0 | is.null(itc_subset)) {
               return(NA)
             }
             for (i in 1:nrow(itc_subset)) {
               outfile <- file.path(outdir, 
                                    paste0(itc_subset$indvdID[i], ".tif"))
               tmpout <- paste0(tempfile(), ".gpkg")
               sf::st_write(itc_subset[i, ], tmpout, delete_dsn = TRUE)
               system(
                 paste("gdalwarp -cutline", tmpout, 
                       "-crop_to_cutline", file, outfile))
             }
           }, 
           cl = cl)
  crown_tifs <- list.files(outdir, pattern = "*.tif", full.names = TRUE)
  
  vals <- pbapply::pblapply(crown_tifs, FUN = function(x) {
    raster::cellStats(raster::brick(x), "median")
  }, cl = cl)
  parallel::stopCluster(cl)
  
  crown_medians <- pbapply::pblapply(vals, function(x) {
    tibble::as_tibble(x) %>%
      dplyr::mutate(band = paste0("band_", 1:length(x))) %>%
      tidyr::pivot_wider(names_from = band, values_from = value)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(indvdID = gsub(".tif", "", x = basename(crown_tifs)))
  crown_medians
}
