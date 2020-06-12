#' Crop RGB data to individual tree crowns
#' 
#' @importFrom magrittr %>%
#' @export
crop_rgb <- function(site, group) {
  crowns <- load_itcs(site, group)
  
  data_dirs <- list.dirs(path = here::here("data", group), 
                         recursive = TRUE, full.names = TRUE)
  data_dirs <- data_dirs[!grepl(pattern = "task1", data_dirs)]
  rgb_dir <- grep("RGB", data_dirs, value = TRUE)
  
  dir.create(here::here("out"), showWarnings = FALSE)
  
  # create a mosaic
  system(paste0(
    "gdalbuildvrt out/rgb_mosaic.vrt ", 
    rgb_dir, "/", site, "_*.tif"
  ))
  system(
    "gdal_translate -of GTiff -co \"TILED=YES\" out/rgb_mosaic.vrt out/rgb_mosaic.tif"
  )
  rgb <- raster::stack("out/rgb_mosaic.tif")
  

  # Iterate over crowns, save each chip -------------------------------------
  outdir <- here::here("out", group, site, "rgb_chips")
  dir.create(outdir, 
             showWarnings = FALSE, recursive = TRUE)
  for (i in 1:nrow(crowns)) {
    outpath <- here::here("out", group, site, "rgb_chips", 
                          paste0(crowns$indvdID[i], ".tif"))
    chip <- raster::crop(rgb, crowns[i, ], snap = "out")
    if (file.exists(outpath)) {
      unlink(outpath)
    }
    raster::writeRaster(chip, outpath)
    system(
      paste0("gdal_translate -of JPEG -scale -co worldfile=yes ", 
             outpath, 
             " ", 
             gsub("\\.tif", ".jpg", outpath))
    )
    unlink(outpath)
  }
  
  list.files(path = ".", pattern = "jpg.aux.xml$", recursive = TRUE) %>%
    unlink
  list.files(path = ".", pattern = ".wld$", recursive = TRUE) %>%
    unlink
}
