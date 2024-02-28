##' @title NEON_download_soil_chemistry
##' @name  NEON_download_soil_chemistry
##' @author Dongchen Zhang
##' 
##' @param lat latitude.  
##' @param lon longitude.
##' @param start_date start date.
##' @param end_date end date.
##' @param store_dir path to where the downloaded files will be stored.
##' @details This function download and extract NEON soil chemistry from NEON API.
##' 
##' @return It returns a data frame consisting of datetime and value.
##' @author Dongchen Zhang
##' ##' @examples
##' \dontrun{
##' dat <- NEON_download_soil_chemistry(lat = 40.3, lon = -105.5, start_date = "2020-01-01", end_date = "2020-12-31", store_dir = "~")
##' }
NEON_download_soil_chemistry <- function(lon, lat, startdate, enddate, store_dir){
  packages <- c("neonstore", "swfscMisc", "neonUtilities", "purrr", "dplyr")
  packages.exist <- lapply(packages, require, character.only = TRUE) %>% unlist
  #install packages.
  for (i in seq_along(packages.exist)) {
    if (!packages.exist[i]) {
      install.packages(packages[i])
      require(packages[i])
    }
  }
  #grab NEON sites info.
  neonsites <- neonstore::neon_sites(api = "https://data.neonscience.org/api/v0", .token = Sys.getenv("NEON_TOKEN"))
  neonsites <- dplyr::select(neonsites, "siteCode", "siteLatitude", "siteLongitude") #select for relevant columns
  #Find site name from coordinates using distance.
  dis <- swfscMisc::distance(lat1 = lat, lon1 = lon, lat2 = neonsites$siteLatitude, lon2 = neonsites$siteLongitude)
  sitename <- neonsites$siteCode[which.min(dis)]
  #distinguish product ID with table and variable names.
  #soil chemistry.
  productID <- "DP1.10086.001"
  table.var <- list(sls_soilCoreCollection = c("soilTemp"),
                    sls_soilChemistry = c("nitrogenPercent", "organicCPercent"),
                    sls_soilMoisture = c("soilMoisture"),
                    sls_soilpH = c("soilInWaterpH"))
  table.name <- c("sls_soilCoreCollection",
                  "sls_soilChemistry",
                  "sls_soilMoisture",
                  "sls_soilpH")
  #download data.
  neonstore::neon_download(product = productID, 
                           dir = storedir, 
                           table = NA, 
                           site = sitename, 
                           start_date = start_date, 
                           end_date = end_date, 
                           type = "basic",
                           api = "https://data.neonscience.org/api/v0")
  #read outputs from variables of multiple tables.
  read.output <- table.name %>% 
    purrr::map(function(table){
      df <- neonstore::neon_read(table = table,
                                 product = productID, 
                                 start_date = start_date, 
                                 end_date = end_date, 
                                 dir = store_dir) %>% 
        dplyr::mutate(collectDate = as.character(as.Date(collectDate))) %>% 
        dplyr::select(c("collectDate", table.var[[table]]))
      #calculate daily average.
      aggregate(.~collectDate, df, mean, na.action = NULL, na.rm = TRUE)
    }) %>% purrr::set_names(table.name)
  #merge together
  total.dates <- read.output %>% 
    purrr::map(\(x)x$collectDate) %>% 
    unlist %>% 
    unique
  final.outputs <- data.frame(collectDate = total.dates)
  for (i in seq_along(read.output)) {
    final.outputs <- dplyr::left_join(final.outputs, read.output[[i]], by = "collectDate")
  }
  return(final.outputs)
}