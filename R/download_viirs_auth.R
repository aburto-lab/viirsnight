
# Downloading night lights
#' It downloads the VIIRS image of night lights.
#'
#' @param year is the year from 2013 to 2021 that should be downloaded
#' @param destination_folder the destination folder where user wants to download image into
#'
#' @return downloads a tif compressed into a .gz file
#' @export
#'
#' @examples
#' download_viirs(2019)
#'
download_viirs_auth <- function(year, destination_folder) {

  if (missing(year)) {stop("A year from 2013 to 2021 must be selected")}
  if (!year %in% 2013:2021) {stop("A year from 2013 to 2021 must be selected")}
  if (missing(destination_folder)) {stop("No destination folder is selected")}

  dir.create(destination_folder)

  params <- list(
    client_id = 'eogdata_oidc',
    client_secret = '2677ad81-521b-4869-8480-6d05b9e57d48',
    username = keyring::key_get("viirs_username"),
    password = keyring::key_get("viirs_password"),
    grant_type = 'password'
  )

  if(year == 2013) {
    data_url <-
      paste0(
        "https://eogdata.mines.edu/nighttime_light/annual/v21/",
        year,
        "/VNL_v21_npp_",
        year,
        "_global_vcmcfg_c202205302300.average_masked.dat.tif.gz"
      )
  } else {
    data_url <-
      paste0(
        "https://eogdata.mines.edu/nighttime_light/annual/v21/",
        year,
        "/VNL_v21_npp_",
        year,
        "_global_vcmslcfg_c202205302300.average_masked.dat.tif.gz"
      )
  }

  token_url <-
    'https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token'
  response <- httr::POST(token_url, body = params, encode = "form")
  access_token_list <-
    jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  access_token <- access_token_list$access_token

  auth <- paste('Bearer', access_token)

  output_file <- paste0(destination_folder, "/", basename(data_url))
  options(timeout=36000)
  download.file(url = data_url,
                destfile = output_file,
                mode = "wb",
                headers = list(Authorization = auth))

}
