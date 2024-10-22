
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
download_viirs <- function(years, destination_folder, username, password) {

  if (missing(years)) {stop("At least one year from 2013 to 2021 must be selected")}
  if (any(!years %in% 2013:2021)) {stop("All years must be between 2013 and 2021")}
  if (missing(destination_folder)) {stop("No destination folder is selected")}
  if (!dir.exists(destination_folder)) {
    dir.create(destination_folder)
  }
  if (missing(username) || missing(password)) {
    stop("Username and password must be provided")
  }

  params <- list(
    client_id = 'eogdata_oidc',
    client_secret = '2677ad81-521b-4869-8480-6d05b9e57d48',
    username = username,
    password = password,
    grant_type = 'password'
  )

  token_url <- 'https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token'
  response <- httr::POST(token_url, body = params, encode = "form")
  access_token_list <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  access_token <- access_token_list$access_token
  auth <- paste('Bearer', access_token)

  options(timeout=36000)

  for (year in years) {
    if (year == 2013) {
      data_url <- paste0(
        "https://eogdata.mines.edu/nighttime_light/annual/v21/",
        year,
        "/VNL_v21_npp_",
        year,
        "_global_vcmcfg_c202205302300.average_masked.dat.tif.gz"
      )
    } else {
      data_url <- paste0(
        "https://eogdata.mines.edu/nighttime_light/annual/v21/",
        year,
        "/VNL_v21_npp_",
        year,
        "_global_vcmslcfg_c202205302300.average_masked.dat.tif.gz"
      )
    }

    output_file <- paste0(destination_folder, "/", basename(data_url))
    download.file(url = data_url,
                  destfile = output_file,
                  mode = "wb",
                  headers = list(Authorization = auth))
  }
}
