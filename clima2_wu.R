#Get historical wheather data (rainfall, temp) through WU

# WU - Weather Underground
# ----------------------------------
# https://www.wunderground.com/history/
# They offer historical weather data for many weather stations, also in SPain. Not all of them but just a few have rainfall data, apparently (in most cases of stations monthly rainall data shows zero mm of rainfall when it did rain indeed. Sabadaell, Moia, Tona, Cerdanyola, etc. A weather station that seems to carry rainfal data effectively is "Barcelona Airport El Prat", shown also when selecting Sant Cugat, for insstance, as wheater station )
#
# rwunderground: R Interface to Weather Underground API
# 
# Tools for getting historical weather information and forecasts from wunderground.com. Historical weather and forecast data includes, but is not limited to, temperature, humidity, windchill, wind speed, dew point, heat index. Additionally, the weather underground weather API also includes information on sunrise/sunset, tidal conditions, satellite/webcam imagery, weather alerts, hurricane alerts and historical high/low temperatures.
# https://cran.r-project.org/web/packages/rwunderground/index.html
#library(pacman)
#p_load("rwunderground")

#Interesting functions: geolookup
# geolookup(set_location(territory = "ES", city = "Moia"))
#>Please enter your weather underground API key and press enter:
#>Error: Invalid key!
#
# They require an API key, and distontinued the API Key free service in 2018
# https://apicommunity.wunderground.com/weatherapi/topics/end-of-service-for-the-weather-underground-api
# ---
#
# Therefore, web scraping will be the workaround to get some of their valuable data
if (FALSE) {
  
  anys <- c(2014:2018)
  mesos <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  wu.url <- data.frame()
  for (any in 1:length(anys)) {
    for (mes in mesos[1]:mesos[length(mesos)]) {
      wu.url[mes, any] <-
        paste0("https://www.wunderground.com/history/monthly/es/sant-cugat-del-vall%C3%A8s/LEBL/date/", anys[any], "-", mesos[mes], "?cm_ven=localwx_history")
    }
  }
  wu.url
  p_load("rvest", "dplyr")
  require(methods)
  require(rvest)
  my.rda.file <- "last.wu.data.Rda"
  if (file.exists(my.rda.file)) {
    load(file=my.rda.file)
    wu.data.all.previous <- wu.data.all
  } else {
    wu.data.all.previous <- NULL
  }
  url_base <- wu.url
  
  webpage <- list()
  html <- list()
  ii <- 0
  if (exists("wu.data")) rm(wu.data); wu.data <- list()
  for (any in 1:length(anys)) {
    for (mes in mesos[1]:mesos[length(mesos)]) {
      # download html files
      any<-1
      mes<-1
      ii <- ii+1
      html[[ii]] <- read_html(wu.url[mes, any])
      webpage[[ii]] <- list(mes=mes,
                            any=any,
                            html=html[[ii]])
      
      # the data we want is in the first table on this page
      # the html_table() command coerces the data into a data frame
      
      webpage[[ii]]$html %>%
        html_nodes("table.days") 
      
      # Fetch data
      wu.data[[ii]]$html <- webpage[[ii]] %>%
        html_nodes("table") %>%
        .[[1]] %>%
        html_table() 
      
      # tsting
      html_nodes(
        html_nodes(
          html_nodes(webpage[[ii]], ".views-field-field-documento")[-1],
          ".file"),
        "a")
      
    }
    
  }
}

