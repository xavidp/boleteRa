#Get historical wheather data (rainfall, temp) through WU
if (!require("devtools")) install.packages("devtools"); library(devtools)
if (!require("pacman")) install.packages("pacman"); library(pacman)

# Meteoland - Landscape Meteorology Tools
# Functions to estimate weather variables at any position of a landscape [De Caceres et al. (2018) <doi:10.1016/j.envsoft.2018.08.003>].
# https://cran.r-project.org/package=meteoland
# https://cran.r-project.org/web/packages/meteoland/vignettes/UserGuide.html  
# ----------------------------------
# Load packages
p_load(meteoland, tidyverse, tidyr, dplyr, sf, lubridate, fs, tibbletime, janitor, scales)

# ------------------------------------------
# Add a base map
# ------------------------------------------
p_load(GADMTools); dir.create("dades/mapes/")
gadm.esp <- gadm_sf_loadCountries(fileNames="ESP", level=4, basefile="dades/mapes/")
gadm.bcn <- gadm_subset(gadm.esp, regions="Barcelona")
bbox_cat_xy <- c(0.0652144217437066, 40.5150150670591, 3.33555501686516, 42.8839369882321)

# Definim els límit del mapa de Catalunya
x <- c(0.0652144217437066, 3.33555501686516)
y <- c(40.5150150670591, 42.8839369882321)
bbox_cat <- rbind(x, y); 
colnames(bbox_cat) <- c("min", "max")

p_load(ggmap)
# Ampliem el requadre per mostrar mapa de fons
bb = t(apply(bbox_cat, 1, bbexpand, .04))
bb

# Definim el mapa de fons
bgMap = get_map(as.vector(bb), source = "osm",  maptype="roadmap", force=T) 
# WGS84 for background map. 
# Tweak get_map() with source= or maptype=
#ggmap(bgMap)
# BBox of Catalonia
# westBoundLongitude= 0.0652144217437066
# eastBoundLongitude= 3.33555501686516
# southBoundLatitude= 40.5150150670591
# northBoundLatitude= 42.8839369882321

# ------------------------------------------
# get data from smc
# ------------------------------------------
smc.stations <- downloadSMCstationlist(api=Sys.getenv("r_smc_api_key"), date=as.Date("2019-07-06"))
smc.sl <- smc.stations
smc.sl.sp <- smc.sl
smc.sl.sf <- st_as_sf(smc.sl.sp) %>% 
  mutate(elev_range=case_when(
    ceiling(elevation) %in% 0:299 ~ "a.0_300m",
    ceiling(elevation) %in% 300:599 ~ "b.300_600m",
    ceiling(elevation) %in% 600:899 ~ "c.600_900m",
    ceiling(elevation) %in% 900:1199 ~ "d.900_1200m",
    ceiling(elevation) %in% 1200:1499 ~ "e.1200_1500m",
    ceiling(elevation) %in% 1500:1799 ~ "f.1500_1800m",
    ceiling(elevation) %in% 1800:4000 ~ "g.1800m_..."
  )) %>% 
  mutate(col=case_when(
    ceiling(elevation) %in% 0:299 ~ alpha("green", 0.4), # For an alpha of 0.4, i. e. an opacity of 40%.)
    ceiling(elevation) %in% 300:599 ~ alpha("orange", 0.4),
    ceiling(elevation) %in% 600:899 ~ alpha("blue", 0.4),
    ceiling(elevation) %in% 900:1199 ~ alpha("pink", 0.4),
    ceiling(elevation) %in% 1200:1499 ~ alpha("lightgreen", 0.4),
    ceiling(elevation) %in% 1500:1799 ~ alpha("yellow", 0.4),
    ceiling(elevation) %in% 1800:4000 ~ alpha("brown", 0.4)
  ))

tabyl(smc.sl.sf$elev_range) %>% adorn_totals()
tabyl(smc.sl.sf$province) %>% adorn_totals()
table(smc.sl.sf$province, smc.sl.sf$elev_range) %>% data.frame() %>% arrange(Var1) %>% adorn_totals()
tabyl(smc.sl.sf$county)

# Map with all stations in Catalonia from today's rainfall from SMC datasets
smc.sl.sf.3857 <- st_transform(smc.sl.sf, st_crs(3857))
plot(smc.sl.sf.3857["elev_range"], bgMap = bgMap, pch = 16, cex = 1.5, axes=TRUE, col = smc.sl.sf.3857["col"]) 
plot(smc.sl.sf.3857["elev_range"], bgMap = bgMap, pch = 16, cex = 1.5, axes=TRUE) 
warnings()
smc.meta <- downloadSMCvarmetadata(api=Sys.getenv("r_smc_api_key"))
smc.today <-downloadSMCcurrentday(api=Sys.getenv("r_smc_api_key"))
smc.sl.id <- row.names(smc.today)
plot(smc.today)
# ------------------------------------------
# Fetch Historical Data
# ------------------------------------------
# Display progress bar
for (yy in 2018:2018) {
  p <- progress_estimated(length(smc.sl.id))
  for (ss in 1:length(smc.sl.id)){
    cat(paste0("Loop item: ", ss, ", station: ", smc.sl.id[ss]))
    downloadSMChistorical(api=Sys.getenv("r_smc_api_key"), 
                            dates = seq(from=as.Date(paste0(yy, "-01-01")), to=as.Date(paste0(yy, "-12-31")), by=1),
                            station_id = smc.sl.id[ss],
                            export=T,
                            exportDir=file.path("precipitacio", "_smc", yy),
                            exportFormat="meteoland/txt"
    )
    #rename station list file name to the name of this station to prevent getting overwritten by the next station
    file.rename(file.path("precipitacio", "_smc", yy, "MP.txt"), 
                file.path("precipitacio", "_smc", yy, paste0("MP_", smc.sl.id[ss], ".txt"))
    )
    p$tick()$print()
    cat("\n")
  }
}

# ------------------------------------------
# get data from aemet
# ------------------------------------------
sl.sp <- downloadAEMEThistoricalstationlist(Sys.getenv("r_aemet_token"))
# display all stations with pacakge sp
plot(sl.sp)

# convert sp object to sf (Simple features)
sl.sf <- st_as_sf(sl.sp)
# now you can use tidyverse::dplyr commands on the sf object, such as filter, et all
sl.sf.ct.all <- sl.sf %>% 
  filter(province == "BARCELONA" | province == "GIRONA" | province == "LLEIDA" | province == "TARRAGONA") 

write_csv(sl.sf.ct.all, path=file.path("precipitacio", "_aemet", yy, "MP_ct_all.csv"))

sl.sf.ct <- sl.sf.ct.all %>% 
  # Some stations where removed because downloading data from some years fail and break period loop to download data
  filter(ID != "9771C") %>% # Lleida
  filter(ID != "9987P") %>% # St Jaume d'Enveja
  filter(ID != "0255B") %>% # Santa Susanna
  filter(ID != "0367")  # Girona aeroport

# ------------------------------------------
# Write data to disk
# ------------------------------------------
write_csv(sl.sf.ct, path=file.path("precipitacio", "_aemet", yy, "MP_ct_subset.csv"))

# and then you can plot the sf object against some of the variables
plot(sl.sf.ct["elevation"])

# ------------------------------------------
# Get STation IDs
# ------------------------------------------
# Now we download some rainfall data from aemet stations from catalonia
# ID list from those stations:
sl.ct.all.ids <- sl.sf.ct.all$ID
sl.ct.ids <- sl.sf.ct$ID

# ------------------------------------------
# Get Historical Data - TODAY
# ------------------------------------------
sl.today <- downloadAEMETcurrentday(Sys.getenv("r_aemet_token"), daily = TRUE, verbose = TRUE)
sl.today.sf <- st_as_sf(sl.today)
sl.today.sf.ct <- sl.today.sf %>% 
  tibble::rownames_to_column() %>% 
  filter(rowname %in% sl.ct.all.ids)
plot(sl.today.sf.ct["Precipitation"])

# Map with all stations in Catalonia from today's rainfall from AEMET datasets
sl.today.sf.ct.3857 <- st_transform(sl.today.sf.ct, st_crs(3857))
plot(sl.today.sf.ct.3857["Precipitation"], bgMap = bgMap, pch = 16, cex = 1.5, axes=TRUE)
# ------------------------------------------

# ------------------------------------------
# Fetch Historical Data
# ------------------------------------------
# Display progress bar
for (yy in 2019:2019) {
  p <- progress_estimated(length(sl.ct.all.ids))
  for (ss in 1:length(sl.ct.all.ids)){
    cat(paste0("Loop item: ", ss, ", station: ", sl.ct.all.ids[ss]))
    downloadAEMEThistorical(Sys.getenv("r_aemet_token"), 
                            dates = seq(from=as.Date(paste0(yy, "-01-01")), to=as.Date(paste0(yy, "-12-31")), by=1),
                            station_id = sl.ct.all.ids[ss],
                            export=T,
                            exportDir=file.path("precipitacio", "_aemet", yy),
                            exportFormat="meteoland/txt"
    )
    #rename station list file name to the name of this station to prevent getting overwritten by the next station
    file.rename(file.path("precipitacio", "_aemet", yy, "MP.txt"), 
                file.path("precipitacio", "_aemet", yy, paste0("MP_", sl.ct.all.ids[ss], ".txt"))
    )
    p$tick()$print()
    cat("\n")
  }
}

# ------------------------------------------
# Massage data to aggregate records in a single Data frame
# ------------------------------------------
# Read all meteo data in a R to end up producing a single data.frame with all data from all stations
# File list (fl)
fl <- fs::dir_ls(path=file.path("precipitacio", "_aemet", yy), type="file",  regexp="/-?\\d+[a-zA-Z].txt$")
#glob="*.txt",

list.data <- list()
for (f in fl) {
  # Llegim l'arxiu i el desem en data.frame dins un element d'una llista
  list.data[[f]] <- rio::import(fl[f], skip=1)
}

# Convert list of n df into one single df
d <- bind_rows(list.data, .id = "column_label")
colnames(d) <- c("Station", "Date", "Precipitation", "MeanTemperature", "MinTemperature", "MaxTemperature", "WindDirection", "WindSpeed", "SunshineHours")
d$Station <- gsub("precipitacio/aemet/2019/", "", d$Station, fixed=T)
d$Station <- gsub(".txt", "", d$Station, fixed=T)


# ------------------------------------------
# Cumulative rainfall sum in rolling windows
# ------------------------------------------
rollsum_5 <- tibbletime::rollify(sum, window = 5)
d.sum <- d %>%
  select(Station, Date, Precipitation) %>% 
  mutate(Date = ymd(Date)) %>% 
  group_by(Station, Date) %>%
  summarise(Precipitation = sum(Precipitation)) %>%
  mutate(cumsum = rollsum_5(Precipitation)) %>%
  ungroup %>%
  filter(Precipitation != 0) %>% 
  filter(cumsum > 40) %>% 
  filter(Date >= today() - months(2))


# ------------------------------------------
# Attach side info from these stations through inner join
# ------------------------------------------
d.sum.extra <- inner_join(d.sum, sl.sf.ct.all, by=c("Station"="ID"))
d.sum.extra

# ------------------------------------------
# Map of stations with higher rainfall than threshold over period of interest
# ------------------------------------------
#plot(d.sum.extra["geometry"])
cat.sf <- st_transform(d.sum.extra$geometry, st_crs(3857))
#plot(cat.sf, pch = 13, cex = 1.5, col="red", axes=TRUE)
plot(cat.sf, bgMap = bgMap, pch = 16, cex = 1.5, col="red", axes=TRUE)

# ------------------------------------------
# Map with all stations in Catalonia from AEMET
# ------------------------------------------
sl.sf.ct.all.3857 <- st_transform(sl.sf.ct.all["elevation"], st_crs(3857))
plot(sl.sf.ct.all.3857, bgMap = bgMap, pch = 16, cex = 1.5, col="orange", axes=TRUE)

# ============================================================================
# WeeWX - Open source software for your weather station. 
# http://www.weewx.com/stations.html
# ----------------------------------
# WeeWX is a free, open source, software program, written in Python, which interacts with your weather station to produce graphs, reports, and HTML pages. It can optionally publish to weather sites or web servers. Thousands of stations throughout the world run weeWX, many of whom have opted-in to be shown on our station map. 
#
# Estacions interessants amb dades de pluja
#
# TERRASSA, estación W8681 de EA3KZ. Estación Meteorológica en Terrassa. Latitud: 41° 34.62' N Longitud: 002° 00.98' E Altura: 300 metros [JN11AN]. La estación meteorológica en uso es una W-8681 conectada a un pequeño ordenador Raspberry pi con el software weewx twitter @ea3kz_wx
# Estació Meteorològica sense fils W8681 PROII. Preu: 220.22 € [transport gratuït, només Espanya península] - https://www.astroradio.com/ca/514040
# https://meteo.ea3kz.com/history.html
# https://meteo.ea3kz.com/NOAA/NOAA-2018-08.txt
# https://meteo.ea3kz.com/NOAA/NOAA-2018-09.txt
# https://meteo.ea3kz.com/NOAA/NOAA-2018-10.txt
# https://meteo.ea3kz.com/NOAA/NOAA-2018-11.txt
# https://meteo.ea3kz.com/NOAA/NOAA-2018-12.txt
p_load("tidyverse")
p_load("fs")
#p_load("readr", "dplyr", "tidyr")

dies_mes <- c(31,28,31,30,31,30,31,31,30,31,30,31)
rm(my.meteo)
my.meteo <- data.frame(
  c("BCN-SantGenis",      2, 2014, "ICATALUA116", "41.42694444", "2.13638889"),
  c("Berga-IBACELON1",    2, 2010, "IBACELON1", "42.09916667", "1.84583333"),
  c("Figueres",           2, 2018, "IFIGUERE4", "42.25972222", "2.96444444"),
  c("Gurb",               1, 2012, "http://elserrat.cat/weather/NOAA/", "41.97888889", "2.24111111"),
  c("Moia-IBARCELO58",    2, 2010, "IBARCELO58", "41.81361111", "2.10083333"),
  c("Moia-IMOI3",         2, 2014, "IMOI3", "41.81277778", "2.09083333"),
  c("MontBlanc-NE",       2, 2012, "IMONTBLA9", "41.37222222", "1.16083333"),
  c("MontSant-SW",        2, 2015, "ICATALUN125", "41.21694444", "0.72861111"),
  c("Montseny-Arbucies",  2, 2010, "ICATALUN15", "41.82361111", "2.47027778"),
  c("Montseny-ElBrull",   2, 2018, "IELBRULL2", "41.81916667", "2.27722222"),
  c("Montseny-StaMariaP", 2, 2018, "ISANTAMA1754", "41.70000000", "2.45000000"),
  c("SantBoi",            1, 2014, "http://jordicorbella.com/meteocam/weewx/NOAA/", "41.35611111", "2.04388889"),
  c("SantFeliuDeCodines", 2, 2014, "ICATALON10", "41.68444444", "2.16305556"),
  c("Solsona",            2, 2017, "ISOLSONA3", "41.98111111", "1.54361111"),
  c("TCN-PtaLaMora",      2, 2018, "ITLAMORA4", "41.13861111", "1.34916667"),
  c("Terrassa",           1, 2015, "https://meteo.ea3kz.com/NOAA/", "41.58388889", "2.02722222")
  )
my.meteo <- data.frame(t(my.meteo))
rownames(my.meteo) <- c(1:nrow(my.meteo))
colnames(my.meteo) <- c("name", "type", "any.ini", "url.base", "lat", "lon")
my.meteo <- my.meteo %>%
  mutate(id=as.integer(rownames(my.meteo)))
my.data <- list()
ii <- 0
for (n.meteo in 1:length(my.meteo$name)) {
  # Fixa el rang d'any en que hi ha dades per a cada estació (n.meteo)
  anys <- c(as.integer(as.character(my.meteo$any.ini[n.meteo])):2019)
  
 for (any in 1:length(anys)) {
  for (mes in mesos[1]:mesos[length(mesos)]) {
    ii <- ii+1
    my.file.name <- c(paste0("NOAA-", anys[any], "-", mesos[mes], ".txt"), # for Meteo Type 1 - WeeWX
                      paste0("WU-", anys[any], "-", mesos[mes], ".txt")) # for Meteo Type 2 - WU
    my.file.relpath <- file.path("precipitacio", as.character(my.meteo$name[n.meteo]))
    my.file.relname <- file.path(my.file.relpath,
                                 paste0(as.character(my.meteo$name[n.meteo]), "-", my.file.name[as.integer(my.meteo$type[n.meteo])])
                              )
    # FEs el directori de l'estacio meteorològica si encara no hi és
    dir_create(my.file.relpath)
    # si no tenim l'arxiu de dades per a un mes concret, descarrega'l
    if (!file.exists(my.file.relname)) {
      if (as.integer(my.meteo$type[n.meteo]) == 1) {
        # URL type 1: WeeWX
        download.file(paste0(as.character(my.meteo$url.base[n.meteo]), 
                             my.file.name[as.integer(my.meteo$type[n.meteo])]),
                      my.file.relname
        )
      } else if (as.integer(my.meteo$type[n.meteo]) == 2) {
      # URL Type 2: WU Historial Text (Sant Genis BCN, etc)
      # https://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID=ICATALUA116&day=29&month=12&year=2018&format=1&graphspan=month
        # URL type 2: WU
        download.file(paste0("https://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID=",
                             as.character(my.meteo$url.base[n.meteo]),
                             "&format=1&graphspan=month&month=", mesos[mes], "&year=", anys[any]),
                      my.file.relname
        )
      }
    }
    read.fwf.files <- FALSE
    if (read.fwf.files) {
      my.data[[ii]] <- read_fwf(my.file.relname,
                                fwf_cols("DAY"=3, 
                                         "MEAN_TEMP"=7, 
                                         "HIGH"=7, 
                                         "H_TIME"=7, 
                                         "LOW"=7, 
                                         "L_TIME"=7, 
                                         "HEAT_DEG_DAYS"=7, 
                                         "COOL_DEG_DAYS"=7,
                                         "RAIN_mm"=7,
                                         "AVG_WIND_SPEED_kph"=7,
                                         "HIGH_WIND"=7,
                                         "TIME_WIND"=7,
                                         "DOM_DIR"=7
                                ),
                                skip = 13, 
                                n_max = dies_mes[mes],
                                na="-",
                                col_types=NULL
      )
      if (mes!=1 & anys[any]!=2015 & as.character(my.meteo$name[n.meteo])!="Terrassa") {
        stop_for_problems(my.data[[ii]])
        problems(my.data[[ii]])
      }
      
      my.data[[ii]] <- my.data[[ii]] %>%
        mutate(year=anys[any],
               month=mesos[mes],
               site=as.character(my.meteo$name[n.meteo])
        )
    } # end of check to run read.fwf.files

  } # end of months loop
 } # end of years loop
} # end of meeto location loop

# El Serrat del Serí, Gurb, Catalunya
# ELEV: 525 metres    LAT: 41-58.44 N    LONG: 002-14.28 E
# http://elserrat.cat/weather/history.html
# http://elserrat.cat/weather/NOAA/NOAA-2018-08.txt
# http://elserrat.cat/weather/NOAA/NOAA-2018-09.txt
# http://elserrat.cat/weather/NOAA/NOAA-2018-10.txt
# http://elserrat.cat/weather/NOAA/NOAA-2018-11.txt
# http://elserrat.cat/weather/NOAA/NOAA-2018-12.txt


# Get historical wheather data (rainfall, temp) - potential alternative ways}
# ============================================================================
# AWEKAS (AUstria)
# https://www.awekas.at/
# AWEKAS is an abbrevation for “Automatic WEather map (german: KArten) System”. It is a system that processes indicated values of private weather stations graphically, generates weather maps and evaluates the data.
# Estació de tona, en gràfics, no taula de dades. :-(
# https://www.awekas.at/en/instrument.php?id=10147#day

# Open Weather Map
# https://openweathermap.org/price
# ----------------------------------
# 10 USD for 6 year of historical wheater data for a given city
# https://openweathermap.org/history
# https://openweathermap.org/history-bulk


# Agromonitoring 
# ----------------------------------
# See 
# https://agromonitoring.com/api/history-weather
# https://agromonitoring.com/api/accumulated-precipitation
# However historical rainfall is for paid subscription palnms only
# https://agromonitoring.com/price

# Worldbank
# ---------------------------------
# http://sdwebx.worldbank.org/climateportal/index.cfm?page=downscaled_data_download&menu=historical
# They allow downloading some excel file from any country with temp or rainfal from several periods, last one available by the time of this writing is 1991-2015 (better than nothing for my use case :-) 