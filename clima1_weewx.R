#Get historical wheather data (rainfall, temp) through WU

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
if (!require("devtools")) install.packages("devtools"); library(devtools)
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load("tidyverse")
p_load("fs")
#p_load("readr", "dplyr", "tidyr")

dies_mes <- c(31,28,31,30,31,30,31,31,30,31,30,31)
rm(my.meteo)
my.meteo <- data.frame(
  c("BCN-SantGenis",      2, 2014, "ICATALUA116"),
  c("Berga-IBACELON1",    2, 2010, "IBACELON1"),
  c("Figueres",           2, 2018, "IFIGUERE4"),
  c("Gurb",               1, 2012, "http://elserrat.cat/weather/NOAA/"),
  c("Moia-IBARCELO58",    2, 2010, "IBARCELO58"),
  c("Moia-IMOI3",         2, 2014, "IMOI3"),
  c("MontBlanc-NE",       2, 2012, "IMONTBLA9"),
  c("MontSant-SW",        2, 2015, "ICATALUN125"),
  c("Montseny-Arbucies",  2, 2010, "ICATALUN15"),
  c("Montseny-ElBrull",   2, 2018, "IELBRULL2"),
  c("Montseny-StaMariaP", 2, 2018, "ISANTAMA1754"),
  c("SantBoi",            1, 2014, "http://jordicorbella.com/meteocam/weewx/NOAA/"),
  c("SantFeliuDeCodines", 2, 2014, "ICATALON10"),
  c("Solsona",            2, 2017, "ISOLSONA3"),
  c("TCN-PtaLaMora",      2, 2018, "ITLAMORA4"),
  c("Terrassa",           1, 2015, "https://meteo.ea3kz.com/NOAA/")
  )
my.meteo <- data.frame(t(my.meteo))
rownames(my.meteo) <- c(1:nrow(my.meteo))
colnames(my.meteo) <- c("name", "type", "any.ini", "url.base")
my.data <- list()
ii <- 0
for (n.meteo in 1:length(my.meteo.name)) {
  # Fixa el rang d'any en que hi ha dades per a cada estació (n.meteo)
  anys <- c(as.integer(as.character(my.meteo$any.ini[n.meteo])):2018)
  
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


