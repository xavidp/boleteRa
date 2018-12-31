# boleteRa
Some R scripts to get some visualization out of mushroom locations out of information collected through Android based devices with Zamiadroid ([Google Play](https://play.google.com/store/apps/details?id=uni.projecte&hl=ca), [Researchgate](https://www.researchgate.net/publication/262638211_ZAMIADROID_Captura_de_datos_biologicos_en_el_campo_con_dispositivos_moviles), [Website](http://biodiver.bio.ub.es/zamiaDroid/).)

Zamiadroid | Designing Project | Taking data | Viewing data
--- | --- | --- | --- 
Start screen in Zamiadroid on an Android powered-smartphone | Once you create a new project, you can define the fields you want to fill in and record for each citation | You can take as many geolocated citations (with picture/s)  as you wish | You can view your geolocated citations there from the smartphone itself
<img src=http://seeds4c.org/display564 width=300> | <img src=http://seeds4c.org/display567 width=180> | <img src=http://seeds4c.org/display563 width=250> | <img src=http://seeds4c.org/display569 width=250>

But then you might want to keep it elsewhere available online through a web browserm aggregating info from the citations you recorded across years & smartphones (& different peoplem eventually), while also adding extra info to the database such as elevation over the sea level, nearest city/village name, nearest meteorological station (in case you wonder how much did it rain in that area beforehand that day in which I did find so little - or so many - mushrooms in that location ), etc. 

All these data management ad enrichment is done in this project through R.

See some example graphs produced out of it.

Basic leaflet map with different base maps and different circle color and size depending on different criteria:

Road map | National Geographic | Satellite
--- | --- | ---
<img src=http://seeds4c.org/display576 width=300> | <img src=http://seeds4c.org/display575 width=300> | <img src=http://seeds4c.org/display577 width=300>

Example citation view using as a base map:

National Geographic | Satellite
--- | ---
<img src=http://seeds4c.org/display573 width=600> | <img src=http://seeds4c.org/display574 width=600> 

Some pivottable to query the dataset in realtime and make heatmap-like aggregation tables through the internet browser itself:


First attempt to use Crosstalkk package to display map and table in Sync (failed, so far ;-) 
<img src=http://seeds4c.org/display522>

If you want, comment, clone or fork and Enjoy! :-)
