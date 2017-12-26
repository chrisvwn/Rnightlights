# Rnightlights

## Nightlights for R

### ChangeLog
####v0.1.1:
Initial release

####v0.1.2:
* Fixed a problem that was causing processNlData() and getCtryNlData() to return all NAs.
* Changed the naming system of downloaded and renamed VIIRS tiles to remove the underscore between year and month thus making it nlPeriod uniform with the OLS tiles. Also added a dummy tileName to the end of the renamed OLS tile to make it uniform with VIIRS tiles. Thus the format is now "nlType_nlPeriod_tileName.tif/tgz". This fixes a problem with listCtryNlTiles() which was causing it to crash.
* Divided area by 1e6 to convert square meters to square kilometers.
* Exported getCtryRasterOutputFname to allow exploreData to access clipped country rasters.
* Modified exploreData to embed the country raster in the map.
* Exported nlPeriod functions to list and check validity of nlPeriods.
* General updates in shiny app.
* Fixed raster display in maps. Still large maps will be slow.
* Made stats radio buttons dynamic.
* Fixed OLS tile download where multiple URLs were returned. Picking only first. May be prudent to download both and process returning average of the two.
* Added missing pkgOption numCores for parallel processing when raster pkgOptions are selected.

####v0.1.3:
* Fixed a bug in calculation of stats using "gdal" where functions apart from mean, sum and var would fail.
* Re-introduction of the raster layer in the exploreData map display.
* Various minor code and documentation updates.

####v0.1.4
* Updated the package code and documentation to reflect a change in available VIIRS data. The earliest available downloadable data is changed to "201401" from "201204".

####v0.1.5
* Returned the VIIRS range to start in "201204" as it seems to have been a temporary problem.
* Updated file extraction from downloaded tile tgz. Raster data has been reduced to 100 significant digits reducing tile sizes. Raster filename is now rade9h.tgz rather than rade9.tgz