# Rnightlights: Nightlights for R

### ChangeLog
####v0.2.0
**IMPORTANT: Raster and data files as well as data column names from previous Rnightlights versions will be renamed as part of the upgrade to version 0.2.0**

1. There are two major changes in v0.2.0 which warranted the increment of the major package version:

* The change of the nlTypes naming convention to allow different time resolutions in the rasters. This mainly affects VIIRS which has daily, monthly and yearly data. The names reflect the time resolution e.g. VIIRS.D = daily, VIIRS.M = monthly and VIIRS.Y = yearly. OLS only has OLS.Y = yearly data but can easily be expanded to add other resolutions should they become available. This also has affected the naming conventions of raster and data files.
**NOTE 1: By the time of release (2018-04-23) only yearly rasters for 2015 are available**
**NOTE 2: DAILY Raster availability seems to be limited to the present and immediate past month only**

* The addition of admLevels as a parameter to the getCtryNlData() and processNlData functions allowing one to specify the admin level(s) at which to perform calculations. Previously all calculations were performed at the lowest country level and then would have to be aggregated to the required admin level. This has also affected the naming of data files and columns.

    As such an upgradeRnightlights function has been added to convert existing data and files. It is run automatically on installation. In case of any errors in converting files the data and rasters will not be available in the new version. Manual intervention may be required where files may have been manually altered. The package will continue to work otherwise. In case of any problems please raise an issue on the [package github page](https://github.com/chrisvwn/Rnightlights) or send an email to [chris.njuguna@gmail.com](mailto:chris.njuguna@gmail.com).

2. Added package function testing. Currently test coverage is about 15%. This will be updated to get as close to 100% coverage as possible.

####v0.1.5
* Returned the VIIRS range to start in "201204" as it seems to have been a temporary problem.
* Updated file extraction from downloaded tile tgz. Raster data has been reduced to 100 significant digits reducing tile sizes and filename changed. Raster filename now ends in "rade9h.tgz"" rather than "rade9.tgz"
* Minor updates to the exploreData() shiny interface 
* Changed the gdal calculation of zonal stats to use ff and ffbase packages to store large raster data to disk allowing accurate calculation of all stats
* Changed the order of parameters in getCtryNlData and processNlData for consistency

####v0.1.4
* Updated the package code and documentation to reflect a change in available VIIRS data. The earliest available downloadable data is changed to "201401" from "201204".

####v0.1.3:
* Fixed a bug in calculation of stats using "gdal" where functions apart from mean, sum and var would fail.
* Re-introduction of the raster layer in the exploreData map display.
* Various minor code and documentation updates.

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

####v0.1.1:
Initial release