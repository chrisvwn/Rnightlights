# Rnightlights: Nightlights for R

### ChangeLog
#### v0.2.5.0
##### *Bug Fixes*
This version does not fix any bugs.

##### *New features*
This version introduces a few new features:

- **`Sample visualization functions`** This version has implemented vaious visualization functions for plotting graphs, maps, clusters and time-series data. These are just samples and may serve as a basis for custom visualizations. 

- **`Authentication for tile downloads`** This version implements authentication for the download of raster tiles from the download site after this became a requirement. Users must register themselves and then provide username and password to the package in order to access the tile downloads. The package will store the credentials so they need only be entered once. The notice for this change is [here](https://payneinstitute.mines.edu/eog-2/transition-to-secured-data-access/).

- **`Background/gas-flare removal`** Previously only the DMSP-OLS gasflare polygons were available for gas-flare removal. Currently, the package includes background masking using the VIIRS yearly tiles (currently 2015/2016). The plan is to also add DSMP-OLS raster masking using the selected year. Any feedback on the gasflare and background removal methods employed in the package are welcome.

- **`New tile download site`** The tile download sites for both VIIRS and OLS have been moved to the [Payne Institute at the Colorado School of Mines](https://payneinstitute.mines.edu/eog/). Users will now need to register and provide auth credentials ot the package to access the new data location (see below).

- **`nlStat repository`** To ensure the integrity of the data processed we need to store the nlStat functions that are actually used. The package stores the function name, signature (arguments supplied), a hash of the function and the function body to ensure that it can distinguish between different functions even if they have similar names. Calls to the same function with different arguments will result in a new data process. Finally, the function that was used can be retrieved in case the user needs it. However, function dependencies are not stored so if any functionality is called externally it will not be available. This feature is still very buggy.

- **`Credits`** A function has been added to print out credits to the various data sources. It is set to print when the package is loaded. It can be suppressed by wrapping a call to `library()` with the `suppressPackageStartupMessages()` function.

- **`shiny app GUI`** The shiny app previously called `exploreData()` is now called `gui()`. It has been revamped to not only view data but to be able to visually access the main functionality of the package i.e. select `country`, `admLevel`, `nlType`, `nlPeriod` and `nlStat` and process the data. The visualizations will only work for nlStats that have single vector values.

- **`Package data upgrade disabled`** The upgrade of the data directory and contents from previous versions of the package will probably fail. The upgrade functions need extensive work to bring the data up to date. Until this is fixed, the package will not update your data to the latest version. Please re-run your scripts to re-process the data. The old and new data should be able to live happily side-by-side but you may want to back up your previous data just in case.

- **`Package unit tests failing`** The package unit tests have not (never) been fully done and at the moment some will fail. Hence, the build badge will show failing. However, the package should build fine.

#### v0.2.4
##### *Bug Fixes*
This version does not fix any bugs. A workaround for a missing layer in at least one shapefile (IDN) is introduced by adding a parameter *gadmPolyType*. More on that in the *New features* section below.

##### *New features*
This version introduces a few new features:

- **`ignoreMissing=FALSE is default`** Up to now, `ignoreMissing=FALSE` had to be specified to tell `getCtryNlData` to process missing nlStats. This is now the default and does not need to be specified. To suppress processing of data for example to retrieve only stats that have already been calculated use `ignoreMissing=TRUE` or to return NULL if any nlStats have not already been calculated set `ignoreMissing=NULL`

- **`default nlStats`** the default nlStats used by `getCtryNlData` and `processNlData` as specified by `pkgOptions("nlStats")` has been altered to introduce the parameter `na.rm=TRUE` for both `sum` and `mean`.

- **`Enhanced nlStat custom function data`** Custom nlStat functions can now receive apart from pixel values also colrow (column and row) and lonlat (longitude and latitude) values. The colrow values are in reference to the whole country/custom polygon. Conceptually, this should allow for more complex spatial aware custom functions in addition to the normal aggregations. In addition to the ability to return multiple values this may add the power of functionality. Higher order functions may require multiple passes which right now requires a good understanding of the package and some manual custom function chaining. Maybe in future the ability to perform multiple passes/function chaining passing the output of functions to higher order functions will be added. An example is given to show how these values might be useful.

- **`mergeTileStrategy`** is added to specify how to deal with nlPeriods where there are more than one tile available. Currently this only happens with OLS.Y where there are tiles from 2 satellites at different points. Before the introduction of this the package just used the first tile. The default now is to merge the tiles averaging pixel values. One can choose to use the `first` tile, or `last` tile only or if there are more than 2 to select the indices to merge. **Due to this change, getCtryNlData parameter order changes. This is to compel users to actually make the change manually noting the new defaults**.

- **`mergeTileFun`** adds the idea that apart from merging by taking the mean of pixels you can choose a different way to merge them by specifying the function to use. This is not in use currently.

- **`removeGasFlares`** (finally) adds the ability to apply gasflare removal for areas that are known to have gasflares. The gasflare polygons used are from NOAA, however, they may be dated (created 2009). If you might know of a more recent gasflare polygon dataset please do let me know. This also creates a `gasflares` folder under the dataPath.

- **`gadmPolyType`** is added as a parameter to getCtryNlData give the option to allow the download one of the various formats of polygon available on GADM i.e.:

    + gpkgZip (gpkg zip),
    + kmlZip (KML zip),
    + shpZip (ESRI shapefile zip),
    + spRDS (spatial polygons saved to RDS),
    + sfRds (simple features saved to RDS)

  Only shpZip and spRds are currently implemented. Previously only shapefile zips were possible and this change was necessitated by [issue #22](https://github.com/chrisvwn/Rnightlights/issues/22) where a missing polygon layer was detected in the shpZip for Indonesia (IDN). The spRds offering was not affected by this problem. So in a sense this is a workaround.

- **`configName`** is added as a parameter to getCtryNlData allowing one to select which raster to use out of the available rasters per nlType. They provide the rasters with different pre-processing applied to them. [See](https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html and https://ngdc.noaa.gov/eog/gcv4_readme.txt) for details.
    They are:
    
  + `configName_VIIRS.D`: The regex to uniquely identify the tile file to use out
      of the downloaded tile .tgz. The version 1 monthly series is run globally
      using two different configurations:

        + vcmcfg: excludes any data impacted by stray light.
        + vcmsl: includes these data if the radiance values have undergone the 
          stray-light correction procedure and will have more data coverage toward
          the poles, but will be of reduced quality.

  + `configName_VIIRS.M`: The regex to uniquely
      identify the tile file to use out of the downloaded monthly .tgz
      tile. 

        + Has the same options as configName.VIIRS.D
  
  + `configName_VIIRS.Y`: The regex to uniquely identify the tile file to
      use out of the downloaded tile .tgz. The annual products can have
      other values for the config shortname (Field 5). They are:

        + vcm-orm (VIIRS Cloud Mask - Outlier Removed): This product
            contains cloud-free average radiance values that have undergone
            an outlier removal process to filter out fires and other ephemeral
            lights.
        + vcm-orm-ntl (VIIRS Cloud Mask - Outlier Removed - Nighttime Lights):
            This product contains the "vcm-orm" average, with background 
            (non-lights) set to zero.
        + vcm-ntl (VIIRS Cloud Mask - Nighttime Lights): This product contains
            the "vcm" average, with background (non-lights) set to zero.
          
  + `configName_OLS.Y`: The annual versions are only made with the "vcm"
  version, excluding any data impacted by stray light.
      The options for OLS.Y are:

        + cf_cvg (Cloud-free coverages): This product tallies the total 
            number of observations that went into each 30 arc second grid cell. This
            image can be used to identify areas with low numbers of observations
            where the quality is reduced. In some years there are areas with zero
            cloud-free observations in certain locations.

        + avg_vis (Raw avg_vis): contains the average of the 
            visible band digital number values with no further filtering. Data
            values range from 0-63. Areas with zero cloud-free observations are
            represented by the value 255.

        + stable_lights (cleaned up avg_vis): contains the lights from cities,
            towns, and other sites with persistent lighting, including gas flares. 
            Ephemeral events, such as fires have been discarded. Then the background
            noise was identified and replaced with values of zero. Data values
            range from 1-63. Areas with zero cloud-free observations are represented
            by the value 255.

#### v0.2.3
This version fixes some bugs and sees the advent of a few new features:

##### *Bugs *
- A bug in `getCtryNlData` caused `admLevel=country` to error out [issue #10](https://github.com/chrisvwn/Rnightlights/issues/10). This is fixed.
- A possible change in the download policy of GADM causes `downloadMethod=aria` to fail for polygon downloads. This version forces polygon downloads to use normal download methods.
- A bug was found in the extraction of `VIIRS.Y` raster tiles [issue #12](https://github.com/chrisvwn/Rnightlights/issues/12). This was fixed.
- A change in GADM downloads that only allows https connections is made.

##### *New features*
- This version introduces the possibility to have `nlStat` functions return multi-value outputs. Prior to this each function could only return a scalar per (sub-)polygon/zone.
- On trial but incomplete is the addition of spatial-aware data to functions. Previous versions have only supplied functions with a vector of radiance values that fall within a particular sub-polygon. This version now supplies a dataframe with added columns `col`, `row`, `lon` and `lat` in addition to the `zone` (when `pkgOption (extractMethod='gdal')` is set) and `val` cols. To use the colrow values pass a function to nlStat with parameters col, row and vals e.g. `fn1 <- function(col, row, vals)`. Similarly, for lonlat use `function(lon, lat, vals)`.
- Set the default GADM version to the latest GADM version i.e. `3.6`
- functions can now return multiple values per sub-polygon/zone
- one can now specify arguments to pass to `nlStats` functions
- related to the arguments feature above, `na.rm=TRUE` is no longer assumed
- there is now an option to decide which raster in the raster tile tar.gz downloaded from NOAA to use. More info [here](https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html)

#### v0.2.2
This version adds the possibility to use custom polygon shapefiles other than the GADM shapefiles. The use of custom shapefiles is incomplete and still extremely buggy though and will probably be overhauled in future versions which should decouple the requirement for country code, etc when extracting data. This should allow for passing in extents, or coordinates and a radius instead of a polygon. This version also fixes some bugs with admLevels which rendered getCtryNlData unusable.

#### v0.2.1
This is a bugfix version. Some bugs with the `getCtryNlData` function and specifically with the `admLevel` parameter. The bugfixes fix the case where the country polygons have not been downloaded yet so that it will force a download from GADM. The polygons are required to search for admLevels but this happens before getCtryNlData downloads polygons. A few updates to the documentation as well to clarify that we no longer calculate only at the lowest level but at the specified admLevel.

#### v0.2.0
**IMPORTANT: Raster and data files as well as data column names from previous Rnightlights versions will be renamed as part of the upgrade to version 0.2.0**

1. There are two major changes in v0.2.0 which warranted the increment of the major package version:

* The change of the nlTypes naming convention to allow different time resolutions in the rasters. This mainly affects VIIRS which has daily, monthly and yearly data. The names reflect the time resolution e.g. VIIRS.D = daily, VIIRS.M = monthly and VIIRS.Y = yearly. OLS only has OLS.Y = yearly data but can easily be expanded to add other resolutions should they become available. This also has affected the naming conventions of raster and data files.
**NOTE 1: By the time of release (2018-04-23) only yearly rasters for 2015 are available**
**NOTE 2: DAILY Raster availability seems to be limited to the present and immediate past month only**

* The addition of admLevels as a parameter to the getCtryNlData() and processNlData functions allowing one to specify the admin level(s) at which to perform calculations. Previously all calculations were performed at the lowest country level and then would have to be aggregated to the required admin level. This has also affected the naming of data files and columns.

    As such an upgradeRnightlights function has been added to convert existing data and files. It is run automatically on installation. In case of any errors in converting files the data and rasters will not be available in the new version. Manual intervention may be required where files may have been manually altered. The package will continue to work otherwise. In case of any problems please raise an issue on the [package github page](https://github.com/chrisvwn/Rnightlights) or send an email to [chris.njuguna@gmail.com](mailto:chris.njuguna@gmail.com).

2. Added package function testing. Currently test coverage is about 15%. This will be updated to get as close to 100% coverage as possible.

#### v0.1.5
* Returned the VIIRS range to start in "201204" as it seems to have been a temporary problem.
* Updated file extraction from downloaded tile tgz. Raster data has been reduced to 100 significant digits reducing tile sizes and filename changed. Raster filename now ends in "rade9h.tgz"" rather than "rade9.tgz"
* Minor updates to the exploreData() shiny interface 
* Changed the gdal calculation of zonal stats to use ff and ffbase packages to store large raster data to disk allowing accurate calculation of all stats
* Changed the order of parameters in getCtryNlData and processNlData for consistency

#### v0.1.4
* Updated the package code and documentation to reflect a change in available VIIRS data. The earliest available downloadable data is changed to "201401" from "201204".

#### v0.1.3:
* Fixed a bug in calculation of stats using "gdal" where functions apart from mean, sum and var would fail.
* Re-introduction of the raster layer in the exploreData map display.
* Various minor code and documentation updates.

#### v0.1.2:
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

#### v0.1.1:
Initial release