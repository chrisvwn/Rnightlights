# Rnightlights

## Nightlights for R

### ChangeLog

v0.1.2:
+ Fixed a problem that was causing processNlData() and getCtryNlData() to return all NAs
+ Changed the naming system of downloaded and renamed VIIRS tiles to remove the underscore between year and month thus making it nlPeriod uniform with the OLS tiles. Also added a dummy tileName to the end of the renamed OLS tile to make it uniform with VIIRS tiles. Thus the format is now "nlType_nlPeriod_tileName.tif". This fixes a problem with listCtryNlTiles() which was causing it to crash.
+ Divided area by 10e6 to yield square kilometers
