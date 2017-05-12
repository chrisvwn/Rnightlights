# shiny_nightlights

## Nightlights for R

### A three-in-one project

1. Hopefully will end up as a CRAN package. This will allow R users to play around with nightlights. 

	a. For those who just want the nightlight measurements, one has the option of downloading pre-processed data related to country
boundaries to the lowest administration level.

	b. There is also the option to process the nightlights from scratch involving downloading the tiles, cropping, masking and extracting
data. This can be done in the background without user involvement.

	c. There is finally the option to use the individual functions to download, crop mask and do whatever else one wants with all the 
component user functions which are usually internally used but can be used by anyone.

2. Will also have a shiny web app for EDA and maybe more e.g. user data uploads, model building, 
saving of environments and models, and allowing sharing of urls that will rebuild the environment

3. For the pre-processed nightlights there will be a repo system that holds all the data. One central server will be the master and will 
also have a directory of all other repos. Anyone can register a repo. Registered repos will do simple syncs to pull data from the master.
However, one can build their own repo for whatever reason from the R package.

A step farther ... if this is successful and there is sufficient interest (and time) expand this to other satellite data products!
