[Details](#Details) 
/ [Topics](#Topics) 
/ [Slides](#Slides) 
/ [Check](#Checks) 
/ [Resources](#Resources) 
/ [R at EGU2019](#r-at-egu2019)
<!--- / [Citation](#Citation)  --->

# Using R in Hydrology - EGU2019 Short Course

Conveners: Alexander Hurley, Louise Slater, Lucy Barker, Guillaume Thirel, Claudia Vitolo, Ilaria Prosdocimi


## Details 
- **Where?** Monday 08 April, 16:15-18:00 in Room -2.16 (*basement!*); 
- [Using R in Hydrology (link to session)](https://meetingorganizer.copernicus.org/EGU2019/session/30963 "Link to EGU Session Description")
- **What?** This session is aimed at hydrologists who are interested in hearing more about R as well as those who are advanced R programmers wanting to discuss recent developments in an open environment. 
- The session is organised in cooperation with the [Young Hydrologic Society (YHS)](https://younghs.com/ "Young Hydrologic Society website") .
- Participants are invited to post and discuss questions in the [Hydrology in R Facebook group](https://www.facebook.com/groups/1130214777123909/ "link to Hydro-R Facebook group")

## Topics:
The detailed and time-tabled list of contributions, tutorials and discussion points will be here as soon it's ready.

## Slides
All materials to reproduce the slides and analyses are available on this repository.
To get everything (slides, data, code, etc.) onto your local machine, we recommend to [download the whole github course repository](https://codeload.github.com/hydrosoc/rhydro_EGU19/zip/master). Individual presentations (*.html* or PDF files) can be downloaded from the [presentations folder](./presentations) and viewed in a regular web browser.

Links to individual presentations (for direct view in browser) are below:


- [Course intro and getting to grips with R in Hydrology](https://hydrosoc.github.io/rhydro_EGU19/presentations/01_02_intro_getting_to_grips/IPgetGrips.html) // A. Hurley, I. Prosdocimi
- [Obtaining, cleaning and visualizing 
hydrological data with R](https://hydrosoc.github.io/rhydro_EGU19/presentations/03_get_clean_viz/03_get-clean-viz.html#1) // A. Hurley 
- [Introduction to parallel and high performance computing for hydrologists](https://hydrosoc.github.io/rhydro_EGU19/presentations/04_hpc_computing/L.SlaterParallelPres.html) // L. Slater
- [Staying up-to date:
automating tasks from downloading data to reporting](https://hydrosoc.github.io/rhydro_EGU19/presentations/06_task_automation/06_task_automation.html) // A. Hurley
- [Using R Shiny to visualise and share your data: A UK drought story](https://hydrosoc.github.io/rhydro_EGU19/presentations/07_drought_app/LucyShinyPresentation_v2/lucy_shiny_v2.html) // L. Barker
- [Modelling the hydrological cycle in snow-dominated catchments](https://hydrosoc.github.io/rhydro_EGU19/presentations/08_snow_hydrology/snow_Thirel.html) // G. Thirel
- [Community-led initiatives: get involved!](https://hydrosoc.github.io/rhydro_EGU19/presentations/09_10_community_farewell/CV.html) // C. Vitolo, A. Hurley

## Checks
We will provide any relevant information you require to follow along during the course in due time. This includes software (versions) and any other materials. Stay tuned!  
**Update**: We've chosen a presentation-style delivery of the course, rather than a hands-on workshop. You are more than welcome to follow along in the raw  (if provided) or rendered versions of any materials. However, some operations, such as downloading and handling rasters, take multiple minutes. If you do want to execute code provided in the raw documents, we advise to give it a try in advance of the course, and ask questions where/if necessary during the session. Required packages are highlighted where necessary.
<!--- - To follow along, participants may wish to run the following code before the session: 
  install.packages(c("rnrfa", "lfstat", "osmdata", "tidyverse", "sf", "leaflet", "ncdf4","lubridate", "ggplot2", "raster", "rgdal", "airGRteaching", "airGR"))
- For the netCDF presentation, sample gridded data for 2015-06 can be downloaded directly by clicking <a href="https://catalogue.ceh.ac.uk/datastore/eidchub/b745e7b1-626c-4ccc-ac27-56582e77b900/chess_precip_201506.nc" rel="nofollow">here</a> and the catchment shapefile can be downloaded from <a href="http://nrfa.ceh.ac.uk/data/station/spatial_download/12001" rel="nofollow">here</a> (see acknowledgments in presentation).                
--->


## Resources:
- [CRAN Hydrology TaskView](https://cran.r-project.org/web/views/Hydrology.html "Hydrology TaskView on CRAN")
- <a href="https://odelaigue.github.io/airGR/" rel="nofollow">airGR</a> - a description of the airGR package (IRSTEA GR Hydrological Models)
- <a href="http://abouthydrology.blogspot.co.uk/2012/08/r-resources-for-hydrologists.html" rel="nofollow">R-Resources for Hydrology</a> - a detailed list of R resources for hydrology (slightly outdated now)
- <a href="https://journal.r-project.org/archive/2016/RJ-2016-036/RJ-2016-036.pdf" rel="nofollow">rnrfa</a> - an R package to interact with the UK National River Flow Archive ([GitHub repo](https://github.com/cvitolo/rnrfa))
- <a href="https://ropensci.github.io/hddtools/" rel="nofollow">hddtools</a> - an R package to facilitate access to a variety of online open data sources for hydrologists

## R at EGU2019

We recommend the PICO presentation [Using R in Hydrology: recent developments and future directions](https://meetingorganizer.copernicus.org/EGU2019/EGU2019-2823-2.pdf) delivered by Louise Slater in the session [Innovative methods to facilitate open science and data analysis in hydrology - from data collection in challenging environments to data sharing, visualization and modelling](https://meetingorganizer.copernicus.org/EGU2019/picos/31717). This presentation is based on our [discussion paper in HESS](https://www.hydrol-earth-syst-sci-discuss.net/hess-2019-50/) with the same title.

There are a number of other short courses using R that may be of interest to you:

- [Building Bayesian Spatial Models to predict landslides by using R-INLA (co-organized) ](https://meetingorganizer.copernicus.org/EGU2019/session/30928)
- [End-member modelling analysis of grain-size data in R (co-organized) ](https://meetingorganizer.copernicus.org/EGU2019/session/30964)
- [Geocomputation with R (co-organized) ](https://meetingorganizer.copernicus.org/EGU2019/session/31033)
- [Writing and maintaining R packages (co-organized) ](https://meetingorganizer.copernicus.org/EGU2019/session/31034)



<!--- ## Citation
Please refer to this course as:
* Louise Slater, Claudia Vitolo, Shaun Harrigan, Tobias Gauster, Guillaume Thirel, & Alexander Hurley. (2018, April). Using R in Hydrology at EGU2018 (Version 1.0.1). Zenodo. http://doi.org/10.5281/zenodo.2554009
**BibTeX**
```
@misc{rhydro_EGU2018, title={Using R in Hydrology at EGU2018}, DOI={10.5281/zenodo.2554009}, abstractNote={<p>This repository contains all contributions to the <a href="https://meetingorganizer.copernicus.org/EGU2018/session/28914">short course</a> delivered at EGU 2018. Materials to this and other short courses can be found on the <a href="https://github.com/hydrosoc">YHS GitHub repository.</a></p>}, publisher={Zenodo}, author={Louise Slater and Claudia Vitolo and Shaun Harrigan and Tobias Gauster and Guillaume Thirel and Alexander Hurley}, year={2018}, month={Apr}}
```
--->
