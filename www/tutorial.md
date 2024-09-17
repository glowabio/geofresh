## Step by step guide to GeoFRESH

### Upload your data

This tutorial walks though the single steps of GeoFRESH using the test data set (random selection of fish occurrences, drawn from the Harmonised freshwater fish occurrence and abundance data for 12 federal states in Germany, downloaded from [GBIF](https://www.gbif.org/dataset/e0908eee-ad49-4e91-b4d0-1f05dd17b291).

Point data coordinates need to be uploaded as a comma-separated table (.csv) with the three columns ID, latitude and longitude (column names are flexible). Latitude and longitude coordinates are required to be in the WGS84 coordinate reference system.

The points are instantly visualized on the map.

<img src="./img/Fig_01_upload_map.jpeg" alt="Upload data overview" align="center" width="95%"/><br/><br/>

You can select different backround layers from the right-hand side, including the stream segment map. The uploaded table is also displayed and can be cross-checked and queried prior to the next steps.


### Snap coordinates to the stream network

After the upload, the points need to be assigned to the corresponding sub-catchments and stream segments of the Hydrography90m dataset. This assignment, called "point snapping", moves the point to the closest stream segment.

<img src="./img/Fig_02_snap.jpeg" alt="Snapping progress bar" align="center" width="95%"/><br/><br/>

When the snapping is completed, the snapped points (yellow icons) are shown on the map. If you zoom in, you can observe that each point has been moved to the closest location of the stream segment within the sub-catchment the point falls into. This is the default option for snapping. 

<img src="./img/Fig_03_map_snap.jpeg" alt="Snpped points on map" align="center" width="95%"/><br/><br/>


The new coordinates of the snapped points are also displayed in the table as additional columns.

<img src="./img/Fig_02b_snapped_table.jpeg" alt="Table overview" align="center" width="95%"/><br/><br/>



<!-- Υοu can choose between the type of snapping: defining a distance threshold (in meters) between the point and the stream segment (i.e., only stream segments close to points will be considered), or using flow accumulation in addition, i.e., the size of the upstream contributing area. Flow accumulation allows to specify whether the points should be snapped to small or large rivers. -->

<!-- In addition, the upstream catchments, i.e. the contributing drainage area of each point, are displayed as raster files on the map. You can thus cross-check if the point snapping was performed correctly, and if the catchments are those to be expected, or if another type of snapping may be preferred.  -->

<!-- ![](./img/upstream_catchment_map.png) -->

### Select environmental variables

Afterwards, you can annotate the point data with environmental information across the sub-catchment of each point. You can select from a suite of 48 variables related to [topography and hydrography](https://hydrography.org/hydrography90m/hydrography90m_layers'), 19 [climate variables](http://chelsa-climate.org/'), (i.e., current bioclimatic variables), 15 [soil](https://soilgrids.org/') variables and 22 [land cover](http://maps.elie.ucl.ac.be/CCI/viewer/index.php) variables.

<img src="./img/Fig_04_select_variables.jpeg" alt="Select environmental variables" align="center" width="95%"/><br/><br/>


### Extract __local__ environmental information

Click on "Start query" on the bottom to initiate the computation. For each selected environmental variable, you will receive __local, i.e., ., within-sub-catchment__ summary statistics (mean, minimum, maximum, range, standard deviation) for each point location as a table.

<img src="./img/Fig_05_local.jpeg" alt="Select local environmental variables" align="center" width="95%"/><br/><br/>



<!-- ![](./img/env_var_table.png) -->


### Extract __upstream__ environmental information

Additionally, you can obtain the summary statistics (mean, min, max, sd) for the __upstream catchment__ of each point for each of the selected environmental variables is calculated and displayed in a table. Again, click on "Start query":

<img src="./img/Fig_05_upstream.jpeg" alt="Select upstream environmental variables" align="center" width="95%"/><br/><br/>
<!-- ![](./img/env_var_table_upstream.png) -->

Finally, you can __download the data__ as multiple comma-separated tables in a zip-file by clicking on ""Download ZIP". 


### Plot data
After obtaining the local and / or upstream environemntal information, you can visualize the results in a histogram and box-plots (for categorical land-cover data). Move the slider to change the number of bins in the histogram:

<img src="./img/Fig_07_plots.jpeg" alt="Plot results" align="center" width="95%"/><br/><br/>


After closing the browser window, all data is removed, meaning that no data is stored permanently on the platform.


### References

GBIF.org (24 April 2023) GBIF Occurrence Download <https://doi.org/10.15468/dl.xbuqe5>

<!-- 
### Get routing info

In this panel, you can assess network distances among the uploaded points and receive a distance matrix for download.



### Download results as CSV
-->
