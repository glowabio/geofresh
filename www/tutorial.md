## Step by step guide to GeoFRESH

### Upload your data

For this example, we will use a random subset of 100 occurrence points, drawn from the Harmonised freshwater fish occurrence and abundance data for 12 federal states in Germany, downloaded from [GBIF](https://www.gbif.org/dataset/e0908eee-ad49-4e91-b4d0-1f05dd17b291).

Point data coordinates need to be uploaded as a comma-separated table (.csv) with the three columns ID, longitude and latitude (column names are flexible). Longitude and latitude coordinates are required to be in the WGS84 coordinate reference system.

The points are instantly visualized on the map.

<img src="./img/upload_data_overview.png" alt="Upload data overview" align="center" width="95%"/><br/><br/>

The points can also be overlayed with the stream segment map.

![](img/point_stream_segm.png)

The uploaded table is also displayed and can be cross-checked and queried prior to the next steps.

<img src="./img/input_points_table.png" alt="Input points table" align="center" width="37%"/><br/><br/>

After the upload, the points need to be assigned to the corresponding sub-catchments and stream segments of the Hydrography90m dataset. This assignment, called "point snapping", moves the point to the closest stream segment.

<img src="./img//snapping_progress_bar.png" alt="Snapping progress bar" align="center" width="95%"/><br/><br/>

When the snapping is completed, the snapped points (in red) are shown on the map on top of the sub-catchments.

<img src="./img/snapped_points_zoom1.png" alt="Snapped points zoom1" align="center" width="95%"/><br/><br/>

If you zoom in, you can observe that each point has been moved to the closest location of the stream segment within the sub-catchment the point falls into. This is the default option for snapping.

<img src="./img/snapped_points_zoom2.png" alt="Snapped points zoom2" align="center" width="25%"/><br/><br/>

The new coordinates of the snapped points are also displayed in the table as additional columns.

<img src="./img/snapping_results_table.png" alt="Snapping results table" align="center" width="50%"/><br/><br/>

<!-- Υοu can choose between the type of snapping: defining a distance threshold (in meters) between the point and the stream segment (i.e., only stream segments close to points will be considered), or using flow accumulation in addition, i.e., the size of the upstream contributing area. Flow accumulation allows to specify whether the points should be snapped to small or large rivers. -->

<!-- In addition, the upstream catchments, i.e. the contributing drainage area of each point, are displayed as raster files on the map. You can thus cross-check if the point snapping was performed correctly, and if the catchments are those to be expected, or if another type of snapping may be preferred.  -->

<!-- ![](./img/upstream_catchment_map.png) -->

### Select environmental variables

Afterwards, you can annotate the point data with environmental information across the sub-catchment of each point. You can select from a suite of 48 variables related to [topography and hydrography](https://hydrography.org/hydrography90m/hydrography90m_layers'), 19 [climate variables](http://chelsa-climate.org/'), (i.e., current bioclimatic variables), 15 [soil](https://soilgrids.org/') variables and 22 [land cover](http://maps.elie.ucl.ac.be/CCI/viewer/index.php) variables.

<img src="./img/env_var_select.png" alt="Select environmental variables" align="center" width="95%"/><br/><br/>

For each selected environmental variable, you will receive within-sub-catchment summary statistics (mean, minimum, maximum, range, standard deviation) for each point location as a table.

<!-- ![](./img/env_var_table.png) -->

Additionally, the summary statistics (mean, min, max, sd) for the upstream catchment of each point for each of the selected environmental variables is calculated and displayed in a table.

<!-- ![](./img/env_var_table_upstream.png) -->

### Get routing info

In this panel, you can assess network distances among the uploaded points and receive a distance matrix for download.

### Download results as CSV

Finally, you can download the data as multiple comma-separated tables in a zip-file. After closing the browser window, all data is removed, meaning that data is not stored permanently on the portal.

### References

GBIF.org (24 April 2023) GBIF Occurrence Download <https://doi.org/10.15468/dl.xbuqe5>
