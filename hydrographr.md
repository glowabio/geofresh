## R package _hydrographr_

The R package [__hydrographr__](https://glowabio.github.io/hydrographr/) provides 
a collection of R function wrappers for GDAL and GRASS-GIS functions to efficiently work with the newly created 
[Hydrography90m dataset](https://hydrography.org/hydrography90m/hydrography90m_layers/) 
and spatial biodiversity data. The easy-to-use functions process large raster 
and vector data directly on disk in parallel, such that the memory of R does not 
get overloaded. This allows creating scalable data processing and analysis 
workflows in R, even though the data is not processed directly in R. Below is 
a list of the functions that are implemented in _hydrographr_ while additional functions 
are currently being developed.

The package was described in a publication in _Methods in Ecology and Evolution_
([doi:10.1111/2041-210X.14226](https://doi.org/10.1111/2041-210X.14226)). 
A detailed explanation and examples can be found on its website 
https://glowabio.github.io/hydrographr/, and its source code is openly available 
on [GitHub](https://github.com/glowabio/hydrographr/).

__Development team__:  Afroditi Grigoropoulou, Marlene Schürz, Sami Domisch, 
Jaime García Márquez, Yusdiel Torres-Cambas, Thomas Tomiczek, Merret Buurman, 
Christoph Schürz, Vanessa Bremerich <br/>
__Contact information__:  sami.domisch@igb-berlin.de <br/>
__Bug reports and feature requests__: https://github.com/glowabio/hydrographr/issues


__Project funding__: [NFDI4Biodiversity (DFG)](https://nfdi4biodiversity.org), [
NFDI4Earth (DFG)](https://www.nfdi4earth.de)

This work has been funded as a [Use Case](https://www.nfdi4biodiversity.org/de/was-wir-tun/use-cases-uebersicht/#7077d1ec-b44f-4cec-83b0-fcc66bf0ff68)
of NFDI4Biodiversity (DFG project number 442032008,
https://nfdi4biodiversity.org) within the German National Research Data 
Infrastructure (NFDI, https://www.nfdi.de/). In addition, this work has been 
funded by NFDI4Earth (DFG project no. 460036893, https://www.nfdi4earth.de).

### Citation

Please cite the __hydrographr package__ as follows:

Schürz, M., Grigoropoulou, A., Garcia Marquez, J.R., Torres-Cambas, Y., Tomiczek, T.,
Floury, M., Bremerich, V., Schürz, C., Amatulli, G., Grossart, H.-P., Domisch, S. (2023). 
hydrographr: an R package for scalable hydrographic data processing. 
_Methods in Ecology and Evolution_, 14, 2953–2963. 
[https://doi:10.1111/2041-210X.14226](https://doi.org/10.1111/2041-210X.14226)

Please also cite the __Hydrography90m dataset__ as follows:

Amatulli, G., Garcia Marquez, J.R., Sethi, T., Kiesel, J., Grigoropoulou, A., 
Üblacker, M., Shen, L., Domisch, S. (2022). Hydrography90m: 
A new high-resolution global hydrographic dataset. Earth System Science Data, 
14(10), 4525–4550. [https://doi.org/10.5194/essd-14-4525-2022](https://doi.org/10.5194/essd-14-4525-2022)


---


### _hydrographr_ functions
<!-- Use HTML table: -->
<!-- Table styling! We need an embedded HTML table because markdown tables always 
have a header row. (The CSS styling below also applies to markdown tables). 
Colours: dark blue like menu bar above: #003D72, medium blue like selected item 
in menu bar: #006EB7, light blue in similar hue: #D5EEFF -->

<style>
  tr:hover {background-color: #D5EEFF;}
  table, th, td {
    border: 1px solid lightgrey;
    padding: 1em;
    width: 100%;
  }
  td.icon {
    width: 5%
  }
  td.funame {
    width: 17%
  }
  td.descrip {
    width: 78%
  }
</style>


### Downloading

<table>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/get_tile_id.html"><img style="padding: 7px;" width="120px" src="img/scr_get_tile_id.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/get_tile_id.html"><code>get_tile_id()</code></a></td>
    <td class="descrip">Identifies the ID of the regular tile(s) of the Hydrography90m, where the input points are located. The output IDs are required to download the data using the function <code>download_tiles</code>.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/get_regional_unit_ids.html"><img style="padding: 7px;" width="120px" src="img/scr_get_regional_unit_id.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/get_regional_unit_id.html"><code>get_regional_unit_id()</code></a></td>
    <td class="descrip">Identifies the ID of the regional unit(s) of the Hydrography90m, where the input points are located. The output IDs are required to download the data using the function <code>download_tiles</code>.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/download_tiles.html"><img style="padding: 7px;" width="120px" src="img/scr_download_tiles.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/download_tiles.html"><code>download_tiles()</code></a></td>
    <td class="descrip">Downloads data of the Hydrography90m dataset.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/download_test_data.html"><img style="padding: 7px;" width="120px" src="img/scr_download_test_data.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/download_test_data.html"><code>download_test_data()</code></a></td>
    <td class="descrip">Downloads the test data of the Hydrography90m dataset, required to run the examples of the manual.</td>
  </tr>
</table>


### Processing

<table>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/merge_tiles.html"><img style="padding: 7px;" width="120px" src="img/scr_merge_tiles.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/merge_tiles.html"><code>merge_tiles()</code></a></td>
    <td class="descrip">Merges raster (.tif) or vector (.gpkg) files using the GDAL functions <code>gdalbuilvrt</code> and <code>gdal_translate</code> for raster files and <code>ogrmerge.py</code> and <code>ogr2ogr</code> for vector files.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/crop_to_extent.html"><img style="padding: 7px;" width="120px" src="img/scr_crop_to_extent.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/crop_to_extent.html"><code>crop_to_extent()</code></a></td>
    <td class="descrip">Crops a raster (.tif) file to a polygon border line or to the extent of a bounding box using the GDAL function <code>gdalwarp</code>.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/snap_to_network.html"><img style="padding: 7px;" width="120px" src="img/scr_snap_to_network.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/snap_to_network.html"><code>snap_to_network()</code></a></td>
    <td class="descrip">Snaps point to the next stream segment within a defined radius or a defined radius and a minimum flow accumulation using the GRASS GIS function <code>r.stream.snap</code>.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/snap_to_subc_segment.html"><img style="padding: 7px;" width="120px" src="img/scr_snap_to_subc_segment.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/snap_to_subc_segment.html"><code>snap_to_subc_segment()</code></a></td>
    <td class="descrip">Snaps points to the stream segment of the sub-catchment where the points are located in using the GRASS GIS functions <code>v.net</code> and <code>v.distance</code>.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/set_no_data.html"><img style="padding: 7px;" width="120px" src="img/scr_set_no_data.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/set_no_data.html"><code>set_no_data()</code></a></td>
    <td class="descrip">Sets a <code>NoData</code> value to input files using the GDAL function <code>gdal_edit.py</code>.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/reclass_raster.html"><img style="padding: 7px;" width="120px" src="img/scr_reclass_raster.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/reclass_raster.html"><code>reclass_raster()</code></a></td>
    <td class="descrip">Reclassifies an integer raster (.tif) layer using the GRASS GIS function <code>r.reclass</code>.</td>
  </tr>
</table>

### Reading & data extraction


<table>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/extract_ids.html"><img style="padding: 7px;" width="120px" src="img/scr_extract_ids.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/extract_ids.html"><code>extract_ids()</code></a></td>
    <td class="descrip">Extracts the ID value of the drainiage basin and/or sub-catchment raster layer at given point locations using the GDAL function <code>gdallocationinfo</code>.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/report_no_data.html"><img style="padding: 7px;" width="120px" src="img/scr_report_no_data.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/report_no_data.html"><code>report_no_data()</code></a></td>
    <td class="descrip">Reports the <code>NoData</code> value of input files using the GDAL function <code>gdalinfo</code>.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/extract_zonal_stat.html"><img style="padding: 7px;" width="120px" src="img/scr_extract_zonal_stat.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/extract_zonal_stat.html"><code>extract_zonal_stat()</code></a></td>
    <td class="descrip">Calculates zonal statistics based on one or more environmental variable raster .tif layers across a set (or all) or sub-catchments in a spatial extent using the GRASS GIS function <code>r.univar</code>.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/read_geopackage.html"><img style="padding: 7px;" width="120px" src="img/scr_read_geopackage.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/read_geopackage.html"><code>read_geopackage()</code></a></td>
    <td class="descrip">Loads a .gpkg file, or only part of it, as a <code>data.table</code>, <code>graph</code>, <code>sf</code>, or <code>spatVector</code> object.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/get_upstream_catchment.html"><img style="padding: 7px;" width="120px" src="img/scr_get_upstream_catchment.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/get_upstream_catchment.html"><code>get_upstream_catchment()</code></a></td>
    <td class="descrip">Calculates the upstream basin taking each point as the outlet using the GRASS GIS function <code>g.region</code>.</td>
  </tr>
</table>


### Distance related


<table>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/get_distance.html"><img style="padding: 7px;" width="120px" src="img/scr_get_segment_neighbours.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/get_distance.html"><code>get_distance()</code></a></td>
    <td class="descrip">Calculates the euclidean or within-network distance between points using the GRASS GIS function <code>v.distance</code> or <code>v.net.allpairs</code>.</td>
  </tr>
</table>

### Graph-based connectivity analyses


<table>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/get_segment_neighbours.html"><img style="padding: 7px;" width="120px" src="img/scr_get_segment_neighbours.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/get_segment_neighbours.html"><code>get_segment_neighbours()</code></a></td>
    <td class="descrip">Reports the up- and/or downstream stream segments that are connected to the input segments within a neighbour order. Provides the option to summarise attributes across these segments.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/get_catchment_graph.html"><img style="padding: 7px;" width="120px" src="img/scr_get_catchment_graph.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/get_catchment_graph.html"><code>get_catchment_graph()</code></a></td>
    <td class="descrip">Extracts the upstream, downstrea or entire catchment of the input stream segments from a network graph.</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/get_distance_graph.html"><img style="padding: 7px;" width="120px" src="img/scr_get_distance_graph.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/get_distance_graph.html"><code>get_distance_graph()</code></a></td>
    <td class="descrip">Calculates the network distance between all input sub-catchment IDs from node to node (outlet of the stream segment).</td>
  </tr>
  <tr>
    <td class="icon"><a href="https://glowabio.github.io/hydrographr/reference/get_pfaffstetter_basins.html"><img style="padding: 7px;" width="120px" src="img/scr_get_pfaffstetter_basins.png"></img></a></td>
    <td class="funame"><a href="https://glowabio.github.io/hydrographr/reference/get_pfaffstetter_basins.html"><code>get_pfaffstetter_basins()</code></a></td>
    <td class="descrip">Delineates Pfaffstetter sub-basins for the input stream network.</td>
  </tr>
</table>