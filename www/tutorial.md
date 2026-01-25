## Step-by-step guide to GeoFRESH

This tutorial walks though the single steps of GeoFRESH using the test data set (random selection of fish occurrences, drawn from the Harmonised freshwater fish occurrence and abundance data for 12 federal states in Germany, downloaded from <a href="https://www.gbif.org/dataset/e0908eee-ad49-4e91-b4d0-1f05dd17b291" target="_blank">GBIF</a>).

GeoFRESH helps you link point locations (e.g., occurrence records or sampling sites) to the **Hydrography90m** river network and catchments, and then annotate those points with **environmental variables** at two scales: **local sub-catchment** and **upstream catchment**.

Point data can be created either by **uploading a CSV** or by **creating/editing points in the Point Editor**. Depending on your needs, you can follow one of several workflows.

---

# 1) Create your point data

## Option A — Upload points as CSV
Upload a **.csv** file containing point coordinates. The file must include:
- an **ID** column
- **latitude**
- **longitude**

Column names are flexible, but coordinates must be in **WGS84 (EPSG:4326)**.

After upload:
- points appear immediately on the map
- the table view allows inspection, filtering, and quality checks before processing

## Option B — Create points in the Point Editor
Open the **Point Editor** to create or refine points interactively:
- **Insert points** by placing markers on the map
- **Move points** by dragging markers
- **Delete points** or keep a subset using selection tools:
  - polygon selection
  - bounding box
  - uploaded polygons (GeoPackage)
  - catchment-based selection (click-to-delineate)

This option is useful for digitizing points manually or correcting uploaded coordinates.

---

# 2) Snap points to the stream network

Snapping assigns each point to the Hydrography90m **regional unit**, **sub-catchment**, and **stream segment**, and places the point **exactly on the stream line**. A progress bar indicates status.

### Snapping methods
GeoFRESH provides two snapping methods:

1) **Sub-catchment snapping (default)**  
Points are snapped to the nearest stream segment **within the sub-catchment the point falls into**. This is the recommended option for catchment-based analyses.

2) **Closest stream by Strahler order (optional)**  
Points are snapped to the nearest stream segment with a **selected Strahler order**, within a maximum search distance. Points farther than the maximum distance remain unsnapped.

### Manual correction concept (important)
If the initial snapping result is not what you expect, you can correct it in the **Point Editor**:

- Dragging a snapped marker creates a **manual hint** (your intended location).
- Snapping must then be run again to:
  - place the point precisely on the stream line
  - update its region/sub-catchment/segment assignment consistently

---

# 3) Review snapped points

After snapping:
- snapped points are shown on the map (typically **yellow markers**)
- the data table includes additional snapped coordinate columns
- zooming in shows the point lies precisely on the stream line

If a point cannot be snapped (e.g., no matching segment under the selected constraints), it remains unsnapped and is flagged.

---

# 4) Annotate points with environmental variables

Afterwards, you can annotate the point data with environmental information across the sub-catchment of each point. You can select from a suite of **48 variables related to**  
<a href="https://hydrography.org/hydrography90m/hydrography90m_layers" target="_blank">topography and hydrography</a>,  
**19** <a href="http://chelsa-climate.org" target="_blank">climate variables</a> (i.e., current bioclimatic variables),  
**15** <a href="https://soilgrids.org" target="_blank">soil</a> variables, and  
**22** <a href="http://maps.elie.ucl.ac.be/CCI/viewer/index.php" target="_blank">land cover</a> variables.

You can compute summaries for:

- **Local (sub-catchment) conditions** at each point  
- **Upstream catchment conditions** draining into each point

Click **Start query** to compute the results.

### Outputs of environmental annotation
Each extraction (local or upstream) provides:

1) **Results table (CSV)**  
A table is generated and can be downloaded as a **CSV file** from the same menu where the extraction was run.

2) **Summary plot**  
A plot is produced to summarize the selected variables (e.g., distributions for continuous variables and category summaries for categorical land-cover variables). This helps with quick interpretation and quality control before downloading.

---

# 5) Download your data

GeoFRESH provides multiple download options depending on what you produced in the session.

## A) Download points from the Point Editor (CSV/GeoJSON/GPKG)
From the **Point Editor**, use **Save as…** to export a points file at any time. You can export:

- **Edited points** (your current working version after any changes)
- **Original coordinates** (latitude/longitude)
- **Snapped coordinates** (latitude_snap/longitude_snap), if snapping has been run  
  - if some points are not snapped, exports may fall back to original coordinates depending on the chosen option

This is the recommended way to export corrected point datasets.

## B) Download environmental tables from their respective menus (CSV)
Local and upstream environmental annotation tables can be downloaded as **CSV** directly from the menu where they were created.

## C) Download everything from the central Download menu (lateral sidebar)
A **central Download menu** in the lateral sidebar allows you to download:

- any available output independently (points, local table, upstream table)
- or a bundled download (e.g., points + local + upstream), **provided those outputs exist** in your session

This is the easiest way to retrieve all outputs in one place.

---

# Common workflows

## Workflow 1 — Upload → Snap (menu) → Annotate → Download
1) Upload points as CSV  
2) Snap points from the lateral menu (choose method if needed)  
3) Annotate points (local and/or upstream) and inspect the summary plot  
4) Download tables (CSV) and/or use the central Download menu

Use this workflow when default snapping produces the expected results.

---

## Workflow 2 — Upload → Snap → Correct in Point Editor → Snap again → Annotate → Download
1) Upload points as CSV  
2) Snap points  
3) If some points are snapped to unexpected segments:
   - open the Point Editor
   - move points and/or provide manual hints
4) Snap again to apply corrections and update assignments  
5) Annotate points and inspect plots  
6) Download point exports (original/snapped) and annotation tables

Use this workflow when you need interactive correction to achieve the intended stream placement.

---

## Workflow 3 — Create points in Point Editor → Snap → Annotate → Download
1) Create points manually in the Point Editor  
2) Save edits  
3) Snap points (select method if needed)  
4) Annotate points (local and/or upstream) and inspect plots  
5) Download exported points and annotation tables (or use the central Download menu)

Use this workflow when you want to build a dataset from scratch inside GeoFRESH.

---

# Data privacy and session behavior
All data are session-based. When you close the browser window, uploaded data and derived results are removed. No data are stored permanently on the platform.

---

## References
GBIF.org (24 April 2023) GBIF Occurrence Download. doi.org/10.15468/dl.xbuqe5
