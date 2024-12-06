## GeoFRESH in a nutshell

Freshwater water bodies are highly connected with each other and with their 
terrestrial catchments. In the light of climate and land use changes as well as 
feedback mechanisms between earth systems, the integration of earth system data 
into freshwater research is long-overdue to assess those interdependencies. 
However, freshwater-specific characteristics like spatial connectivity and 
fragmentation as well as legacy effects require a specialized workflow. 
Within the __first pilot project__ 
<a href="https://www.nfdi4earth.de/2participate/pilots" target="_blank">
"GeoFRESH: Getting freshwater spatio-temporal data on track"</a>, 
the aim was to built a prototype for a new online platform, called _GeoFRESH_. 
The platform provides the integration, processing, management and visualization 
of various standardized spatiotemporal freshwater-related earth system data. 
The platform is built around 
<a href="https://geo.igb-berlin.de" target="_blank">IGB GeoNode</a> using RShiny 
and includes the newly created 
<a href="https://hydrography.org/hydrography90m/hydrography90m_layers" target="_blank">
Hydrography90m dataset</a>. 
In the __second pilot project__ 
<a href="https://www.nfdi4earth.de/2participate/pilots" target="_blank">
"Connecting rivers and lakes FAIRly"</a>
we integrated lakes into GeoFRESH, currently based on the Hydrolakes dataset and 
planned to be replaced by the NASA SWOT data. The aim for the __third pilot project__ 
<a href="https://www.nfdi4earth.de/2participate/pilots" target="_blank">
"The seamless interoperability of geospatial freshwater tools"</a>
is an improved interoperability and user experience of the GeoFRESH platform, 
including interactive point snapping, an improved visualization, a connection to 
the _hydrographr_ R-package, and the integration of dams / barriers.


__Development team__: Vanessa Bremerich, Yusdiel Torres-Cambas, Afroditi Grigoropoulou, 
Jaime R. García Márquez, Sami Domisch, Thomas Tomiczek, Merret Buurman <br/>
__Proposal team__: Sami Domisch, Giuseppe Amatulli, Luc De Meester, Hans-Peter Grossart, 
Mark Gessner, Thomas Mehner, Vanessa Bremerich, Rita Adrian <br/>
__Contact information__:  sami.domisch@igb-berlin.de <br/>
__Bug reports and feature requests__: 
<a href="https://github.com/glowabio/geofresh/issues" target="_blank">github.com/glowabio/geofresh/issues</a>
<br/>

__Project duration__:  <br/>
Pilot 1: 01.04.2022 - 31.03.2023 <br/>
Pilot 2: 01.10.2023 - 30.09.2024 <br/>
Pilot 3: 01.03.2025 - 28.02.2026 <br/>

__Project funding__: 
<a href="https://www.nfdi4earth.de" target="_blank">NFDI4Earth (DFG)</a><br/>


This work has been funded by the German Research Foundation (DFG) through the 
project NFDI4Earth (TA1 M1.1, DFG project no. 460036893, 
<a href="https://www.nfdi4earth.de" target="_blank">www.nfdi4earth.de</a>) 
within the German National Research Data Infrastructure (NFDI, 
<a href="https://www.nfdi.de/" target="_blank">www.nfdi.de</a>). 


---




### Citation

Please cite the GeoFRESH platform as follows:

Domisch, S., Bremerich, V., Buurman, M., Kaminke, B., Tomiczek, T., Torres-Cambas, 
Y., Grigoropoulou, A., Garcia Marquez, J. R., Amatulli, G., Grossart, H. P., Gessner, 
M. O., Mehner, T., Adrian, R. & De Meester, L. (2024). GeoFRESH – an online 
platform for freshwater geospatial data processing. _International Journal of Digital Earth_, 
17(1). <a href="https://doi.org/10.1080/17538947.2024.2391033" target="_blank">
doi.org/10.1080/17538947.2024.2391033</a>

<br/>

In addition, GeoFRESH relies on a number of external data sources regarding the 
environmental data and we ask you to please use the following citations depending 
on your analysis: 

__Topography (Hydrography90m)__ <br/>
Amatulli, G., Marquez, J.G., Sethi, T., Kiesel, J., Grigoropoulou, A., Ublacker, 
M. M., Shen, L. Q., & Domisch, S. (2022). Hydrography90m: a new high-resolution 
global hydrographic dataset. Earth System Science Data, 14(10), 4525-4550. 
<a href="https://doi.org/10.5194/essd-14-4525-2022" target="_blank">
doi.org/10.5194/essd-14-4525-2022</a>

__Climate (CHELSA v2.1)__ <br/>
Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., 
Zimmermann, N.E., Linder, P., Kessler, M. (2017): Climatologies at high resolution 
for the Earth land surface areas. _Scientific Data_, 4 170122. 
<a href="https://doi.org/10.1038/sdata.2017.122" target="_blank">
doi.org/10.1038/sdata.2017.122</a><br/>
Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., 
Zimmermann, N.E., Linder, H.P. & Kessler, M. (2021) Climatologies at high resolution 
for the earth’s land surface areas. EnviDat. 
<a href="https://doi.org/10.16904/envidat.228.v2.1" target="_blank">
doi.org/10.16904/envidat.228.v2.1</a>


__Soil (SoilGrids250m)__ <br/>
Hengl, T., Mendes de Jesus, J., Heuvelink, G.B.M., Ruiperez Gonzalez, M., 
Kilibarda, M., Blagotić, A., Shangguan, W., Wright, M.N., Geng, X., 
Bauer-Marschallinger, B., Guevara, M.A., Vargas, R., MacMillan, R.A., Batjes, 
N.H., Leenaars, J.G.B., Ribeiro, E., Wheeler, I., Mantel, S., & Kempen, B. (2017). 
SoilGrids250m: Global gridded soil information based on machine learning. 
PLOS ONE, 12(2), e0169748. <a href="https://doi.org/10.1371/journal.pone.0169748" target="_blank">
doi.org/10.1371/journal.pone.0169748</a>

__Landcover (ESA CCI LC)__ <br/>
ESA. Land Cover CCI Product User Guide Version 2. Tech. Rep. (2017). Available 
at: <a href="http://maps.elie.ucl.ac.be/CCI/viewer/download/ESACCI-LC-Ph2-PUGv2_2.0.pdf" target="_blank">
maps.elie.ucl.ac.be/CCI/viewer/download/ESACCI-LC-Ph2-PUGv2_2.0.pdf</a>


__Lakes (HydroLAKES v1.0)__ <br/>
Messager, M.L., Lehner, B., Grill, G., Nedeva, I., Schmitt, O. (2016). 
Estimating the volume and age of water stored in global lakes using a 
geo-statistical approach. Nature Communications, 7: 13603. 
<a href="https://doi.org/10.1038/ncomms13603" target="_blank">
doi.org/10.1038/ncomms13603</a>


---

### Changelog

15.11.2024: Changed the unit of the climate CHELSA v2.1 variable "bio4" from 
°C to °C/100 (following the CHELSA documentation) <br/>

19.08.2024: Publication online available at 
<a href="https://doi.org/10.1080/17538947.2024.2391033" target="_blank">
doi.org/10.1080/17538947.2024.2391033</a><br/>
			Updated tutorial <br/>
			Minor user interface improvements <br/>

---


### NFDI4Earth

<a href="https://www.nfdi4earth.de" target="_blank">NFDI4Earth</a> addresses 
digital needs of Earth System Sciences. Earth System scientists cooperate in 
international and interdisciplinary networks with the overarching aim to 
understand the functioning and interactions within the Earth system and address 
the multiple challenges of global change. NFDI4Earth is a community-driven 
process providing researchers with FAIR, coherent, and open access to all relevant 
Earth System data, to innovative research data management and data science methods. 

GeoFRESH was initiated as part of the <a href="https://www.nfdi4earth.de/2participate/pilots" target="_blank">
first cohort of 1-year pilot projects</a>: 
in a first round in 2020, 14 pilots out of 38 were selected and started in April 
2022. In the second round, seven (out of 27) pilots were funded and started in 
2023. In the third round, another five pilots were funded, starting in 2025.


### Further functionality of R package _hydrographr_

For further analyses of your freshwater data, you can use the _hydrographr_ R package 
that facilitates the download and data processing of the Hydrography90m data 
(publication in <a href="https://doi.org/10.1111/2041-210X.14226" target="_blank">
Methods in Ecology and Evolution</a>, 
<a href="https://glowabio.github.io/hydrographr" target="_blank">website</a> 
with details and examples, <a href="https://github.com/glowabio/hydrographr" target="_blank">
source code</a> on GitHub).

For more details and an overview of the functions, please check the _hydrographr_ panel.
