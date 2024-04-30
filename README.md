[View on github.](https://github.com/stevenranney/angler_pressure)

Target journal: North American Journal of Fisheries Management

This repository contains the code and data that are used in the manuscript titled "Angler Satisfaction Survey Results as Indicators of Fisheries Changes: A Case Study."

Users can clone the repo, review the code, and run the analyses contained in the paper and evaluate the output for themselves. `R` code is contained in the logically-named directory `R` and, if conducting all analyses, should be run beginning with sourcing the file labeled `00_helper_functions.R` and proceed from there. Numbered files build on output from the previous files. Raw data (i.e., as downloaded from the MTFWP FishMT site) is located in `./data/raw`. "Handled" data is written to `./data/*.rds`. Data associated with mapping is in `./data/gis`.

I have intentionally dropped the files beginning with `03...` and `04...` because they were no longer useful to the analyses. Notably, `05...` and `05a...` files contain analyses that are NOT included in the manuscript but are preserved here in case reviewers demand their inclusion, though an argument could be made to drop all analyses of *W_r*.

Output from analyses files are written to `output/` dir and branced from there.

If readers have any questions, please feel free to contact me at `Steven.Ranney "at" gmail.com`.

## NEWS

* 2024-04-30 - Initial submission to the North American Journal of Fisheries Management.

## NOTES

* Special thanks to datawim at https://www.datawim.com/post/inset-map-in-r/ for helping me develop the inset map.
* The river hydgrogrphy GIS data for creating the map can be downloaded from https://ftpgeoinfo.msl.mt.gov/Data/Spatial/MSDI/Hydrography/. The dataset is large and committing it here is too much.
* Many, but not all, of the literature sources that are referenced in the manuscript are available in the `/literature` directory.

## Reads

* https://derekogle.com/fishR/examples/oldFishRVignettes/MROpen.pdf

