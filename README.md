[View on github.](https://github.com/stevenranney/angler_pressure)

Target journal: North American Journal of Fisheries Management

This repository contains the code and data that are used in the manuscript titled "Angler Satisfaction Survey Results as Indicators of Fisheries Changes: A Case Study."

Users can clone the repo, review the code, and run the analyses contained in the paper and evaluate the output for themselves. `R` code is contained in the logically-named directory `R` and, if conducting all analyses, should be run beginning with sourcing the file labeled `00_helper_functions.R` and proceed from there. Numbered files build on output from the previous files. Raw data (i.e., as downloaded from the MTFWP FishMT site) is located in `./data/raw`. "Handled" data is written to `./data/*.rds`. Data associated with mapping is in `./data/gis`.

I have intentionally dropped some numbered files because they were no longer useful to the analyses. Notably, `05...` and `05a...` files contain analyses that are NOT included in the manuscript but are preserved here in case reviewers demand their inclusion, though an argument could be made to drop all analyses of *W<sub>r</sub>*.

Output from analyses files are written to `output/` dir and branched from there.

The `/python` directory is a dead end. Originally, I had considered doing all of these analyses in Python because I like Python, but given I am much more efficient, I switched to `R`. I'm leaving this here for posterity.

If readers have any questions, please feel free to contact me at `Steven.Ranney "at" gmail.com`.

## NEWS

* 2025-06-19 - I've chosen to trim much of this manuscript and shift its focus to it strongest sections, namely the quantile regression estimates and discussion.
* DATE - Rejected by one reviewer, resubmit by the other.
* 2024-04-30 - Initial submission to the North American Journal of Fisheries Management.

## NOTES

* Special thanks to [datawim](https://www.datawim.com/post/inset-map-in-r/) for helping me develop the inset map.
* The river hydgrogrphy GIS data for creating the map can be downloaded from [here](https://ftpgeoinfo.msl.mt.gov/Data/Spatial/MSDI/Hydrography/). The dataset is large and committing it here is too much.
* Many, but not all, of the literature sources that are referenced in the manuscript are available in the `/literature` directory.

## Reads

* [Some information from D. Ogle that helped with the MLE estimates.](https://derekogle.com/fishR/examples/oldFishRVignettes/MROpen.pdf)

