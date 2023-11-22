[View on github.](https://github.com/stevenranney/angler_pressure)

A work-in-progress to describe what's happening to the trout and angler population on the upper madison river. There are more anglers on the river and fewer fish. The human dimensions of fisheries management are often overlooked and the increased angling pressure on the Madison is already impacting angler sentiment. What happens as pressure continues to increase and mortality increases concomittantly with no understanding of recruitment?

This started as an analysis in Python but for some things, R really is better (i.e., comparing Q3 lines).

Target journal: North American Journal of Fisheries Management

Files are named "in order" of how I've drafted the manuscript; running `00_helper_functions.R`, then `01_...`, etc, will recreate all the data files and images needed for anyone new to this repository.

## NOTES

* Special thanks to datawim at https://www.datawim.com/post/inset-map-in-r/ for helping me develop the inset map.
* The river hydgrogrphy GIS data for creating the map can be downloaded from https://ftpgeoinfo.msl.mt.gov/Data/Spatial/MSDI/Hydrography/. The dataset is large and committing it here is too much.