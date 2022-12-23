# big-man-betweenness
This repository contains all working material pertinent to the Big Man Betweenness [notebook](https://www.kaggle.com/code/brunoscodari/big-man-betweenness-bmb), which was submitted to the NFL Big Data Bowl 2023 Kaggle competition. 

## Reproducibility
For those seeking to reproduce our work, we recommend the following: 
- Download/install the specific version of `R` and required packages outlined in `requirements.txt`
- Clone a remote copy of the repository to your local machine and navigate to the root directory:
```
git clone https://github.com/bscod27/big-man-betweenness.git
cd big-man-betweenness.git
``` 
- Execute the scripts detailed below

## Scripts
The following scripts were used in this project and executed in chronological order: 
1. `tracking.R` - converts the weekly tracking data from `.csv >> .gz` to compress the data into a form that can be pushed to GitHub; note that this script assumes the tracking data has been downloaded locally as `.csv` files
2. `batch_build.R` - command-line implementation that intakes the raw data, constructs frame-by-frame networks for each play, calculates network measures of interest, and creates weekly builds for downstream analysis 
3. `analysis.R` - takes the builds, wrangles them into analysis-friendly form, and produces the [notebook](https://www.kaggle.com/code/brunoscodari/big-man-betweenness-bmb) results
4. `gifs.R` - saves down the individual images for the network GIFs

## Folders
Data, visuals, and other pertinent material are partitioned inside the following folders:
- `data` - contains the raw data as supplied by Kaggle; weekly tracking data has been converted to `.gz` files by `tracking.R`
- `builds` - contains the engineered data produced by `builds.R`
- `images` - contains all images produced produced by `analysis.R` 
- `gifs` - contains all GIFs produced by `gifs.R` and `analysis.R`
