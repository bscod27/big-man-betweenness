# big-man-betweenness
This repository contains all working material pertinent to the Big Man Betweenness [notebook](https://www.kaggle.com/code/brunoscodari/big-man-betweenness-bmb), which was submitted to the NFL Big Data Bowl 2023 [competition](https://www.kaggle.com/competitions/nfl-big-data-bowl-2023). 

## Reproducibility
For those seeking to reproduce our work, we recommend the following: 
- Install the specific version of `R` and required packages outlined in `requirements.txt`
- Clone a remote copy of the repository to your local machine and navigate to the root directory:

```
git clone https://github.com/bscod27/big-man-betweenness.git
cd big-man-betweenness.git
``` 

- Download, unzip, and store the competition [files](https://www.kaggle.com/competitions/nfl-big-data-bowl-2023/data) in the `data` folder
- Execute the scripts detailed below

## Scripts
The following scripts were used in this project and executed in chronological order: 
1. `batch_build.R` - command-line implementation that intakes the raw data, constructs frame-by-frame networks for each play, calculates network measures of interest, and creates weekly builds for downstream analysis 
2. `analysis.R` - takes the builds, wrangles them into analysis-friendly form, and produces the [notebook](https://www.kaggle.com/code/brunoscodari/big-man-betweenness-bmb) results
3. `gifs.R` - saves down the individual images for the network GIFs

## Folders
Data, visuals, and other pertinent material are partitioned inside the following folders:
- `data` - placeholder folder for the competition data
- `builds` - contains the wrangled "builds" (produced by `builds.R`) and `rolled.csv` (produced by `analysis.R`), which is displayed in the report 
- `images` - contains the images (produced by `analysis.R`) that are shown in the report
- `gifs` - contains the animations (produced by `gifs.R`, `analysis.R`, and `plotly_viz.py`) that are shown in the report
  - Note: `Python` documentation for `plotly_viz.py` can be found in `gifs`; we have not included it here as all information in the report can be generated using the `R` analytic workflow
