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

`Python` was used to generate one plot in the `gifs` folder. Please navigate to that folder for details. 

## Scripts
The following scripts were used in this project and executed in chronological order: 
1. `batch_build.R` - command-line implementation that intakes the raw data, constructs frame-by-frame networks for each play, calculates network measures of interest, and creates weekly builds for downstream analysis 
2. `analysis.R` - takes the builds, wrangles them into analysis-friendly form, and produces the [notebook](https://www.kaggle.com/code/brunoscodari/big-man-betweenness-bmb) results
3. `gifs.R` - saves down the individual images for the network GIFs

## Folders
Data, visuals, and other pertinent material are partitioned inside the following folders:
- `data` - placeholder folder for the competition data
- `builds` - contains the wrangled datasets produced by `builds.R` and one intermediate dataset produced by `analysis.R`
- `images` - contains all images produced produced by `analysis.R` 
- `gifs` - contains all GIFs produced by `gifs.R` and `analysis.R`
