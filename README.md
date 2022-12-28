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
1. `batch_build.R` - takes the raw data, constructs frame-by-frame networks for each play, calculates network measures of interest, and creates weekly "builds" for downstream analysis; note that this script has been implemented to intake a command-line argument, such that parallel computation can be achieved using computing clusters if available (each week takes ~1 day to run)
2. `analysis.R` - wrangles the builds into analysis-friendly form and produces the results
3. `gifs.R` - creates the `igraph` network animations

## Folders
Data, visuals, and other pertinent material are partitioned inside the following folders:
- `data` - placeholder folder for the competition data, wrangled datasets, and images used to construct animations
- `images` - contains all static images 
- `gifs` - contains all animations; also contains the `Python` documentation for the supplementary `plotly` animation
