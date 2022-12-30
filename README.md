# big-man-betweenness
This repository contains all working material pertinent to the Big Man Betweenness [notebook](https://www.kaggle.com/code/brunoscodari/big-man-betweenness-bmb), which was submitted to the 2023 NFL Big Data Bowl [competition](https://www.kaggle.com/competitions/nfl-big-data-bowl-2023). 

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
1. `batch_build.R` - takes the raw data, constructs frame-by-frame networks for each play, calculates network measures of interest, and creates weekly "builds" for downstream analysis
    - *Note: this script has been programmed to be executed via the command line and fed a positional argument specifying the week/batch of interest; this was done to enable parallel computation, as the program is computationally expensive*
2. `analysis.R` - wrangles the builds into analysis-friendly form and produces the results
3. `gifs.R` - creates the `igraph` network animations

We use relative path notation in our scripts, as we assume the user sets their working directory to the root of the current folder structure.

## Folders
Data, visuals, and other pertinent material are partitioned inside the following folders:
- `data` - placeholder folder for the competition data, wrangled datasets, and images used to construct animations
- `images` - contains all static images 
- `gifs` - contains all animations; also contains `Python` documentation for the supplementary `plotly` animation
