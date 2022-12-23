setwd('~/01. Dartmouth/04. Coursework/05. Fall 2022/03. Big Data Bowl/03. Linked Git/data') 

library(tidyverse)
library(data.table)
library(R.utils)

for (i in list.files(pattern='week.*.csv')) {
  # compress to gz files
  print(paste0(i,'...'))
  df <- read.csv(i)
  df$week <- as.numeric(str_match(i, 'week(.*).csv')[2])
  fwrite(df, paste0(str_match(i, '(.*).csv')[2],'.gz'), compress='gzip')
  # generate plotly csv data 
  if (i == 'week1.csv') {
    df %>% 
      filter(gameId==2021091207, playId==3828) %>% 
      write.csv(., '../gifs/plotly_tracking.csv')
  }
}