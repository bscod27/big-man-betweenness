setwd('~/01. Dartmouth/04. Coursework/05. Fall 2022/03. Big Data Bowl/03. Linked Git/data') 

library(tidyverse)
library(data.table)
library(R.utils)

for (i in list.files(pattern='week.*.csv')) {
  print(paste0(i,'...'))
  df <- read.csv(i)
  df$week <- as.numeric(str_match(i, 'week(.*).csv')[2])
  fwrite(df, paste0(str_match(i, '(.*).csv')[2],'.gz'), compress="gzip")
}
