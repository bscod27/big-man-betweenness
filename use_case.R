library(tidyverse)
library(igraph)
library(lme4)

setwd('~/01. Dartmouth/04. Coursework/05. Fall 2022/03. Big Data Bowl/03. Linked Git/data') 
euclidean_dist <- function(x, y) {sqrt(sum((x - y)^2))}

##### 00. Read in the data #####
tracking <- data.frame()
for (i in list.files(pattern='.csv')) {
  print(paste0(i,'...'))
  df <- read.csv(i)
  if (str_detect(str_match(i, '(.*).csv')[2], 'week')) {
    df$week <- as.numeric(str_match(i, 'week(.*).csv')[2])
    tracking <- rbind(tracking, df)
  } else {
    assign(str_match(i, '(.*).csv')[2], df)
  }
  rm(df)
}

##### 01. Join all data into main frame #####
df <- tracking %>%
  left_join(players, by = 'nflId') %>%
  left_join(plays, by = c('gameId', 'playId')) %>%
  left_join(pffScoutingData, by = c('gameId', 'playId', 'nflId')) %>%
  left_join(games, by = c('week', 'gameId'))

rm(tracking)
colnames(df)

##### 02. Loop through tracking and define networks for each play #####

example <- data.frame()
week <- 1; game <- 2021091207; play <- 3828
dat <- df %>% filter(week==week, gameId==game, playId==play)

for (frame in unique(dat$frameId)) {
  sliver <- dat %>% filter(frameId == frame)
  qb <- sliver %>% filter(officialPosition == 'QB') 
  if (nrow(qb) > 1) {
    qb <- qb[1, ] # keep first row only
  }
  
  line <- sliver %>% filter(officialPosition %in% c('C','G','T')) 
  offense <- sliver %>% filter(officialPosition %in% c('WR','RB','FB','TE')) 
  defense <- sliver %>% filter(officialPosition %in% c('DE','NT','DT','OLB','MLB','ILB','LB','SS','FS','CB','DB')) 
  out <- data.frame()
  hurry <- ifelse(sum(line$pff_hurryAllowed, na.rm=T)>1,1,0)
  sack <- ifelse(sum(line$pff_sackAllowed, na.rm=T)>1,1,0)
  hit <- ifelse(sum(line$pff_hitAllowed, na.rm=T)>1,1,0)
  
  # get distances between QB and all other players
  q_xy <- qb %>% dplyr::select(x,y) %>% unique(.)
  
  for (lineman in unique(line$nflId)){
    l_xy <- line %>% filter(nflId == lineman) %>% dplyr::select(x,y) %>% unique(.)
    add <- data.frame(
      week,'gameId'=game,'playId'=play,'frameId'=frame,'event'=qb$event,
      'ref'=qb$nflId,'player'=lineman,'player_pos'='line','dist'=euclidean_dist(q_xy,l_xy),q_xy,l_xy, hurry, sack, hit,
      'pos_team'=unique(qb$possessionTeam), 'def_team'=unique(qb$defensiveTeam)
    )
    colnames(add)[10:13] <- c('rx','ry','x','y')
    out <- rbind(out, add)
  }

  for (defender in unique(defense$nflId)){
    d_xy <- defense %>% filter(nflId == defender) %>% dplyr::select(x,y) %>% unique(.)
    add <- data.frame(
      week,'gameId'=game,'playId'=play,'frameId'=frame,'event'=qb$event, 
      'ref'=qb$nflId,'player'=defender,'player_pos'='defense','dist'=euclidean_dist(q_xy,d_xy),q_xy,d_xy, hurry, sack, hit,
      'pos_team'=unique(qb$possessionTeam), 'def_team'=unique(qb$defensiveTeam)
    )
    colnames(add)[10:13] <- c('rx','ry','x','y')
    out <- rbind(out, add)
  }
  
  for (attacker in unique(offense$nflId)){
    o_xy <- offense %>% filter(nflId == attacker) %>% dplyr::select(x,y) %>% unique(.)
    add <- data.frame(
      week,'gameId'=game,'playId'=play,'frameId'=frame,'event'=qb$event,
      'ref'=qb$nflId,'player'=attacker,'player_pos'='offense','dist'=euclidean_dist(q_xy,o_xy),q_xy, o_xy, hurry, sack, hit,
      'pos_team'=unique(qb$possessionTeam), 'def_team'=unique(qb$defensiveTeam)
    )
    colnames(add)[10:13] <- c('rx','ry','x','y')
    out <- rbind(out, add)
  }
  
  # get distances between offensive line and all defensive players defense
  for (lineman in unique(line$nflId)){
    coord <- line %>% filter(nflId == lineman) %>% dplyr::select(x,y)
    for (defender in unique(defense$nflId)){
      d_xy <- defense %>% filter(nflId == defender) %>% dplyr::select(x,y) %>% unique(.)
      add <- data.frame(
        week,'gameId'=game,'playId'=play,'frameId'=frame,'event'=qb$event, 
        'ref'=lineman,'player'=defender,'player_pos'='defense','dist'=euclidean_dist(coord,d_xy),coord,d_xy, hurry, sack, hit,
        'pos_team'=unique(qb$possessionTeam), 'def_team'=unique(qb$defensiveTeam)
      )
      colnames(add)[10:13] <- c('rx','ry','x','y')
      out <- rbind(out, add)
    }
  }
  
  if(nrow(out)>0) { # sometimes there is no ball snap in the data, so out will be null
    # extract edges, nodes, and create graph
    edges <- out[, c('ref','player')] %>% unique(.)
    nodes <- rbind(
      data.frame(player=out$ref[1], 'player_pos'='qb'),
      unique(out[, c('player', 'player_pos')])
    ) %>% mutate(
      color = case_when(
        player_pos == 'qb' ~ 'yellow',
        player_pos == 'line' ~ 'green',
        player_pos == 'defense' ~ 'pink',
        player_pos == 'offense' ~ 'lightblue'
      )
    ) %>% 
      left_join(players[, c('nflId', 'officialPosition')], by=c('player'='nflId'))
    g <- graph.data.frame(edges, directed=FALSE)
    E(g)$weight <- unique(1/out$dist) # inverse weighting scheme with restriction
    
    # plot
    l <- layout.auto(g)
    qb_slice <- qb %>% select('player'=nflId, x, y)
    line_slice <- out %>% filter(player_pos=='line') %>% dplyr::select(player,x,y) %>% unique(.)
    def_slice <- out %>% filter(player_pos=='defense') %>% dplyr::select(player,x,y) %>% unique(.)
    off_slice <- out %>% filter(player_pos=='offense') %>% dplyr::select(player,x,y) %>% unique(.)
    colnames(qb_slice) <- c('player', 'x', 'y')
    colnames(line_slice) <- c('player', 'x', 'y')
    colnames(def_slice) <- c('player', 'x', 'y')
    colnames(off_slice) <- c('player', 'x', 'y')
    l <- as.matrix(rbind(qb_slice, line_slice, def_slice, off_slice)[, 2:3])
    V(g)$name <- nodes$player
    V(g)$color <- nodes$color
    V(g)$pos <- nodes$officialPosition
    g <- delete.edges(g, which(1/E(g)$weight > 7))
    edge_attr(g)$weight[which(1/E(g)$weight > 7)] <- 0.00001 # really low value
    V(g)$label.cex = 1
    plot(g, layout=l, vertex.label=V(g)$pos)
    
    # join
    a <- out %>% dplyr::select(week, gameId, playId, frameId, event) %>% unique(.) %>% filter(complete.cases(.))
    b <- cbind(a, nodes, l)
    example <- rbind(example, b)
    
  }
}

bmb <- read.csv('../use_case/example.csv')
graph <- example %>% left_join(bmb, by = c('week', 'gameId', 'playId', 'frameId'))
write.csv(graph, '../use_case/graph.csv', row.names = FALSE)
