## NOTE: Please set working directory to root of the repository folder structure ##

library(tidyverse)
library(igraph)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop('No argument provided.')
}
week <- args[1]

euclidean_dist <- function(x, y) {sqrt(sum((x - y)^2))}
transform_values <- function(x) {1.025^x-1}
inv_transform <- function(y) {log(y+1)/log(1.025)}

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

##### 02. Loop through tracking and define networks for each play #####
df_line <- data.frame()
print(paste0('Week: ',week))
sliced <- df[df$week == week, ]

game_count <- 1
for (game in unique(sliced$gameId)) {
  print(paste0(' Game: ',game_count,'/',length(unique(sliced$gameId))))
  temp <- sliced %>% filter(gameId == game)
  
  for (play in unique(temp$playId)) {
    dat <- temp %>% filter(playId == play)
    
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
      
      # get distances between the QB and defense and the QB and O-line
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
      
      # get distances between O-line and defense
      for (lineman in unique(line$nflId)){
        coord <- line %>% filter(nflId == lineman) %>% dplyr::select(x,y)
        for (defender in unique(defense$nflId)){
          d_xy <- defense %>% filter(nflId == defender) %>% dplyr::select(x,y)
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
        out$dist[which(out$player_pos=='offense')] <- 10 # tricking R to not showing these edges
        edges <- out[, c('ref','player')] %>% unique(.)
        nodes <- rbind(
          data.frame(player=out$ref[1], 'player_pos'='qb'),
          unique(out[, c('player', 'player_pos')])
        ) %>% mutate(
          color = case_when(
            player_pos == 'qb' ~ 'yellow',
            player_pos == 'line' ~ 'blue',
            player_pos == 'defense' ~ 'red',
            player_pos == 'offense' ~ 'green'
          )
        )
        g <- graph.data.frame(edges, directed=FALSE)
        E(g)$weight <- transform_values(out$dist) # transform the edges
        
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
        edge_attr(g)$weight[which(inv_transform(E(g)$weight) == 0)] <- 0.001 # deal with zeroes 
        g <- delete.edges(g, which(inv_transform(E(g)$weight) > 7)) # remove edges >7 yards
        
        # calculate various network measures
        l_idx <- which(names(V(g)) %in% line_slice$player)
        
        # node measures
        betw <- betweenness(g, normalized = TRUE)
        eigs <- eigen_centrality(g)$vector
        close <- closeness(g)
        
        # line metrics
        line_betw_mean <- mean(betw[l_idx])
        line_eigs_mean <- mean(eigs[l_idx])
        line_close_mean <- mean(close[l_idx])
        line_betw_var <- var(betw[l_idx])
        line_eigs_var <- var(eigs[l_idx])
        line_close_var <- var(close[l_idx])
        
        preds <- data.frame(
          line_betw_mean, line_eigs_mean, line_close_mean,
          line_betw_var, line_eigs_var, line_close_var
        )
        
        # store team attributes for modeling
        append_df <- cbind(
          out %>% dplyr::select(week, gameId, playId, frameId, event) %>% unique(.) %>% filter(complete.cases(.)),
          'any_hurry' = hurry, 'any_sack' = sack, 'any_hit' = hit,
          preds
        )
        df_line <- rbind(df_line, append_df)
        
      }
    }
  }
  game_count <- game_count + 1
}

df_line$week <- as.integer(df_line$week)
games$week <- as.integer(games$week)


main <- df_line %>%
  left_join(plays, by = c('gameId', 'playId')) %>%
  left_join(games, by = c('week', 'gameId'))

write.csv(main, paste0('data/build',week,'.csv'))