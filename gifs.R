setwd('~/01. Dartmouth/04. Coursework/05. Fall 2022/03. Big Data Bowl/04. Linked Git/')

library(tidyverse)
library(igraph)
library(lme4)
library(latex2exp)
library(magick)

euclidean_dist <- function(x, y) {sqrt(sum((x - y)^2))}

##### 00. Read in the data #####
games <- read.csv('data/games.csv')
plays <- read.csv('data/plays.csv')
players <- read.csv('data/players.csv')
pffScoutingData <- read.csv('data/pffScoutingData.csv')

tracking <- data.frame()
for (i in list.files(path = 'data', pattern='week')) {
  df <- read.csv(paste0('data/', i))
  df$week <- as.numeric(substr(i, 5, 5))
  tracking <- rbind(tracking, df)
}

##### 01. Join all data into main frame #####
df <- tracking %>%
  left_join(players, by = 'nflId') %>%
  left_join(plays, by = c('gameId', 'playId')) %>%
  left_join(pffScoutingData, by = c('gameId', 'playId', 'nflId')) %>%
  left_join(games, by = c('week', 'gameId'))

rm(tracking)

##### 02. Loop through tracking and define networks for each play #####
dir.create('data/betw')
dir.create('data/pos')

for (week in 1:length(unique(df$week))) { # unique weeks
  print(paste0('Week: ',week))
  sliced <- df[df$week == week, ]
  
  game_count <- 1
  for (game in unique(sliced$gameId)) {
    print(paste0(' Game: ',game_count,'/',length(unique(sliced$gameId))))
    temp <- sliced %>% filter(gameId == game)
    
    for (play in unique(temp$playId)[19]) { # want play 19 -- check this
      print(play)
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
          
          # plot 1
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
          V(g)$label.cex = .5
          
          # first
          png(paste0('data/pos/image_',ifelse(str_length(frame)==1, paste0('0',frame), frame),'.png'), units='in', height=5, width=5,res=300)
          plot(g, layout=l, vertex.label=V(g)$pos, vertex.frame.color=NA)
          dev.off()
          
          # second
          l_idx <- which(names(V(g)) %in% line_slice$player)
          betw <- betweenness(g, normalized = T)
          V(g)$betw <- betw
          png(paste0('data/betw/image_',ifelse(str_length(frame)==1, paste0('0',frame), frame),'.png'), units='in', height=5, width=5,res=300)
          plot(g, layout=l, vertex.label=V(g)$pos, vertex.frame.color=NA)
          text(
            0, 1.25, TeX(
            paste('$\\sqrt{O-line \\ betweenness} = $',format(signif(mean(sqrt(betw[l_idx])),digits=10), nsmall=10))
              )
            )
          dev.off()
        }
      }
      break
    }
    game_count <- game_count + 1
    break
  }
  break
}

##### 03. Produce animations #####
create_animation <- function(arg) {
  list.files(path = paste0('data/', arg), pattern = '.png', full.names = TRUE) %>% 
    image_read() %>% 
    image_crop(., '2100x2000+300+200') %>%
    image_join() %>% 
    image_animate(fps = 4) %>% 
    image_write(paste0('gifs/',arg,'_anim.gif'))
}

create_animation('pos')
create_animation('betw')
