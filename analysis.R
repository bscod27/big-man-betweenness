library(tidyverse)
library(lme4)

setwd('~/01. Dartmouth/04. Coursework/05. Fall 2022/03. Big Data Bowl/03. Linked Git/builds')

##### 01. Data load #####
main <- data.frame()
for (i in list.files(pattern='.csv')) {
  print(paste0(i,'...'))
  dat <- read.csv(i) %>% dplyr::select(-X)
  name <- str_match(i, '(.*).csv')[2]
  assign(name, dat)
  main <- rbind(main, dat)
  rm(dat)
}

##### 02. Data wrangling #####
main %>% 
  dplyr::select(-season,-yardlineSide,-contains(c('penalty', 'foul', 'personell'))) %>%
  filter(complete.cases(defendersInBox)) %>%
  rename(
    # play descriptives
    desc_down = down, 
    desc_play = playDescription,
    desc_yardstogo = yardsToGo, desc_posteam = possessionTeam, desc_defteam = defensiveTeam, 
    desc_yardline = yardlineNumber, desc_absyardline = absoluteYardlineNumber, desc_homescore = preSnapHomeScore,
    desc_awayscore = preSnapVisitorScore, #desc_hometeam = homeTeamAbbr, desc_awayteam = visitorTeamAbbr,
    # offensive descriptions
    off_formation = offenseFormation, off_playaction = pff_playAction, off_dropback = dropBackType, 
    # defensive descriptions
    def_playersinbox = defendersInBox, def_coverage = pff_passCoverage, def_covtype = pff_passCoverageType,
    # play results
    result_pass = passResult, result_yards = playResult, 
    # dates or times
    dt_quarter = quarter, dt_gameclock = gameClock, dt_gamedate = gameDate, dt_gametime = gameTimeEastern, 
  ) %>% 
  dplyr::select(week, gameId, playId, frameId, contains(c('any','line_','desc_','off_','def_','game_','result_','dt_'))) %>% 
  mutate(
    any_pressure = ifelse((any_hurry == 1) | (any_hit == 1) | (any_sack == 1), 1, 0),
    def_coverage = ifelse(def_coverage %in% c('Goal Line', 'Miscellaneous', 'Prevent'), 'Other', def_coverage) %>% 
      factor(.) %>% relevel(., ref = 'Other'),
    def_covtype = factor(def_covtype) %>% relevel(., ref = 'Other'),
    off_formation = ifelse(off_formation %in% c('WILDCAT', 'JUMBO'), 'OTHER', off_formation) %>% 
      factor(.) %>% relevel(., ref = 'OTHER'),
    off_dropback = 
      ifelse(off_dropback == 'DESIGNED_ROLLOUT_LEFT' | off_dropback == 'DESIGNED_ROLLOUT_RIGHT', 'designed_rollout',
      ifelse(off_dropback == 'SCRAMBLE_ROLLOUT_LEFT' | off_dropback == 'SCRAMBLE_ROLLOUT_RIGHT', 'scramble_rollout', 
      ifelse(off_dropback == 'SCRAMBLE', 'scramble', 
      ifelse(off_dropback == 'TRADITIONAL', 'traditional', 'other')))) %>% 
      factor(.) %>% relevel(ref = 'other'),
    result_pass = factor(result_pass) %>% relevel(., ref = 'C'),
    dt_gameclock = strptime(dt_gameclock, format = '%M:%S'), 
    time_minleftingame = 
      ifelse(dt_quarter == 1, difftime(dt_gameclock, strptime('00:00',format='%M:%S'))/60 + 45, 
      ifelse(dt_quarter == 2, difftime(dt_gameclock, strptime('00:00',format='%M:%S'))/60 + 30, 
      ifelse(dt_quarter == 3, difftime(dt_gameclock, strptime('00:00',format='%M:%S'))/60 + 15, 
      ifelse(dt_quarter == 4, difftime(dt_gameclock, strptime('00:00',format='%M:%S'))/60 + 0, NA)))),
  ) %>% 
  dplyr::select(-contains(c('dt_', 'result_'))) %>% 
  dplyr::select(week, gameId, playId, frameId, contains('any_'), everything()) -> df

str(df)

##### 03. Play-level roll up #####
rolled <- df %>% 
  group_by(desc_posteam, week, gameId, playId) %>% 
  dplyr::summarise(
    pressure = mean(any_pressure),
    hurry = mean(any_hurry),
    hit = mean(any_hit),
    sack = mean(any_sack),
    frame_count = n(), 
    line_betw = mean(line_betw_mean), 
    down = mean(desc_down),
    yardstogo = mean(desc_yardstogo),
    def_coverage = modeest::mlv(def_coverage) %>% factor(.),
    def_covtype = modeest::mlv(def_covtype) %>% factor(.),
    def_playersinbox = mean(def_playersinbox),
    def_team = modeest::mlv(desc_defteam),
    play = modeest::mlv(desc_play)
    ) %>% 
  rename(pos_team = desc_posteam) %>% 
  dplyr::select(
    week, gameId, playId, frame_count, pressure, hurry, hit, sack, 
    line_betw, everything()
    )

# rolled %>%
  # dplyr::select(week, gameId, playId, pos_team, def_team, down, yardstogo, contains('def'), line_betw, pressure) %>%
  # write.csv(., '../snippets/rolled.csv', row.names = FALSE)

##### 04. Create BMB metric  #####
summary(mod <- lmer(
  sqrt(line_betw) ~ def_playersinbox + (1|week) + (1|gameId) + (1|pos_team) + (1|def_team), 
  data=rolled))
rolled$exp <- predict(mod, rolled)
rolled$oe <- sqrt(rolled$line_betw)/rolled$exp

plot(
  density(rolled$oe, bw=.065), 
  main='Empirical Distribution vs. Normal Approximation for BMB', 
  col='blue')
mu <- mean(rolled$oe); sig <- sd(rolled$oe)
x <- seq(0, 2, length.out=10000)
y_norm <- dnorm(x, mu, sig)
lines(x, y_norm, col='red', lty=2)
legend(
  x='topright', col=c('blue', 'red'), lty=c(1,2),
  legend=c('Empirical Distribution', 'Normal Approximation')
  )

(delta_x <- sd(sqrt(rolled$line_betw))/mean(sqrt(rolled$line_betw)))
(delta_y <- sd(rolled$exp)/mean(rolled$exp))
(a <- 1/delta_x)
(b <- 1/delta_y)


##### 05. Statistical inference #####
Get.Coefs <- function(model) {
  summ <- summary(model)$coefficients
  est <- summ[, 1]; sterr <- summ[, 2]; zscore <- summ[, 3]
  CI.LB <- est - qnorm(0.975) * sterr
  CI.UB <- est + qnorm(0.975) * sterr
  names(CI.LB) <- 'CI.LB'
  names(CI.UB) <- 'CI.UB'
  out <- data.frame(cbind('OR'=exp(est), 'LB'=exp(CI.LB), 'UB'=exp(CI.UB))) %>% 
    mutate(Report = paste0(round(OR,2), ' [', round(LB,2),', ',round(UB,2),']')) %>% 
    dplyr::select(Report)
  return(out)
}

# model 1
summary(o1 <- glmer(
  pressure ~ oe + 
    (1|pos_team) + (1|def_team) + (1|week) + (1|gameId), 
  data = rolled, 
  family = binomial)
  )

# model 2
rolled$def_covtype = rolled$def_covtype %>% factor(.) %>% relevel(ref='Other')
summary(o2 <- glmer(
  pressure ~ oe + def_playersinbox + def_covtype +
    (1|pos_team) + (1|def_team) + (1|week) + (1|gameId), 
  data = rolled, 
  family = binomial)
  )

# model 3
rolled$down <- ifelse(rolled$down==1, '1st', ifelse(rolled$down==2, '2nd',ifelse(rolled$down==3, '3rd',
               ifelse(rolled$down==4|rolled$down==0, '4th/2pc',NA)))) %>% factor(.) %>% relevel(ref = '1st')

summary(o3 <- glmer(
  pressure ~ oe + def_playersinbox + def_covtype + yardstogo + down +
    (1|pos_team) + (1|def_team) + (1|week) + (1|gameId), 
  data = rolled, 
  family = binomial)
  )

# coeficients for table 
Get.Coefs(o1)
Get.Coefs(o2)
Get.Coefs(o3)

##### 06. Team ratings #####
team <- rolled %>%
  group_by(pos_team) %>%
  dplyr::summarise(
    avg_definbox = mean(def_playersinbox),
    avg_sqrt_betw = mean(sqrt(line_betw))
  ) 

coefs <- summary(mod)$coefficients[, 1]
team$avg_exp_betw <- coefs[1]+(coefs[2]*team$avg_definbox)
team$avg_bmb <- team$avg_sqrt_betw/team$avg_exp_betw

sorted <- team %>% 
  arrange(desc(avg_bmb)) %>% 
  mutate(index = 1:32)

# plot
par(mfrow=c(1, 2))  
plot(sorted$avg_bmb, xlab='Rank', ylab='Avg Big Man Betweenness', main='Rank-ordered Team O-lines by BMB', ylim=c(0.92, 1.05))
text(x=sorted$index, y=sorted$avg_bmb, labels=sorted$pos_team, pos=1, cex=0.75)
abline(h=1, col='red', lty='dashed')

plot(team$avg_exp_betw, team$avg_sqrt_betw, xlab='Avg Expected Betweenness', 
     ylab='Avg Square Root of Observed Betweenness', 
     main='O-line Pass Protection Efficiency Matrix')

abline(0, 1, col='red', lty='dashed')
text(x=team$avg_exp_betw, y=team$avg_sqrt_betw, labels=team$pos_team, pos=2, cex=0.75)
abline(v=mean(team$avg_exp_betw), col='red', lty='dashed')
dev.off()

##### 07. Probabilities of success #####
probs <- rolled %>% mutate(prob = pnorm(oe, mu, sig)) 

probs %>% 
  filter(pos_team == 'DAL') %>% 
  group_by(Down = down) %>% 
  summarize('Probability' = mean(prob)) %>% 
  arrange(desc(Probability)) #%>% write.csv(., '../snippets/down.csv', row.names = FALSE)

probs %>% 
  filter(pos_team == 'DAL') %>% 
  group_by(Coverage = def_coverage) %>% 
  summarize('Probability' = mean(prob)) %>% 
  arrange(desc(Probability)) #%>% write.csv(., '../snippets/defcov.csv', row.names = FALSE)

probs %>% 
  filter(pos_team == 'DAL') %>% 
  group_by('Coverage type' = def_covtype) %>% 
  summarize('Probability' = mean(prob)) %>% 
  arrange(desc(Probability)) #%>% write.csv(., '../snippets/covtype.csv', row.names = FALSE)

##### 08. Code to produce data for use case #####
df$def_team <- df$desc_defteam
df$pos_team <- df$desc_posteam
df$exp <- predict(mod, df)
df$oe <- sqrt(df$line_betw_mean)/df$exp

example <- df %>% 
  filter(week==1, gameId==2021091207, playId==3828) %>% 
  mutate(prob = pnorm(oe, mu, sig)) %>% 
  dplyr::select(week, gameId, playId, frameId, oe, prob, desc_play)

write.csv(example, '../use_case/bmb.csv', row.names = FALSE)