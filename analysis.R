library(tidyverse)
library(data.table)
library(lme4)
library(sfsmisc)
library(latex2exp)
library(nflfastR)
library(ggimage)
library(ggthemes) 
library(gganimate)
library(cowplot)
library(teamcolors) 

##### 01. Data load #####
data_str <- 'https://raw.githubusercontent.com/bscod27/big-man-betweenness/main/builds/'
main <- data.frame()
for (i in 1:8) {
  main <- rbind(main, fread(paste0(data_str, 'build', i, '.csv')))
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

rolled %>%
  dplyr::select(week, gameId, playId, pos_team, def_team, down, yardstogo, contains('def'), line_betw, pressure) %>%
  write.csv(., './snippets/rolled.csv', row.names = FALSE)

##### 04. Create BMB metric  #####
summary(mod <- lmer(
  sqrt(line_betw) ~ def_playersinbox + (1|week) + (1|gameId) + (1|pos_team) + (1|def_team), 
  data=rolled))
rolled$exp <- predict(mod, rolled)
rolled$oe <- sqrt(rolled$line_betw)/rolled$exp

png('./images/sampling_distribution.png', units='in', width=11, height=5, res=700)
par(mfrow=c(1, 2))
plot(
  density(rolled$oe, bw=.065), 
  main='Probability Density Function', 
  col='blue', 
  xlab='Quantile '
  )
mu <- mean(rolled$oe); sig <- sd(rolled$oe)
x <- seq(0, 2, length.out=10000)
y_norm <- dnorm(x, mu, sig)
lines(x, y_norm, col='red')
legend(
  x='right', col=c('blue', 'red'), lty=1,
  legend=c('Empirical', 'Normal Approx.')
  )
abline(h=0, col='grey')

ecdf.ksCI(
  rolled$oe, main="Cumulative Distribution Function",
  col='red', ci.col=NA, xlab='Quantile', ylab='Probability', 
  )
p <- seq(0, 1, length.out=1000)
quantile <- qnorm(p, mean = mu, sd = sig)
f_x <- pnorm(quantile, mu, sig)
lines(quantile, f_x, col='blue')
legend(
  x='right', col=c('blue', 'red'), lty=1,
  legend=c('Empirical', 'Normal Approx.')
)
abline(h=1, col='grey')
abline(h=0, col='grey')
dev.off()

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

asp_ratio <- 1.6

# rank-order
tr <- sorted %>%
  left_join(nflfastR::teams_colors_logos, by = c('pos_team' = 'team_abbr')) %>% 
  ggplot(aes(x = reorder(pos_team, -avg_bmb), y = avg_bmb)) +
  geom_image(aes(image = team_logo_wikipedia), size = 0.035, by = "width", asp = asp_ratio) +
  scale_fill_identity(aesthetics = c("fill", "color")) +
  labs(
    x = 'Rank', y='BMB', title='Rank ordered O-lines'
  ) + 
  scale_x_discrete(labels = 1:32)+
  theme_classic() +
  theme(
    aspect.ratio = 1/asp_ratio,
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_hline(yintercept = 1, color='red', linetype = 'dashed') +
  geom_hline(yintercept = 1.02, color='grey', linetype = 'dashed') +
  geom_hline(yintercept = 0.98, color='grey', linetype = 'dashed') + 
  annotate("text", x=5, y=1.045, label= '"Tier 1"') +
  annotate("text", x=16.5, y=1.01, label= '"Tier 2"') +
  annotate("text", x=27.5, y=0.96, label= '"Tier 3"')
  
ggsave("./images/team_ratings.png", tr, height = 5, width = 7)

# matrix
mat <- team %>%
  left_join(nflfastR::teams_colors_logos, by = c('pos_team' = 'team_abbr')) %>% 
  ggplot(aes(x = avg_exp_betw, y = avg_sqrt_betw)) +
  geom_image(aes(image = team_logo_wikipedia), size = 0.035, by = "width", asp = asp_ratio) +
  scale_fill_identity(aesthetics = c("fill", "color")) +
  labs(
    x = TeX('Expected $\\sqrt{O-line \\ betweenness}$'),
    y=TeX('Observed $\\sqrt{O-line \\ betweenness}$'), 
    title='O-line Pass Protection Efficiency Matrix'
  ) +
  theme_classic() +
  theme(
    aspect.ratio = 1/asp_ratio, 
    plot.title = element_text(hjust = 0.5)
  ) + 
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed') + 
  geom_vline(xintercept = mean(team$avg_exp_betw), color = 'red', linetype = 'dashed')

ggsave("./images/pp_matrix.png", mat, height = 5, width = 7)

##### 07. Probabilities of success #####
probs <- rolled %>% mutate(prob = pnorm(oe, mu, sig)) 

probs %>% 
  filter(pos_team == 'DAL') %>% 
  group_by(Coverage = def_coverage) %>% 
  summarize('Probability' = mean(prob)) %>% 
  arrange(desc(Probability)) 

probs %>% 
  filter(pos_team == 'DAL') %>% 
  group_by(Down = down) %>% 
  summarize('Probability' = mean(prob)) %>% 
  arrange(desc(Probability)) 

probs %>% 
  filter(pos_team == 'DAL') %>% 
  group_by('Coverage type' = def_covtype) %>% 
  summarize('Probability' = mean(prob)) %>% 
  arrange(desc(Probability)) 

##### 08. Play animation #####
# citation: http://rstudio-pubs-static.s3.amazonaws.com/494188_154898728be3411ebab39050ca8a8dcd.html
df$def_team <- df$desc_defteam
df$pos_team <- df$desc_posteam
df$exp <- predict(mod, df)
df$oe <- sqrt(df$line_betw_mean)/df$exp

Get.Animation <- function(week=1, game, play) {
  example <- df %>%  
    filter(week==week, gameId==game, playId==play) %>%
    mutate(prob = pnorm(oe, mu, sig)) %>% 
    dplyr::select(week, gameId, playId, frameId, oe, prob, desc_play)
  
  tracking_example <- fread('https://raw.githubusercontent.com/bscod27/big-man-betweenness/main/data/week1.gz')
  games_sum <- read_csv('https://raw.githubusercontent.com/bscod27/big-man-betweenness/main/data/games.csv') 
  plays_sum <- read_csv('https://raw.githubusercontent.com/bscod27/big-man-betweenness/main/data/plays.csv') 
  
  out <- tracking_example %>% 
    inner_join(games_sum) %>% 
    inner_join(plays_sum) %>% 
    filter(week==week, gameId==game, playId == play) %>% 
    left_join(example %>% dplyr::select(frameId, oe), by='frameId') %>% 
    rename(bmb=oe) %>% 
    mutate(bmb_color=ifelse(bmb>=1, 'black', 'brown'))
  
  return(out)
}

# ggplot2 data
example <- Get.Animation(game=2021091201, play=1367) 

# plotly data
plotly_stats <- df %>%  
  filter(week==1, gameId==2021091207, playId==3828) %>%
  mutate(prob = pnorm(oe, mu, sig)) %>% 
  dplyr::select(week, gameId, playId, frameId, oe, prob, desc_play) 

write.csv(plotly_stats, './gifs/plotly_stats.csv', row.names=FALSE)

# ggplot2 visualization
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

ymin <- 60; ymax <- 120
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

animate.play <- ggplot() +
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) +
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c('#00338D', '#654321', '#FFB612'), guide = FALSE) +
  scale_color_manual(values = c('#00338D', '#654321', '#FFB612'), guide = FALSE) +
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), color = "black") + 
  geom_point(data = example, aes(x = (xmax-y), y = x, size = '72x72',
                                      fill = team, group = nflId, color = team), alpha = 0.7) +
  geom_text(data = example, aes(x = (xmax-y), y = x, label = jerseyNumber), color = "white",
            vjust = 0.36, size = 3.5) +
  geom_text(data = example,  aes(x = 26.5, y=115, label = 
                                        paste0('Big Man Betweenness: ',format(signif(bmb,digits=2),nsmall=2))),
            color=example$bmb_color, size = 5.5) +
  ylim(ymin, ymax) +
  coord_fixed() +  
  theme_nothing() + 
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL

anim_save('./gifs/ggplot2_anim.gif', animate.play, units = 'in', height=5, width=5, res=150)
