# lineup analysis to complement cluster model

library(tidyverse)
library(magrittr)
library(reshape2)
library(lme4)
library(lmerTest)

# get libraries required for bbplot
pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')

options(scipen=999)

# read in lineup combination data from 2015-2020 and filter out low-use lineups
# reduces from 10,000 lineup combinations over the five years to 7,612 combinations
lineups = read.csv('https://raw.githubusercontent.com/dhutexas/nba_stats/main/Data/lineups_2015-20.csv', 
                   stringsAsFactors = F) %>%
  filter(MIN >= 10 & GP >= 5) %>%
  # trim out those who didn't play much
  select(TEAM_ID:PIE, SEASON:PLAYER_NAME_5) %>%
  # add id column to match with data later
  tibble::rowid_to_column(., "LINEUP_ID") 


# get predicted classes from cluster model
preds <- read.csv('https://raw.githubusercontent.com/dhutexas/nba_stats/main/Data/fulldf.csv', 
                   stringsAsFactors = F) %>%
  select(PLAYER_NAME, classOrder, classNames, POSITION, TEAM_ABBREVIATION, SEASON)

# get player id's from original data
nba = read.csv('https://raw.githubusercontent.com/dhutexas/nba_stats/main/Data/season_totals_1996-20.csv', 
               stringsAsFactors = F) %>%
  filter(SEASON >= '2015-16') %>%
  select(PLAYER_ID, PLAYER_NAME, SEASON, TEAM_ABBREVIATION, TEAM_ID)

# join the three datasets (just need lineups now)
df = merge(preds, nba, by=c('PLAYER_NAME', 'SEASON'))

# move lineups from wide to long so can more readily match class values to data
p1 = lineups %>%
  select(-PLAYER_ID_2, -PLAYER_ID_3, -PLAYER_ID_4, PLAYER_ID_5) %>%
  reshape2::melt(., id.vars = c('LINEUP_ID', 'TEAM_ABBREVIATION', 'TEAM_ID',
                                'SEASON', 'PLAYER_ID_1')) %>%
  rename(., PLAYER_ID = PLAYER_ID_1)


p2 = lineups %>%
  select(-PLAYER_ID_1, -PLAYER_ID_3, -PLAYER_ID_4, PLAYER_ID_5) %>%
  reshape2::melt(., id.vars = c('LINEUP_ID', 'TEAM_ABBREVIATION', 'TEAM_ID',
                                'SEASON', 'PLAYER_ID_2')) %>%
  rename(., PLAYER_ID = PLAYER_ID_2)

p3 = lineups %>%
  select(-PLAYER_ID_2, -PLAYER_ID_1, -PLAYER_ID_4, PLAYER_ID_5) %>%
  reshape2::melt(., id.vars = c('LINEUP_ID', 'TEAM_ABBREVIATION', 'TEAM_ID',
                                'SEASON', 'PLAYER_ID_3')) %>%
  rename(., PLAYER_ID = PLAYER_ID_3)

p4 = lineups %>%
  select(-PLAYER_ID_2, -PLAYER_ID_3, -PLAYER_ID_1, PLAYER_ID_5) %>%
  reshape2::melt(., id.vars = c('LINEUP_ID', 'TEAM_ABBREVIATION', 'TEAM_ID',
                                'SEASON', 'PLAYER_ID_4')) %>%
  rename(., PLAYER_ID = PLAYER_ID_4)

p5 = lineups %>%
  select(-PLAYER_ID_2, -PLAYER_ID_3, -PLAYER_ID_4, PLAYER_ID_1) %>%
  reshape2::melt(., id.vars = c('LINEUP_ID', 'TEAM_ABBREVIATION', 'TEAM_ID',
                                'SEASON', 'PLAYER_ID_5')) %>%
  rename(., PLAYER_ID = PLAYER_ID_5)

# join all of these long lineup dataframes
lineupsLong = do.call("rbind", list(p1,p2,p3,p4,p5))

# join all of the data together into single dataframe
lineupsFull = merge(lineupsLong, df, by=c('PLAYER_ID', 'SEASON')) %>%
  select(-TEAM_ABBREVIATION.x, -TEAM_ABBREVIATION.y, -TEAM_ID.y)


#### ANALYSIS #######

# get long format, ordered list of position numbers, and counts of each, within each lineup
lineupsFull %>%
  select(LINEUP_ID, classNames) %>%
  group_by(LINEUP_ID, classNames) %>%
  tally() %>%
  mutate(n = n/31) %>%
  arrange(classNames, n, .by_group = TRUE)

# get long format, ordered list of the positions within each lineup
positionCombos = lineupsFull %>%
  select(LINEUP_ID, classNames, variable) %>%
  filter(variable == 'NET_RATING') %>%
  group_by(LINEUP_ID, classNames) %>%
  arrange(classNames, .by_group = TRUE) %>%
  group_by(LINEUP_ID, variable) %>%
  mutate(all_positions = paste(classNames, collapse = " | ")) %>% # create variable of composition
  ungroup() %>% # get rid of variable
  select(LINEUP_ID, all_positions) %>% # drop variable
  group_by(LINEUP_ID) %>%
  slice(1) # grab just first one for each lineup (otherwise its repeated) %>%

# what are the most common combinations?
totals = positionCombos %>%
  group_by(all_positions) %>%
  tally() %>%
  arrange(desc(n))

# get output variable + other predictors from long dataset
modelVars = lineupsFull %>%
  select(LINEUP_ID, SEASON, TEAM_ABBREVIATION, variable, value) %>%
  filter(variable == 'NET_RATING') %>%
  rename(., NET_RATING = value) %>%
  select(-variable) %>%
  group_by(LINEUP_ID) %>%
  slice(1) # grab just first one for each lineup (otherwise its repeated) %>%

# join the data (positions and predictors/outcome data)
modelData = merge(modelVars, positionCombos, by='LINEUP_ID')
modelData$NET_RATING = as.numeric(modelData$NET_RATING)




#### ESTIMATE EFFECT OF EACH POSITION #############
PositionVars = lineupsFull %>%
  select(LINEUP_ID, SEASON, TEAM_ABBREVIATION, classNames, POSITION, variable, value) %>%
  filter(variable == 'NET_RATING') %>%
  rename(., NET_RATING = value) %>%
  select(-variable)

PositionVars$NET_RATING = as.numeric(PositionVars$NET_RATING)
PositionVars$LINEUP_ID = as.factor(PositionVars$LINEUP_ID)
str(PositionVars)

# check against averages by position, generally
grpMeans = PositionVars %>%
  group_by(classNames) %>%
  summarise(mean = mean(NET_RATING))

# run model
modPosition = glmer(NET_RATING ~ classNames -1 + 
                      (1 | TEAM_ABBREVIATION:SEASON),
                    data = PositionVars)

summary(modPosition)

# grab fixed effects for positions from the model into df
posEst = as.data.frame(fixef(modPosition))
posEst$Position = rownames(posEst)
posEst$Position = gsub('classNames','',posEst$Position)

# get confidence intervals around the estimates
confInt = confint(modPosition)
confInt = as.data.frame(confInt)
confInt$Position = rownames(confInt)
confInt$Position = gsub('classNames','', confInt$Position)

# join data
posEst = merge(posEst, confInt)
posEst = merge(posEst, grpMeans, by.x = 'Position', by.y = 'classNames')

# plot fixed effects by position
p = posEst %>%
  ggplot() +
  aes(x = reorder(Position, fixef(modPosition)), y = fixef(modPosition)) +
  geom_errorbar(aes(ymin=`2.5 %`, ymax=`97.5 %`),
                width = .2, # width of the error bars
                position = position_dodge(0.9)) +
  geom_point() +
  geom_point(aes(x = Position, y = mean, shape = 8), colour='blue') +
  scale_shape_identity() +
  geom_hline(yintercept = 0, 
             size = 0.5, 
             color = '#00416d',
             alpha = 0.7) +
  scale_y_continuous(breaks = seq(0, 7, by =1)) +
  coord_flip() +
  bbc_style() +
  labs(title = 'Net Rating Estimate by Position')

p

# alternative visual (unordered)
library(sjPlot)
sjPlot::plot_model(modPosition, type = 'eff')


#### ESTIMATE BEST LINEUPS #######

#library(extraoperators)
#library(JWileymisc)
#library(multilevelTools)

# unadjusted means by lineup type (like group mean)
mod = lmer(NET_RATING ~ (1 | all_positions),
           data = modelData)
modFit = coef(mod)$all_positions

# lineup nested within teams nested within seasons
# gives specific net rating for exact lineup in exact team in exact season 
# too exact
#mod2 = lmer(NET_RATING ~  (1 | all_positions:TEAM_ABBREVIATION:SEASON),
#            data = modelData)
#summary(mod2)
#mod2Fit = coef(mod2)$all_positions

# get specific estimates for the lineups, while controlling for season and team
# but not nesting them, because don't want that specific of an estimate 
# want to go outside of teams and generalize to the lineups
mod3 = lmer(NET_RATING ~  (1 | TEAM_ABBREVIATION:SEASON) + (1 | all_positions),
            data = modelData)
summary(mod3)

mod3Fit = coef(mod3)$all_positions
mod3FitNested = coef(mod3)$TEAM_ABBREVIATION

ranef(mod3)$all_positions # just the random effects
coef(mod3)$all_positions # the coefficients (adding in the other variables to get point estimate)

# get confidence intervals of random effects
# probably a better idea to instead just run a Bayesian model
library(merTools)
randomSims <- REsim(mod3, n.sims = 5000)
# and to plot it
plotREsim(REsim(mod3, n.sims = 5000))



#### plot coefficients with highest values
mod3Fit$lineup = row.names(mod3Fit) # make linup into a column
mod3Fit %>%
  filter(`(Intercept)` > 2.4) %>%
  ggplot() +
  aes(x = reorder(lineup, `(Intercept)`), y = `(Intercept)`) +
  geom_point() + 
  geom_hline(yintercept = 0, 
             size = 0.5, 
             color = '#00416d',
             alpha = 0.7) +
  coord_flip() + 
  bbc_style() +
  theme(axis.text.y = element_text(size = 10)) +
  labs(title = 'Top Lineups by Estimated Net Rating')

#### plot coefficients with lowest values
mod3Fit %>%
  filter(`(Intercept)` < 1) %>%
  ggplot() +
  aes(x = reorder(lineup, `(Intercept)`), y = `(Intercept)`) +
  geom_point() + 
  geom_hline(yintercept = 0, 
             size = 0.5, 
             color = '#00416d',
             alpha = 0.7) +
  coord_flip() + 
  bbc_style() +
  theme(axis.text.y = element_text(size = 10)) +
  labs(title = 'Worst Lineups by Estimated Net Rating')


# visualize all random effects with estimates
devtools::install_github("m-clark/visibly")
library(visibly)
plot_coefficients(mod3, ranef=TRUE, which_ranef = 'all_positions') + coord_flip()

