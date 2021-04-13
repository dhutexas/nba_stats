library(tidyLPA)
library(mclust)
library(tidyverse)
library(magrittr)
library(ggridges)
library(reshape2)

# get libraries required for bbplot
pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')

options(scipen=999)

##########################################################
### Read and process data ######
##########################################################

nba <- read.csv('https://raw.githubusercontent.com/dhutexas/nba_stats/main/Data/season_totals_all_stats_2013-20.csv',
                stringsAsFactors = F) %>%
  # only recent seasons
  filter(SEASON >= '2015-16') %>%
  # filter out those who didn't play much
  filter(MPG >= 5 & GP >= 10) %>%
  # put character data first
  relocate(where(is.numeric), .after = where(is.character)) %>%
  # add id column to match with data later
  tibble::rowid_to_column(., "id")

# variables
statsVariables <- read.csv('https://raw.githubusercontent.com/dhutexas/nba_stats/main/Data/stats_variables.csv', 
                          stringsAsFactors = F) %>%
  rename(., CategorySpecific = Category)

# after removing strongly correlated variables, leaves with 80 characteristics of play
mod_all = nba %>%
  select(PLAYER_HEIGHT_INCHES,AST,AVG_SEC_PER_TOUCH,AVG_SPEED_OFF,
         CATCH_SHOOT_PTS,DIST_MILES_OFF,DRIVE_PTS,DRIVES,ELBOW_TOUCH_PTS,ELBOW_TOUCHES,FG3A,FRONT_CT_TOUCHES,FT_AST,FTA,
         FTR,ISO_PTS,OFF_BOXOUTS,OFF_LOOSE_BALLS_RECOVERED,OREB,PACE,PAINT_TOUCH_PTS,PASSES_MADE,PASSES_RECEIVED,PFD,
         POST_TOUCHES,POTENTIAL_AST,PRBH_PTS,PRRM_PTS,PTS,PTS_2ND_CHANCE,PTS_FB,PTS_OFF_TOV,
         PTS_PAINT,PULL_UP_PTS,SCREEN_ASSISTS,SECONDARY_AST,SU_PTS,TIME_OF_POSS_36,TOUCHES,TOV,
         CATCH_SHOOT_FG_PCT,DRIVE_FG_PCT,EFG,ELBOW_TOUCH_FG_PCT,PAINT_TOUCH_FG_PCT,
         PCT_AST_2PM,PCT_AST_3PM,PCT_FGA_2PT,PCT_FGA_3PT,PCT_FGM,PCT_PTS_FT,
         PCT_PTS_OFF_TOV,PCT_PTS_PAINT,
         PCT_UAST_2PM,PCT_UAST_3PM,POST_TOUCH_FG_PCT,PTS_PER_TOUCH,PULL_UP_FG_PCT,AVG_SPEED_DEF,BLK,BLKA,
         CHARGES_DRAWN,CONTESTED_SHOTS_2PT,CONTESTED_SHOTS_3PT,D_FGA,D_FGM,DEF_BOXOUTS,DEF_LOOSE_BALLS_RECOVERED,
         DEFLECTIONS,DIST_MILES_DEF,DREB,PF,STL,D_FG_PCT,AVG_REB_DIST,DD2,LOOSE_BALLS_RECOVERED,MPG,REB_CHANCES,TD3) %>%
  scale() 

# load fitted model
model_url <- 'https://github.com/dhutexas/nba_stats/raw/main/Data/mod10fit.Rds'
mod10fit <- readRDS(url(model_url, method = "libcurl"))

### make predictions and create dataset which includes predicted classes ################
pred = predict(mod10fit$model_1_class_10$model)
nba$classPred = pred$classification

# get estimates in a new df
df.est = get_estimates(mod10fit) %>%
  # merge in better variable names
  merge(., statsVariables, by.x = 'Parameter', by.y = 'VariableShort') 

# get classes and probabilities in a df
df = get_data(mod10fit) %>%
  # add id column to match with data later
  tibble::rowid_to_column(., "id") 

# rename classes with more helpful titles
df %<>% mutate(classNames = recode(Class,
                                   '1' = 'Mover',
                                   '2' = 'Inside Man',
                                   '3' = 'Interior Passer',
                                   '4' = 'Putback Scorer',
                                   '5' = 'Catch and Shoot, Defender',
                                   '6' = 'Glue Guy',
                                   '7' = 'Opportunistic Scorer',
                                   '8' = 'Three Point Assassin',
                                   '9' = 'Spot-Up Scorer',
                                   '10' = 'Triple Double Threat'))

# rename classes with more helpful titles
df %<>% mutate(typicalPlayer = recode(Class,
                                      '1' = 'Harrison Barnes',
                                      '2' = 'Anthony Davis',
                                      '3' = 'Rudy Gobert',
                                      '4' = 'Andre Drummond (old)',
                                      '5' = 'Draymond Green',
                                      '6' = 'Lonzo Ball',
                                      '7' = 'Kawhi Leonard',
                                      '8' = 'Danny Green',
                                      '9' = 'Marcus Smart',
                                      '10' = 'LeBron James'))

# join estimates and original dataframe
fulldf = inner_join(df, nba, by='id', suffix=c('scaled','per36')) %>%
  relocate(where(is.numeric), .after = where(is.character)) %>%
  relocate(c(Class, CPROB1:CPROB10, id), .before = PLAYER_HEIGHT_INCHESscaled)

# determine class sizes (what is smallest sized cluster?)
membership <- nba %>%
  group_by(PLAYER_NAME, SEASON, classPred)

# what are the sizes of each predicted class?
table(membership$classPred)

# who is in these small classes?
nba %>%
  filter(SEASON == '2019-20') %>%
  group_by(PLAYER_NAME, SEASON, classPred) %>%
  filter(classPred == 1)

# follow a player through time
fulldf %>%
  filter(PLAYER_NAME == "Klay Thompson") %>%
  group_by(PLAYER_NAME, SEASON, classPred) %>%
  select(PLAYER_NAME, SEASON, classPred, classNames)

#write.csv(fulldf, 'LPAoutput.csv', row.names = F)
#write.csv(df.est, 'LPAclassestimates.csv', row.names = F)


##########################################################
### What do the probabilities look like? ######
##########################################################

# class probabilities per player, per season (grabs expected class and probability)
# get distribution values in long format
longdf <- fulldf %>%
  dplyr::select(PLAYER_NAME, TEAM_ABBREVIATION, SEASON, POSITION, Class,
                PLAYER_HEIGHT_INCHESscaled:TD3scaled) %>%
  reshape2::melt(., id.vars = c('PLAYER_NAME','TEAM_ABBREVIATION',
                                'SEASON','POSITION','Class'
  )) %>%
  # remove 'scaled' from variable name
  mutate(variable = gsub("scaled", "", variable, perl = TRUE)) %>% 
  # merge in better variable names
  merge(., statsVariables, by.x = 'variable', by.y = 'VariableShort') %>% 
  relocate(where(is.numeric), .after = where(is.character))


#write.csv(longdf, 'longdf.csv', row.names=F)

playerProbs <- fulldf %>%
  dplyr::select(PLAYER_NAME, SEASON, Class, CPROB1:CPROB10) %>%
  reshape2::melt(., id.vars = c('PLAYER_NAME','SEASON', 'Class')) %>%
  group_by(PLAYER_NAME, SEASON) %>%
  top_n(n=1, wt = value)

# plot distribution of predictions
probsDist <- playerProbs %>%
  ggplot() +
  aes(value) +
  geom_histogram(binwidth = 0.01, colour = "white", fill = "#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  labs(title = "Distribution of Class Probabilities")

finalise_plot(plot_name = probsDist,
              source = "n = 2,234",
              save_filepath = "class_distribution.png",
              width_pixels = 640,
              height_pixels = 450)


### plot the means of each class #####
df.est %>%
  filter(Category == 'Means') %>%
  ggplot(., 
         mapping = aes(x = Parameter, y = Estimate)) +
  geom_bar(position = position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se),
                width = .2, # width of the error bars
                position = position_dodge(0.9)) +
  facet_grid(vars(as.factor(Class))) +
  scale_color_viridis_d() +
  labs(x = "Variables", y = "Standard Deviations +/-", color = "Predicted Class",
       title = "Means Across All Variables, by Profile",
       subtitle = "Five NBA Seasons",
       caption = "NBA.com data")

###########################################
### What do the classes look like? ######
##########################################

for (i in 1:10){
  df.est %>%
    filter(Classes == 10) %>%
    filter(Category == 'Means' & Class == i) %>%
    arrange(., desc(Estimate)) %>%
    slice_head(n=10) -> top
  
  df.est %>%
    filter(Classes == 10) %>%
    filter(Category == 'Means' & Class == i) %>%
    arrange(., desc(Estimate)) %>%
    slice_tail(n=10) -> bottom
  
  p = rbind(top, bottom) %>%
    ggplot(., 
           mapping = aes(reorder(VariableLong, Estimate), Estimate)) +
    geom_bar(position = position_dodge(), 
             stat='identity', 
             fill = '#e3dfc8',
             width = 0.75, 
             alpha=0.8) + 
    geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se),
                  width = .2, # width of the error bars
                  position = position_dodge(0.9)) +
    scale_y_continuous(limits =  c(-3, 4), 
                       breaks = seq(-3, 4, by = 1)) +
    geom_hline(yintercept = 0, 
               size = 0.5, 
               color = '#00416d') +
    coord_flip() +
    bbc_style() +
    labs(y = 'Standard Deviations +/- Mean',
         x = '',
         title = paste(i)) +
    theme(axis.text.y = element_text(size = 10))
  
  print(p)
}


# plot individual classes 
df.est %>%
  filter(Classes == 10) %>%
  filter(Category == 'Means' & Class == 8) %>%
  arrange(., desc(Estimate)) %>%
  slice_head(n=10) -> top

df.est %>%
  filter(Classes == 10) %>%
  filter(Category == 'Means' & Class == 8) %>%
  arrange(., desc(Estimate)) %>%
  slice_tail(n=10) -> bottom

class10 = rbind(top, bottom) %>%
  ggplot(., 
         mapping = aes(reorder(VariableLong, Estimate), Estimate)) +
  geom_bar(position = position_dodge(), 
           stat='identity', 
           fill = '#e3dfc8',
           width = 0.75, 
           alpha=0.8) + 
  geom_errorbar(aes(ymin=Estimate-se, ymax=Estimate+se),
                width = .2, # width of the error bars
                position = position_dodge(0.9)) +
  scale_y_continuous(limits =  c(-3, 4), 
                     breaks = seq(-3, 4, by = 1)) +
  geom_hline(yintercept = 0, 
             size = 0.5, 
             color = '#00416d') +
  coord_flip() +
  bbc_style() +
  labs(y = 'Standard Deviations +/- Mean',
       x = '',
       title = 'Three Point Assassin') +
  theme(axis.text.y = element_text(size = 10))

class10

# class membership
whois = nba %>%
  filter(SEASON == '2018-19') %>%
  group_by(PLAYER_NAME, SEASON, classPred) %>%
  filter(classPred == 8)

##########################################################
### What do the variables look like across classes? ######
##########################################################

# save variables of interest
studyvars = c('PLAYER_HEIGHT_INCHESscaled','ASTscaled','AVG_SEC_PER_TOUCHscaled','AVG_SPEED_OFFscaled',
              'CATCH_SHOOT_PTSscaled','DIST_MILES_OFFscaled','DRIVE_PTSscaled','DRIVESscaled',
              'ELBOW_TOUCH_PTSscaled','ELBOW_TOUCHESscaled','FG3Ascaled','FRONT_CT_TOUCHESscaled',
              'FT_ASTscaled','FTAscaled','FTRscaled','ISO_PTSscaled','OFF_BOXOUTSscaled',
              'OFF_LOOSE_BALLS_RECOVEREDscaled','OREBscaled','PACEscaled','PAINT_TOUCH_PTSscaled',
              'PASSES_MADEscaled','PASSES_RECEIVEDscaled','PFDscaled','POST_TOUCHESscaled',
              'POTENTIAL_ASTscaled','PRBH_PTSscaled','PRRM_PTSscaled','PTSscaled','PTS_2ND_CHANCEscaled',
              'PTS_FBscaled','PTS_OFF_TOVscaled','PTS_PAINTscaled','PULL_UP_PTSscaled',
              'SCREEN_ASSISTSscaled','SECONDARY_ASTscaled','SU_PTSscaled','TIME_OF_POSS_36scaled',
              'TOUCHESscaled','TOVscaled','CATCH_SHOOT_FG_PCTscaled','DRIVE_FG_PCTscaled',
              'EFGscaled','ELBOW_TOUCH_FG_PCTscaled','PAINT_TOUCH_FG_PCTscaled','PCT_AST_2PMscaled',
              'PCT_AST_3PMscaled','PCT_FGA_2PTscaled','PCT_FGA_3PTscaled','PCT_FGMscaled','PCT_PTS_FTscaled',
              'PCT_PTS_OFF_TOVscaled','PCT_PTS_PAINTscaled','PCT_UAST_2PMscaled','PCT_UAST_3PMscaled',
              'POST_TOUCH_FG_PCTscaled','PTS_PER_TOUCHscaled','PULL_UP_FG_PCTscaled','AVG_SPEED_DEFscaled',
              'BLKscaled','BLKAscaled','CHARGES_DRAWNscaled','CONTESTED_SHOTS_2PTscaled',
              'CONTESTED_SHOTS_3PTscaled','D_FGAscaled','D_FGMscaled','DEF_BOXOUTSscaled',
              'DEF_LOOSE_BALLS_RECOVEREDscaled','DEFLECTIONSscaled','DIST_MILES_DEFscaled','DREBscaled',
              'PFscaled','STLscaled','D_FG_PCTscaled','AVG_REB_DISTscaled',
              'DD2scaled','LOOSE_BALLS_RECOVEREDscaled','MPGscaled','REB_CHANCESscaled','TD3scaled')



# try with long data
studyvars = unique(longdf$Description)
studyvars = studyvars[1:10] # practice with subset

for (i in studyvars){
  p = longdf %>%
    select(Class, value, Description) %>%
    filter(Description == i) %>%
    group_by(Class, Description) %>%
    ggplot(., aes(x = value, y = as.factor(Class))) + 
    geom_density_ridges() + 
    theme_ridges() +
    bbc_style() +
    labs(title = paste(i))
  
  print(p)
}


# print ridgeplots of all variables across all 10 classes
for (i in studyvars){
  p = fulldf %>%
    select(Class, i) %>%
    group_by(Class, !!fulldf[[i]]) %>%
    ggplot(., aes(x = !!fulldf[[i]], y = as.factor(Class))) + 
    geom_density_ridges() + 
    theme_ridges()
  
  print(p)
}

# play with plotting options for variable densities across classes
fulldf %>%
  select(typicalPlayer, PTSscaled) %>%
  group_by(typicalPlayer, PTSscaled) %>%
  ggplot(., aes(x = PTSscaled, 
                y = typicalPlayer, 
                fill = PTSscaled)) + 
  geom_density_ridges(scale = .90, 
                      fill = '#66bfbf',
                      color = '#056674',
                      quantile_lines = TRUE, 
                      quantiles = 2,
                      jittered_points = TRUE,
                      point_shape = "|", point_size = 2, size = 0.25,
                      position = position_points_jitter(height = 0)
  ) +
  geom_vline(xintercept = 0, 
             size = 0.5, 
             color = '#ed6663') +
  bbc_style() +
  labs(title = "Distribution of Standardized Metrics",
       subtitle = "ASTscaled")




##########################################################
### How do positions fit within classes? ######
##########################################################

### get classic position props ##########
positionProps = list()
for (i in 1:10){
  p = fulldf %>%
    #filter(classes_number == 8) %>%
    group_by(PLAYER_NAME, Class, POSITION) %>%
    #summarise(Value = max(Probability)) %>%
    filter(Class == i) %>%
    group_by(POSITION) %>%
    summarise(n = n()) %>%
    mutate(pct = prop.table(n),
           class = i)
  positionProps[[i]] = p
}

# gather into single dataframe (from list of lists)
positionProps = do.call(rbind, positionProps)
positionProps

# proportions of position membership
position_props = positionProps %>%
  ggplot() + 
  aes(fill= reorder(POSITION, pct), y=pct, x=as.factor(class)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual("legend", values = c("F" = "#f5f1da", "G" = "#00416d", "C" = "#f8bd7f")) +
  coord_flip() +
  bbc_style() +
  labs(title = "Mix of Traditional Positions by Cluster",
       subtitle = "Previous Five Seasons Combined") +
  theme(panel.grid.major.y=element_blank())
position_props

finalise_plot(plot_name = position_props,
              source = "Source: NBA",
              save_filepath = "position_props.png",
              width_pixels = 640,
              height_pixels = 450)

# facet plot the positions over time (by traditional classes)
posOverTime = fulldf %>%
  group_by(SEASON, Class, POSITION) %>%
  tally() %>%
  mutate(pct = prop.table(n)) 

posOverTime %>%
  group_by(SEASON, Class, POSITION) %>%
  ggplot() +
  aes(fill=POSITION, y=pct, x=SEASON) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual("legend", values = c("F" = "#f5f1da", "G" = "#00416d", "C" = "#f8bd7f")) +
  bbc_style() +
  labs(title = 'Traditional Positions Over Time',
       subtitle = 'By Percent of All Positions Per Year')

# facet plot the positions over time (by predicted classes)
classesOverTime = longdf %>%
  group_by(SEASON, Class) %>%
  tally() %>%
  mutate(pct = prop.table(n)) %>%
  ggplot() +
  aes(fill=as.factor(Class), y=pct, x=SEASON) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual("legend",
                    values = c('1' = '#03045e',
                               '2' = '#023e8a',
                               '3' = '#0077b6',
                               '4' = '#0096c7',
                               '5' = '#00b4d8',
                               '6' = '#48cae4',
                               '7' = '#90e0ef',
                               '8' = '#ade8f4',
                               '9' = '#caf0f8',
                               '10' = '#f1faee')) +
  coord_flip() +
  bbc_style() +
  labs(title = 'Mix of New Positions by Season') +
  theme(legend.position = "right")

finalise_plot(plot_name = classesOverTime,
              source = "",
              save_filepath = "classesOverTime.png",
              width_pixels = 640,
              height_pixels = 450)


