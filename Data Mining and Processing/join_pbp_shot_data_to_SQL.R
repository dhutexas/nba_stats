require(tidyverse)
require(magrittr)
library(DBI)
library(dbplyr)

# pull in config file to hide passwords
dw <- config::get("datawarehouse_nba")

con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = dw$dbname,
                      host = dw$networkhost,
                      port    = dw$port,
                      user    = dw$user,
                      password   = dw$password
)

a <- DBI::Id(
  schema  = "pbp",
  table   = "2020-21"
)

b <- DBI::Id(
  schema = 'shots',
  table = 'stats_nba_shots_all_2020-21.csv'
)

pbp <- DBI::dbReadTable(con, a)
shots <- DBI::dbReadTable(con, b)


# tables in postgres have special characters (numbers and hyphens)
# so need to give special treatment when writing code for them
# here wrap in paste function, with variables to hold odd quotes
tableA = 'pbp."2020-21"'
tableB = 'shots."stats_nba_shots_all_2020-21.csv"'

query <- paste('SELECT * FROM ',tableA,' a 
               FULL OUTER JOIN ',tableB,' b 
               ON a."GAME_ID" = b."GAME_ID" 
                AND a."EVENTNUM" = b."GAME_EVENT_ID"
                AND a."PERIOD" = b."PERIOD";')

# merge and pull in pbp and shots
# drop index (which then allows to drop duplicates)
# then convert some columns
# then group by and arrange by period, time left in period (PCTIMESTRING)
# eventnum is NOT reliable indicator of when action took place in the game
shots_pbp <- dbGetQuery(con, query) %>%
  select(-c(starts_with('index'))) %>%
  distinct() %>%
  mutate(EVENTNUM = as.integer(as.factor(EVENTNUM)),
         PCTIMESTRING = as.character(strptime(PCTIMESTRING, "%M:%S"), "%M:%S")) %>%
  group_by(GAME_ID) %>%
  arrange(GAME_ID, PERIOD, desc(PCTIMESTRING)) %>%
  filter(!str_detect(PERIOD, 'PERIOD')) # get rid of bonus rows of non-data


##### CALCULATE STATS ######
# points scored by shot
shots_pbp %<>%
  mutate(points = case_when(
    EVENTMSGTYPE == 1 & (str_detect(HOMEDESCRIPTION, "3PT") | str_detect(VISITORDESCRIPTION, "3PT")) ~ 3,
    EVENTMSGTYPE == 1 & (str_detect(HOMEDESCRIPTION, "3PT", negate = TRUE) | str_detect(VISITORDESCRIPTION, "3PT", negate = TRUE)) ~ 2,
    EVENTMSGTYPE == 3 & (str_detect(HOMEDESCRIPTION, "MISS", negate = TRUE) | str_detect(VISITORDESCRIPTION, "MISS", negate = TRUE)) ~ 1,
    TRUE ~ 0))

# code misses
shots_pbp %<>%
  mutate(miss = case_when(
    EVENTMSGTYPE == 2 ~ 1,
    TRUE ~ 0
  ))

# code free-throw attempts
shots_pbp %<>%
  mutate(free_throw_att = case_when(EVENTMSGTYPE == '3' ~ 1, TRUE ~ 0),
         free_throw_shooter = ifelse(EVENTMSGTYPE == '3', as.character(PLAYER1_NAME), NA))

# code free-throw makes
shots_pbp %<>%
  mutate(free_throw_made = case_when((EVENTMSGTYPE == '3' & points == 1) ~ 1, TRUE ~ 0))

# code three-point attempts
shots_pbp %<>%
  mutate(
    three_point_att = case_when(
    (str_detect(HOMEDESCRIPTION, "3 PT") | str_detect(VISITORDESCRIPTION, "3 PT") |
       str_detect(HOMEDESCRIPTION, "3PT") | str_detect(VISITORDESCRIPTION, "3PT")) ~ 1,
    TRUE ~ 0),
    three_point_shooter = ifelse(
      (str_detect(HOMEDESCRIPTION, "3 PT") | str_detect(VISITORDESCRIPTION, "3 PT") |
         str_detect(HOMEDESCRIPTION, "3PT") | str_detect(VISITORDESCRIPTION, "3PT")), 
      as.character(PLAYER1_NAME),
      NA))

# code three-point makes
shots_pbp %<>%
  mutate(three_point_made = case_when((points == 3) ~ 1, TRUE ~ 0))

# code two-point makes
shots_pbp %<>%
  mutate(two_point_made = case_when((points == 2) ~ 1, TRUE ~ 0),
         two_point_shooter = ifelse(
           SHOT_TYPE == '2PT Field Goal', 
           as.character(PLAYER1_NAME),
           NA))

# code two-point misses
shots_pbp %<>%
  mutate(two_point_att = case_when((SHOT_ATTEMPTED_FLAG == 1 & three_point_att != 1) ~ 1,
         TRUE ~ 0))

# code rebounds, generally
shots_pbp %<>%
  mutate(rebound = case_when(
    EVENTMSGTYPE == 4 ~ 1,
    TRUE ~ 0
  ))

# offensive rebounds
shots_pbp %<>%
  #arrange(index) %>%
  group_by(GAME_ID) %>%
  mutate(o_reb = case_when(
    (lag(str_detect(HOMEDESCRIPTION, "MISS")) & str_detect(HOMEDESCRIPTION, "REBOUND")) |
      (lag(str_detect(VISITORDESCRIPTION, "MISS")) & str_detect(VISITORDESCRIPTION, "REBOUND")) ~ 1,
    TRUE ~ 0),
    oreb_player = ifelse(
      o_reb == 1, 
      as.character(PLAYER1_NAME),
      NA))

# defensive rebounds
shots_pbp %<>%
  #arrange(index) %>%
  group_by(GAME_ID) %>%
  mutate(d_reb = case_when(
    (lag(str_detect(VISITORDESCRIPTION, "MISS")) & str_detect(HOMEDESCRIPTION, "REBOUND")) |
      (lag(str_detect(HOMEDESCRIPTION, "MISS")) & str_detect(VISITORDESCRIPTION, "REBOUND")) ~ 1,
    TRUE ~ 0),
    dreb_player = ifelse(
      d_reb == 1, 
      as.character(PLAYER1_NAME),
      NA))

# assists
shots_pbp %<>%
  mutate(assist = case_when(
    (str_detect(HOMEDESCRIPTION, "AST") | str_detect(VISITORDESCRIPTION, "AST")) ~ 1,
    TRUE ~ 0))

shots_pbp %<>%
  mutate(assist_passer = ifelse(
    (str_detect(HOMEDESCRIPTION, "AST") | str_detect(VISITORDESCRIPTION, "AST")), 
    as.character(PLAYER2_NAME),
    NA))

shots_pbp %<>%
  mutate(assist_scorer = ifelse(
    (str_detect(HOMEDESCRIPTION, "AST") | str_detect(VISITORDESCRIPTION, "AST")), 
    as.character(PLAYER1_NAME),
    NA))

# blocks
shots_pbp %<>%
  mutate(block = case_when(
    (str_detect(HOMEDESCRIPTION, "BLOCK") | str_detect(VISITORDESCRIPTION, "BLOCK")) ~ 1,
    TRUE ~ 0),
    block_player = ifelse(
      (str_detect(HOMEDESCRIPTION, "BLOCK") | str_detect(VISITORDESCRIPTION, "BLOCK")), 
      as.character(PLAYER3_NAME),
      NA)
  )

# steals
shots_pbp %<>%
  mutate(steal = case_when(
    (str_detect(HOMEDESCRIPTION, "STEAL") | str_detect(VISITORDESCRIPTION, "STEAL")) ~ 1,
    TRUE ~ 0),
    steal_player = ifelse(
      (str_detect(HOMEDESCRIPTION, "STEAL") | str_detect(VISITORDESCRIPTION, "STEAL")), 
      as.character(PLAYER2_NAME),
      NA),
    steal_loser = ifelse(
      (str_detect(HOMEDESCRIPTION, "STEAL") | str_detect(VISITORDESCRIPTION, "STEAL")), 
      as.character(PLAYER1_NAME),
      NA)
  )

# turnovers
shots_pbp %<>%
  mutate(tov = case_when(
    (str_detect(HOMEDESCRIPTION, "Turnover") | str_detect(VISITORDESCRIPTION, "Turnover")) ~ 1,
    TRUE ~ 0),
    turnover_player = ifelse(
      (str_detect(HOMEDESCRIPTION, "Turnover") | str_detect(VISITORDESCRIPTION, "Turnover")), 
      as.character(PLAYER1_NAME),
      NA)
  )


# write to csv and SQL
write.csv(shots_pbp, "shots_pbp_twenty_one.csv", 
          row.names = F)

dbWriteTable(con, SQL("shots_pbp.twenty_one"), shots_pbp, overwrite=T)



###### SCORING AGGREGATION #######

# calculate scoring for the game to double-check
shots_pbp %>%
  group_by(GAME_ID, PLAYER1_NAME, PLAYER1_TEAM_ABBREVIATION) %>%
  summarize(total_points = sum(points)) %>%
  ungroup() %>%
  group_by(GAME_ID) %>%
  arrange(desc(total_points)) %>%
  arrange(desc(PLAYER1_TEAM_ABBREVIATION)) -> game_scores

# free throws
shots_pbp %>%
  group_by(GAME_ID, free_throw_shooter) %>%
  summarize(ftm = sum(free_throw_made),
            fta = sum(free_throw_att)) %>%
  ungroup() %>%
  group_by(GAME_ID) %>%
  arrange(desc(ftm)) -> ft

# twos
shots_pbp %>%
  group_by(GAME_ID, two_point_shooter) %>%
  summarize(two_pt_m = sum(two_point_made),
            two_pt_a = sum(two_point_att)) %>%
  ungroup() %>%
  group_by(GAME_ID) -> two_pt

# threes
shots_pbp %>%
  group_by(GAME_ID, three_point_shooter) %>%
  summarize(three_pt_m = sum(three_point_made),
            three_pt_a = sum(three_point_att)) %>%
  ungroup() %>%
  group_by(GAME_ID)-> three_pt

# d_reb, dreb_player
shots_pbp %>%
  group_by(GAME_ID, dreb_player) %>%
  summarize(total_dreb = sum(d_reb)) %>%
  ungroup() %>%
  group_by(GAME_ID) %>%
  arrange(desc(total_dreb)) -> drebs

# o_reb, oreb_player
shots_pbp %>%
  group_by(GAME_ID, oreb_player) %>%
  summarize(total_oreb = sum(o_reb)) %>%
  ungroup() %>%
  group_by(GAME_ID) %>%
  arrange(desc(total_oreb)) -> orebs

# assists
shots_pbp %>%
  group_by(GAME_ID, assist_passer) %>%
  summarize(assists = sum(assist)) %>%
  ungroup() %>%
  group_by(GAME_ID) %>%
  arrange(desc(assists)) -> assists_df

# blocks
shots_pbp %>%
  group_by(GAME_ID, block_player) %>%
  summarize(blocks = sum(block)) %>%
  ungroup() %>%
  group_by(GAME_ID) %>%
  arrange(desc(blocks)) -> blocks_df

# steals
shots_pbp %>%
  group_by(GAME_ID, steal_player) %>%
  summarize(steals = sum(steal)) %>%
  ungroup() %>%
  group_by(GAME_ID) %>%
  arrange(desc(steals)) -> steals_df

# tovs
shots_pbp %>%
  group_by(GAME_ID, turnover_player) %>%
  summarize(tovs = sum(tov)) %>%
  ungroup() %>%
  group_by(GAME_ID) %>%
  arrange(desc(tovs)) -> tov_df



####### SAVE AGGREGATE SCORING (BY GAME) AS NEW DF ########

# first, must join all of the dataframes
# game_scores, orebs, drebs, assists, blocks, steals, tovs


# nested left joins to add dataframes with varying column names together
left_join(game_scores, orebs,
          by = c("PLAYER1_NAME" = "oreb_player", 
                 "GAME_ID" = "GAME_ID")) %>%
  left_join(., drebs,
            by = c("PLAYER1_NAME" = "dreb_player", 
                   "GAME_ID" = "GAME_ID")) %>%
  left_join(., assists_df,
            by = c("PLAYER1_NAME" = "assist_passer", 
                   "GAME_ID" = "GAME_ID")) %>%
  left_join(., blocks_df,
            by = c("PLAYER1_NAME" = "block_player", 
                   "GAME_ID" = "GAME_ID")) %>%
  left_join(., steals_df,
            by = c("PLAYER1_NAME" = "steal_player", 
                   "GAME_ID" = "GAME_ID")) %>%
  left_join(., tov_df,
            by = c("PLAYER1_NAME" = "turnover_player", 
                   "GAME_ID" = "GAME_ID")) %>%
  left_join(., ft,
            by = c("PLAYER1_NAME" = "free_throw_shooter", 
                   "GAME_ID" = "GAME_ID")) %>%
  left_join(., two_pt,
            by = c("PLAYER1_NAME" = "two_point_shooter", 
                   "GAME_ID" = "GAME_ID")) %>%
  left_join(., three_pt,
            by = c("PLAYER1_NAME" = "three_point_shooter", 
                   "GAME_ID" = "GAME_ID")) -> game_stats


# calculate PP Scoring
game_stats %<>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>% # replace NA with 0
  mutate(PP_score = 
           (total_points * 1) + 
           (total_oreb * 1.2) +
           (total_dreb * 1.2) + 
           (assists * 1.5) + 
           (blocks * 3) +
           (steals * 3) +
           (tovs * -1),
         total_pts = 
           (ftm * 1) + (two_pt_m * 2) + (three_pt_m * 3))


# write individual game-level stats to db
dbWriteTable(con, SQL("game_stats.twenty_one"), game_stats, overwrite=T)


