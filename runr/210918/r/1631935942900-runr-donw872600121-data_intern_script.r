
# load data - update this line to load the data locally
load(file = '../player_game_stats.rda')
head(player_game_stats)
dim(player_game_stats)

# add booleans for double-double, triple-double
player_game_stats <- player_game_stats %>% 
  dplyr::mutate(doubleCounts = ifelse(ptsScored >= 10, 1, 0) + ifelse(ast >= 10, 1, 0) + ifelse(reb >= 10, 1, 0) + ifelse(blk >= 10, 1, 0) + ifelse(stl >= 10, 1, 0)) %>%
  dplyr::mutate(isDoubleDouble = doubleCounts >= 2) %>%
  dplyr::mutate(isTripleDouble = doubleCounts >= 3) %>%
  dplyr::mutate(is30Pts = ptsScored >= 30)


# Task:
# Do a short analysis  (<50 lines of additional code) and write-up (500 words max) on double-double, triple-double, and/or frequency and trends. Could look at:
  # year-to-year, are these events more or less frequent?
  # do certain players, teams stand out?
  # could perhaps look at other types of events (40 pt games, 10+ block or steal games, 5x5 games)

# This is mainly a test of your ability to: 
  # perform basic basketball stats analysis
  # keep the analysis short
  # articulate the results clearly in a write-up that college coaches could understand (the important part)

# Note competitionIds: 22850 = Mens 2018-2019, 24996 = Mens 2019-2020, 27693 = Mens 2020-2021
