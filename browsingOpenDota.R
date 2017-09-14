# api documentation
### https://docs.opendota.com/

# require packages
require(jsonlite)
require(dplyr)

# get account
account <- fromJSON('https://api.opendota.com/api/players/453785711')

# get account win/loses
### tons of parameters to configure
wl <- fromJSON('https://api.opendota.com/api/players/453785711/wl')

# get list of played heroes by an account
heroesPlayed <- fromJSON('https://api.opendota.com/api/players/453785711/heroes')

# get list of heroes
heroes <- fromJSON('https://api.opendota.com/api/heroes')

# combine heroesPlayed with heroes data and transform most important variables
heroesPlayed$hero_id <- as.numeric(heroesPlayed$hero_id)
fullHeroes <- select(heroes, id, localized_name) %>% 
              left_join(heroesPlayed, by = c('id' = 'hero_id')) %>%
              arrange(desc(games)) %>%
              mutate(winRatio = win/games) %>%
              filter(games >= 3)

fullHeroes$last_played <- as.Date(as.POSIXct(fullHeroes$last_played, origin='1970-01-01'))

# get matches where each hero got played
for(i in 1:nrow(fullHeroes)) {
    # get games for the hero
    games <- fromJSON(paste0('https://api.opendota.com/api/players/453785711/matches?hero_id=', fullHeroes[i, 'id']))
    
    # loop around the games 
    for(j in 1:nrow(games)) {
      game <- fromJSON(paste0('https://api.opendota.com/api/matches/', games[j, 'match_id']))
      
      # find player data position
      players <- game[['players']]
      position <- which(players[[2]] == games[j, 'player_slot'])
      
      # store data
      ## gold per minute
      ## exp per minute
      ## kills per minute
      ## last hits per minute
      ## hero damage per minute
      ## tower damage per minute
      bench <- players[['benchmarks']]
      playerData <- c(bench[[1]][[1]][[position]], bench[[1]][[2]][[position]],
                      bench[[2]][[1]][[position]], bench[[2]][[2]][[position]],
                      bench[[3]][[1]][[position]], bench[[3]][[2]][[position]],
                      bench[[4]][[1]][[position]], bench[[4]][[2]][[position]],
                      bench[[5]][[1]][[position]], bench[[5]][[2]][[position]],
                      bench[[6]][[1]][[position]], bench[[6]][[2]][[position]])
      
      
      # overall percentile
      percentiles <- c(playerData[2], playerData[4], playerData[6], playerData[8], playerData[10], playerData[12])
      overallPercentile <- mean(percentiles)
      summary(percentiles)
    }
}


