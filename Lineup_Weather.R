############### MLB ROSTER AND GAMEDAY WEATHER SCRAPING #####################

## Create list of team sites (from urls) & match that with team names used by rotogrinders

teamsSite <- c('orioles','redsox','whitesox','indians','tigers','astros','royals','angels','twins',
               'yankees','athletics','mariners','rays','rangers','bluejays','dbacks','braves',
               'cubs','reds','rockies','dodgers','marlins','brewers','mets','phillies','pirates','padres',
               'giants','cardinals','nationals')
teamsGrinders <- c('Orioles', 'Red Sox', 'White Sox', 'Indians', 'Tigers', 'Astros', 'Royals', 'Angels',
                   'Twins', 'Yankees', 'Athletics', 'Mariners', 'Rays', 'Rangers', 'Blue Jays','Dbacks', 'Braves',
                   'Cubs', 'Reds', 'Rockies', 'Dodgers', 'Marlins' ,'Brewers', 'Mets', 'Phillies', 'Pirates',
                   'Padres', 'Giants', 'Cardinals','Nationals')
teamsTotal <- as.data.frame(matrix(c(teamsSite, teamsGrinders), nrow = 30))
colnames(teamsTotal) <- c('team', 'grindersTeam')

## Put active players for each team (pulling from MLB.com) into a DF
## Can use for comprehensive list of players to predict


rosterList <- list() # Initiate list

for (i in 1:30) {
  rosterURL <- read_html(paste0('http://m.',teamsSite[i],'.mlb.com/roster'))
  r <- as.data.frame(rosterURL %>%
                       html_nodes('.dg-name_display_first_last a') %>%
                       html_text(), 
                     stringsAsFactor= F)
  r$team <- teamsSite[i]
  r <- r[,c(2,1)]
  rosterList[[i]] <- r
}


## Turn list into a DF
rosters <- do.call(rbind.data.frame, rosterList) 
colnames(rosters) <- c('team', 'Name')

## Merge in rotogrinders team names - provide join key for future use
rosters <- merge(rosters, teamsTotal, by = 'team')


## For future work - pull in daily weather data
## Includes text summary - would need NLP to extract

weatherURL <- read_html('http://www.rotowire.com/baseball/weather.htm', encoding = "UTF-8")
weather <- weatherURL %>%
  html_nodes('.weatherfeed-notes , .weatherfeed-goodweather, .weatherfeed-teams') %>%
  html_text(trim =T)