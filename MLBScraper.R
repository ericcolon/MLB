salaryData <- function(dkURL, yahooURL)

  dkURL <- 'https://www.draftkings.com/lineup/getavailableplayerscsv?contestTypeId=28&draftGroupId=9218'
  yahooURL <- 'https://dfyql-ro.sports.yahoo.com/v2/export/contestPlayers?contestId=1176756'

  dkData <- read.csv(url(dkURL), stringsAsFactors = F)
  yahooData <- read.csv(url(yahooURL), stringsAsFactors = F)
  dkData$Name <- iconv(dkData$Name, "latin1", "UTF-8")
  dkData$Name <- chartr('áéóñ', 'aeon', dkData$Name)
  dkData$Name <- gsub('\\.', '', dkData$Name)
  dkData$teamAbbrev <- toupper(dkData$teamAbbrev)
  yahooData$First.Name <- gsub('\\.', '', yahooData$First.Name)
  yahooData$Last.Name <- gsub('\\.', '', yahooData$Last.Name)
  yahooData$Name <- paste0(yahooData$First.Name," ", yahooData$Last.Name)

  unique(dkData$teamAbbrev)
  unique(yahooData$Team)
  
  library(rvest)
  library(splitstackshape)
  
  teamsURL <- read_html('http://mlb.mlb.com/team/index.jsp')
  teams <- as.data.frame(teamsURL %>%
           html_nodes('h5 a') %>%
           html_text(), stringsAsFactors = F)
  teams$abbrev <- c('BAL', 'BOS', 'CWS', 'CLE', 'DET', 'HOU', 'KC', 'LAA', 'MIN', 'NYY',
                    'OAK', 'SEA', 'TB', 'TEX', 'TOR', 'ARI', 'ATL', 'CHC', 'CIN', 'COL',
                    'LAD', 'MIA', 'MIL', 'NYM', 'PHI' ,'PIT', 'SD', 'SF', 'STL', 'WAS')
  
  teamsSite <- c('orioles','redsox','whitesox','indians','tigers','astros','royals','angels','twins',
                 'yankees','athletics','mariners','rays','rangers','bluejays','dbacks','braves',
                 'cubs','reds','rockies','dodgers','marlins','brewers','mets','phillies','pirates','padres',
                 'giants','cardinals','nationals')
  
  rosterList <- list()
  
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

  rosters <- do.call(rbind.data.frame, rosterList)
  colnames(rosters) <- c('team', 'Name')

  
  date <- gsub('-','/',Sys.Date())
  ppURL <- read_html(paste0('http://mlb.mlb.com/news/probable_pitchers/?c_id=mlb&date=',date))
  allSP <- ppURL %>%
           html_nodes('h5 span , h5 a, h5 a, h4') %>%
           html_text()
  allSP <- allSP[7:length(allSP)]
  allSP <- data.frame(matrix(allSP,
                             nrow = (length(allSP)/5),
                             byrow = T))
  allSP <- as.data.frame(cSplit(allSP, 'X1', sep='@'))
  homeStarters <- allSP[,c(1:2,5:6)]
  roadStarters <- allSP[,c(3:4,6,5)]
  colnames(homeStarters) <- c('SP', 'hand', 'team', 'opponent')
  colnames(roadStarters) <- c('SP', 'hand', 'team', 'opponent')
  probSP <- rbind(homeStarters, roadStarters)
  
  ### Position Player Lineups
  
  posPlayerURL <- read_html('http://www.rotowire.com/baseball/daily_lineups.htm')
  posPlayer <- posPlayerURL %>%
               html_nodes('.dlineups-hplayer a , .dlineups-mainbar-home a, .dlineups-mainbar-away a, .dlineups-vplayer a') %>%
               html_text()
  posPlayer <- data.frame(matrix(posPlayer,
                                 nrow = (length(posPlayer)/20),
                                 byrow =T))
  roadTeam <- posPlayer[,c(1:11)]
  colnames(roadTeam) <- c('team', 'opponent','One','Two','Three','Four','Five','Six','Seven','Eight','Nine')
  roadTeam <- melt(roadTeam, id.vars=c('team', 'opponent'))
  roadTeam <- roadTeam[order(roadTeam$team),]
  homeTeam <- posPlayer[,c(1:2,12:20)]
  colnames(homeTeam) <- c('team', 'opponent','One','Two','Three','Four','Five','Six','Seven','Eight','Nine')
  homeTeam <- melt(homeTeam, id.vars=c('team', 'opponent'))
  homeTeam <- homeTeam[order(roadTeam$team),]
  
  weatherURL <- read_html('http://www.rotowire.com/baseball/weather.htm', encoding = "UTF-8")
  weather <- weatherURL %>%
             html_nodes('.weatherfeed-notes , .weatherfeed-goodweather, .weatherfeed-teams') %>%
             html_text(trim =T)
  
  ####### ROTOGRINDERS - MAY BE BEHIND PAYWALL, SO KEEP EVERYTHING ABOVE ###########

  dateGrinders <- Sys.Date()
  grindersURL <- read_html(paste0('https://rotogrinders.com/lineups/mlb?date=',dateGrinders,'&site=draftkings'))
  grinders <- grindersURL %>%
              html_nodes('.lng , .meta, .stats .salary, .player-popup') %>%
              html_text(trim =T)
  grinders[grinders==""] <- '0'
  grinders <- gsub('\\$', '', grinders)
  grinders <- gsub('\\n', '', grinders)
  grinders <- gsub("                        ", "_", grinders)
  grinders <- gsub("                            ", "_", grinders)
  grinders <- as.data.frame(matrix(grinders, nrow = (length(grinders)/42), byrow=T))
  head(grinders)
  awayGrinders <- grinders[,1:22]
  homeGrinders <- grinders[,c(2,1,23:42)]
    

  
  
   
  
  


  
## TO DO - Turn into massive function
## Figure out what to do with weather data
## Scrap everything and use the rotogrinders data

  