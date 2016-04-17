library(pitchRx)

## Functions - run these in this sequence

## Insert desired start & end date, and directory you want to put the file in - see example "new"
## Suggest also saving as a DF (returns output in DF format)

pitchRxScraper <- function(startDate, endDate, directory) {
  dat <- scrape(start = startDate, end = endDate)
  test <- dat[['atbat']]
  names(dat)
  
  ## Select only relevant variables
  test <- test[,c('batter',
                  'pitcher',
                  'num',
                  'inning',
                  'o',
                  'stand',
                  'p_throws',
                  'event',
                  'batter_name',
                  'pitcher_name',
                  'score',
                  'home_team_runs',
                  'away_team_runs',
                  'inning_side',
                  'gameday_link',
                  'date')]
  
  
  ## Pull out team codes
  
  library(splitstackshape)
  test <- as.data.frame(cSplit(test,
                               'gameday_link',
                               sep = '_'))
  test <- test[,c(1:15,20,21)]
  colnames(test)[16:17] <- c('road', 'home')
  test$road <- substr(test$road, 1, 3)
  test$home <- substr(test$home, 1, 3)
  
  ## Convert all columns to proper format
  
  test$score[is.na(test$score)] <- 'F'
  test$home_team_runs <- as.numeric(test$home_team_runs)
  test$away_team_runs <- as.numeric(test$away_team_runs)
  test$date <- as.Date(test$date, format = '%Y_%m_%d')
  test$inning <- as.numeric(test$inning)
  test$o <- as.numeric(test$o)
  
  ## Order dataset by day by game by batter
  
  test <- test[order(test$date, test$home, test$num),]
  
  ## Note - necessary to break out outcome any more granularly? This covers all at-bat level outcomes
  ## Included in daily fantasy scoring
  
  test$outcome <- ifelse(test$event == 'Single', 1,
                         ifelse(test$event == 'Double', 2,
                                ifelse(test$event == 'Triple', 3,
                                       ifelse(test$event == 'Home Run', 4,
                                              ifelse(test$event == 'Walk' | test$event == 'Intent Walk' | test$event == 'Hit By Pitch', 5,
                                                     ifelse(test$event == 'Strikeout' | test$event == 'Strikeout - DP', 6, 7))))))
  
  ## Create variable that indicates it is the first batter for a team in a given game
  library(dplyr)
  
  test$start <- ifelse(test$num == 1, 1,
                       ifelse(lag(test$o == 3) & test$inning == 1, 1, 0))
  
  ## Create RBI variable per at bat - note: not sure how to calc runs
  
  test$rbi <- ifelse(test$start == 1 & test$inning_side == 'top', test$away_team_runs,
                     ifelse(test$start == 1 & test$inning_side == 'bottom', test$home_team_runs,
                            ifelse(test$inning_side == 'top', test$away_team_runs - lag(test$away_team_runs),
                                   test$home_team_runs - lag(test$home_team_runs))))
  
  setwd(directory)
  write.csv(test, paste0('scraper_',startDate,"_",endDate,'.csv'), row.names = FALSE)
  return(test)
}
new <- pitchRxScraper('2016-04-04','2016-04-16', '~/Documents/Northwestern/498/MLB Scraper/pitchRx Data')

## Stacks existing player database with new data - see example full data

fileStacker <- function(dfNew, fileOld, directory) {
  setwd(directory)
  a <- read.csv(fileOld, stringsAsFactors = F)
  a$date <- as.Date(a$date, format = '%Y-%m-%d')
  d <- as.data.frame(rbind(dfNew, a))
  write.csv(d, 'playerDatabase.csv', row.names = F)
  return(d)
}
fullData <- fileStacker(new, 'playerDatabase.csv', '~/Documents/Northwestern/498/MLB Scraper/pitchRx Data')

## Converts data into the proper format

dataConvert <- function(df) {
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                         as.factor)
  return(df)
}
fullData <- dataConvert(fullData)

## Creates modeling file

modelFile <- function(df) {
  library(reshape2)
  leftyOutcome <- filter(df, df$p_throws == 'L')
  rightyOutcome <- filter(df, df$p_throws == 'R')
  leftyOutcome <- dcast(leftyOutcome, batter ~ outcome, fun.aggregate = length, value.var = 'outcome')
  leftyOutcome$total <- leftyOutcome$`1` + leftyOutcome$`2` + leftyOutcome$`3` + leftyOutcome$`4` + leftyOutcome$`5` + leftyOutcome$`6`+ leftyOutcome$`7`
  rightyOutcome <- dcast(rightyOutcome, batter ~ outcome, fun.aggregate = length, value.var = 'outcome')
  rightyOutcome$total <- rightyOutcome$`1` + rightyOutcome$`2` + rightyOutcome$`3` + rightyOutcome$`4` + rightyOutcome$`5` + rightyOutcome$`6`+ rightyOutcome$`7`
  
  perAB <- function(x) {
    for( i in 2:(length(x)-1)) {
      x[[i]] <- x[[i]] / x[['total']]
    }
    return(x)
  }
  
  rightyOutcome <- perAB(rightyOutcome)
  rightyOutcome$p_throws <- 'R'
  leftyOutcome <- perAB(leftyOutcome)
  leftyOutcome$p_throws <- 'L'
  
  ## Pitcher stats - same as for batter, find probability of each outcome by righty/lefty
  
  pitchRightyOutcome <- filter(df, stand == 'R')
  pitchLeftyOutcome <- filter(df, stand == 'L')
  
  pitchRightyOutcome <- dcast(pitchRightyOutcome, pitcher ~ outcome, fun.aggregate = length, value.var = 'outcome')
  pitchRightyOutcome$total <- pitchRightyOutcome$`1` + pitchRightyOutcome$`2` + pitchRightyOutcome$`3` + pitchRightyOutcome$`4` + pitchRightyOutcome$`5` + pitchRightyOutcome$`6`+ pitchRightyOutcome$`7`
  pitchLeftyOutcome <- dcast(pitchLeftyOutcome, pitcher ~ outcome, fun.aggregate = length, value.var = 'outcome')
  pitchLeftyOutcome$total <- pitchLeftyOutcome$`1` + pitchLeftyOutcome$`2` + pitchLeftyOutcome$`3` + pitchLeftyOutcome$`4` + pitchLeftyOutcome$`5` + pitchLeftyOutcome$`6`+ pitchLeftyOutcome$`7`
  
  pitchRightyOutcome <- perAB(pitchRightyOutcome)
  pitchRightyOutcome$stand <- 'R'
  pitchLeftyOutcome <- perAB(pitchLeftyOutcome)
  pitchLeftyOutcome$stand <- 'L'
  
  ## Create one dataset used for modeling that has batter & pitcher stats for each at bat, as well as outcome
  
  modelDF <- df[,c('batter', 'pitcher', 'stand', 'p_throws', 'home', 'outcome')]
  modelDF <- merge(modelDF, rightyOutcome, by = c('batter', 'p_throws'), all.x = T)
  modelDF <- merge(modelDF, leftyOutcome, by = c('batter', 'p_throws'), all.x = T)
  modelDF$batOne <- ifelse(is.na(modelDF$`1.x`), modelDF$`1.y`, modelDF$`1.x`)
  modelDF$batTwo <- ifelse(is.na(modelDF$`2.x`), modelDF$`2.y`, modelDF$`2.x`)
  modelDF$batThree <- ifelse(is.na(modelDF$`3.x`), modelDF$`3.y`, modelDF$`3.x`)
  modelDF$batFour <- ifelse(is.na(modelDF$`4.x`), modelDF$`4.y`, modelDF$`4.x`)
  modelDF$batFive <- ifelse(is.na(modelDF$`5.x`), modelDF$`5.y`, modelDF$`5.x`)
  modelDF$batSix <- ifelse(is.na(modelDF$`6.x`), modelDF$`6.y`, modelDF$`6.x`)
  modelDF$batSeven <- ifelse(is.na(modelDF$`7.x`), modelDF$`7.y`, modelDF$`7.x`)
  modelDF$batTotal <- ifelse(is.na(modelDF$total.x), modelDF$total.y, modelDF$total.x)
  modelDF <- modelDF[,c('batter', 'pitcher', 'stand', 'p_throws', 'home',
                        'batOne', 'batTwo', 'batThree', 'batFour' ,'batFive', 'batSix', 'batSeven', 'batTotal',
                        'outcome')]
  
  modelDF <- merge(modelDF, pitchLeftyOutcome, by = c('pitcher', 'stand'), all.x = T)
  modelDF <- merge(modelDF, pitchRightyOutcome, by = c('pitcher', 'stand'), all.x = T)
  modelDF$pitchOne <- ifelse(is.na(modelDF$`1.x`), modelDF$`1.y`, modelDF$`1.x`)
  modelDF$pitchTwo <- ifelse(is.na(modelDF$`2.x`), modelDF$`2.y`, modelDF$`2.x`)
  modelDF$pitchThree <- ifelse(is.na(modelDF$`3.x`), modelDF$`3.y`, modelDF$`3.x`)
  modelDF$pitchFour <- ifelse(is.na(modelDF$`4.x`), modelDF$`4.y`, modelDF$`4.x`)
  modelDF$pitchFive <- ifelse(is.na(modelDF$`5.x`), modelDF$`5.y`, modelDF$`5.x`)
  modelDF$pitchSix <- ifelse(is.na(modelDF$`6.x`), modelDF$`6.y`, modelDF$`6.x`)
  modelDF$pitchSeven <- ifelse(is.na(modelDF$`7.x`), modelDF$`7.y`, modelDF$`7.x`)
  modelDF$pitchTotal <- ifelse(is.na(modelDF$total.x), modelDF$total.y, modelDF$total.x)
  
  
  modelDF <- modelDF[,c('batter', 'pitcher', 'stand', 'p_throws', 'home',
                        'batOne', 'batTwo', 'batThree', 'batFour' ,'batFive', 'batSix', 'batSeven', 'batTotal',
                        'pitchOne', 'pitchTwo', 'pitchThree', 'pitchFour' ,'pitchFive', 'pitchSix', 'pitchSeven', 'pitchTotal',
                        'outcome')]
  
  rounder <- function(x) {
    round(x, 3)
  }
  
  modelDF[,c(6:20)] <- as.data.frame(sapply(modelDF[,c(6:20)], rounder))
  return(modelDF)
  write.csv(modelDF, 'modelFile.csv')
}
fullData <- dataConvert(fullData)

## Creates RBI percentages - note looks like it is not working now
## Not used for modeling, so not a huge deal, will fix later

rbiPerc <- function(df) {
  ## RBIs per player per outcome type
  
  totalOutcome <- dcast(df, batter ~ outcome, fun.aggregate = length, value.var = 'outcome')
  totalOutcome <- totalOutcome[order(totalOutcome$batter),]
  playerRBI <- dcast(df, batter ~ outcome , fun.aggregate = sum, value.var = 'rbi')
  playerRBI <- playerRBI[order(playerRBI$batter),]
  
  for (i in 2:length(playerRBI)) {
    playerRBI[[i]] <- ifelse(totalOutcome[[i]] == 0, 0, playerRBI[[i]] / totalOutcome[[i]])
  }
  return(playerRBI)
}
rbiData <- rbiPerc(fullData)

## Run to match player names with their codes - not necessary for modeling, used for matching player names with RotoGrinders
## Will also have to create a manual match list between RotoGrinders names & these name

playerMatch <- function(newDF, oldFile, directory) {
  g <- read.csv(oldFile, stringsAsFactors = F)
  g <- as.data.frame(rbind(newDF, g))
  g[,2] <- ifelse(g[,1] == 666560, 'Byung Ho Park',
                  ifelse(g[,1] == 624424, 'Michael Conforto',
                         ifelse(g[,1] == 628329, 'Rusney Castiloo',
                                ifelse(g[,1] == 607074, 'Carlos Rodon', g[,2]))))
  g$dups <- duplicated(g[,1])
  g <- filter(g, dups == F)
  g$dups <- NULL
  write.csv(g, oldFile, row.names = F)
  return(g)
}
batterLookup <- playerMatch(batterLookup, 'batterLookup.csv', '~/Documents/Northwestern/498/MLB Scraper/pitchRx Data')
pitcherLookup <- playerMatch(pitcherLookup, 'pitcherLookup.csv', '~/Documents/Northwestern/498/MLB Scraper/pitchRx Data')

## Not included in outcome variable: Batters - Stolen Base, Runs
## Pitchers - Inning Pitched, Win, Earned Run Allowed, Complete Game, Complete Game Shutout

## PULL IN HISTORIC DATA - Do in sequences so it doesn't crash
## Change date (a) and for loop sequence to get previous years
## Output will be a big list housing all games over the time frame, as well as i CSV files in WD listed within the pitchRxScraper function
## Change WD there to your desired WD

myList <- list()
a <- as.Date('2015-04-05')
for (i in 1:26) {
  b <- a + 7
  myList[[i]] <- pitchRxScraper(a, b, '~/Documents/Northwestern/498/MLB Scraper/pitchRx Data')
  a <- a + 7
}







