library(pitchRx)

dat <- scrape(start = "2016-04-03", end = "2016-04-13")
test <- dat[['atbat']]
names(dat)
head(dat[[5]], 10)

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

test$batter <- as.factor(test$batter)
test$pitcher <- as.factor(test$pitcher)
test$stand <- as.factor(test$stand)
test$p_throws <- as.factor(test$p_throws)
test$event <- as.factor(test$event)
test$batter_name <- as.factor(test$batter_name)
test$pitcher_name <- as.factor(test$pitcher_name)
test$score[is.na(test$score)] <- 'F'
test$score <- as.factor(test$score)
test$home_team_runs <- as.numeric(test$home_team_runs)
test$away_team_runs <- as.numeric(test$away_team_runs)
test$date <- as.Date(test$date, format = '%Y_%m_%d')
test$road <- as.factor(test$road)
test$home <- as.factor(test$home)
test$inning_side <- as.factor(test$inning_side)
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

## Not included in outcome variable: Batters - Stolen Base, Runs
## Pitchers - Inning Pitched, Win, Earned Run Allowed, Complete Game, Complete Game Shutout

## Create batter database facing righties & lefties
## This calculates the probability of each outcome 

library(reshape2)
leftyOutcome <- filter(test, test$p_throws == 'L')
rightyOutcome <- filter(test, test$p_throws == 'R')
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


## RBIs per player per outcome type

totalOutcome <- dcast(test, batter ~ outcome, fun.aggregate = length, value.var = 'outcome')
totalOutcome <- totalOutcome[order(totalOutcome$batter),]
playerRBI <- dcast(test, batter ~ outcome , fun.aggregate = sum, value.var = 'rbi')
playerRBI <- playerRBI[order(playerRBI$batter_team),]

for (i in 2:length(playerRBI)) {
  playerRBI[[i]] <- ifelse(totalOutcome[[i]] == 0, 0, playerRBI[[i]] / totalOutcome[[i]])
}

## Pitcher stats - same as for batter, find probability of each outcome by righty/lefty

pitchRightyOutcome <- filter(test, stand == 'R')
pitchLeftyOutcome <- filter(test, stand == 'L')

pitchRightyOutcome <- dcast(pitchRightyOutcome, pitcher ~ outcome, fun.aggregate = length, value.var = 'outcome')
pitchRightyOutcome$total <- pitchRightyOutcome$`1` + pitchRightyOutcome$`2` + pitchRightyOutcome$`3` + pitchRightyOutcome$`4` + pitchRightyOutcome$`5` + pitchRightyOutcome$`6`+ pitchRightyOutcome$`7`
pitchLeftyOutcome <- dcast(pitchLeftyOutcome, pitcher ~ outcome, fun.aggregate = length, value.var = 'outcome')
pitchLeftyOutcome$total <- pitchLeftyOutcome$`1` + pitchLeftyOutcome$`2` + pitchLeftyOutcome$`3` + pitchLeftyOutcome$`4` + pitchLeftyOutcome$`5` + pitchLeftyOutcome$`6`+ pitchLeftyOutcome$`7`

pitchRightyOutcome <- perAB(pitchRightyOutcome)
pitchRightyOutcome$stand <- 'R'
pitchLeftyOutcome <- perAB(pitchLeftyOutcome)
pitchLeftyOutcome$stand <- 'L'

## Create one dataset used for modeling that has batter & pitcher stats for each at bat, as well as outcome

modelDF <- test[,c('batter', 'pitcher', 'stand', 'p_throws', 'home', 'outcome')]
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

head(modelDF)
     ## TO DO - turn into function, merge with lineup data, pull in 2015 data

