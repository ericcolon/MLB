library(pitchRx)
library(RSQLite)
library(dplyr)
library(data.table)
library(splitstackshape)
library(reshape2)
library(DescTools)

######################  DON'T NEED TO RUN PAST THIS UNLESS RUNNING HISTORIC PULL  ##################

## Use these to go back and get old data for pitch - atbat currently on github

## Create an empty csv file that has column names that match the below (ignore the numbers)

setwd('~/Documents/Northwestern/498/MLB Scraper/pitchRx Data/new')

##  [1] "type"         "x"            "y"            "event_num"    "start_speed" 
## [6] "pfx_x"        "pfx_z"        "px"           "pz"           "x0"          
## [11] "z0"           "vx0"          "vy0"          "vz0"          "ax"          
## [16] "ay"           "az"           "break_y"      "break_angle"  "break_length"
## [21] "pitch_type"   "zone"         "spin_dir"     "spin_rate"    "pitcher"     
## [26] "bb"           "ot"    

date <- as.Date('2015-04-05')
for (i in 1:26) {
  add <- date + 7
  e <- pitchRxScraper(date, add)
  pitch <- pitchRxPitch('pitch.csv', e, '~/Documents/Northwestern/498/MLB Scraper/pitchRx Data/new', paste0(date,'_',add))
  date <- date + 7
}

date <- as.Date('2016-04-04')
for (i in 1:4) {
  date2 <- date + 7
  pitch <- pitchRxPitch('pitch.csv', e, '~/Documents/Northwestern/498/MLB Scraper/pitchRx Data/new', paste0(date,'_',add))
  date <- date + 7
}

################################ MLB Data Pull - Using Pitch fX dataset #################################

## To start - make sure you have full pitch.csv & atbat.csv files at your desired working directory
## By "full", I mean all data prior to the date range you put in for pitchRxScraper
## All other data can be generated within this script, but just need to have that baseline
## The code at the very bottom loops through historic data (for 2015 and through 5/1 2016)

## Insert desired start & end date, and directory you want to put the file in - see example "new"

setwd('~/Documents/Northwestern/498/MLB Scraper/pitchRx Data/new')

## Function to scrape data from pitch Fx database - saved as df name 'dat'
## Change to include new data - note at start: Atbat.csv runs through 5/7. To sync up, after running the code to get historic data
## run the below code and then JUST pitchRxPitch for 2016-05-02 - 2016-05-07. From that point, when you add new days run the full code
## To also update atbat.csv in addition to pitch.csv

pitchRxScraper <- function(startDate, endDate) {
  dat <- scrape(start = startDate, end = endDate)
  return(dat)
}
dat <- pitchRxScraper('2016-05-02', '2016-05-07')

## Create pitcher & at-bat files - these will be saved to your working directory
## For the pitch data a total file will be saved, as well as a file for the time range you ran pitchRxScraper
## For the at bat data, totalfile must be merged with new data - occurs in lines 101-104
## Line 105 writes a new total at bat file

pitchRxPitch <- function(existing, newFile, directory, daterange){
  pitch <- new[['pitch']]
  test <- new[['atbat']]
  pitch$break_y <- as.numeric(pitch$break_y)
  pitch$break_angle <- as.numeric(pitch$break_angle)
  pitch$break_length <- as.numeric(pitch$break_length)
  pitch$pitch_type <- as.factor(pitch$pitch_type)
  pitch <- pitch %>% filter(!pitch_type %in% c('UN','PO','FO','IN', NA))
  pitch$type <-  ifelse(pitch$des %in% c('Swinging Strike', 'Missed Bunt', 'Swinging Strike (Blocked'), 'SS', pitch$type)
  pitch <- merge(pitch, test[,c('num', 'gameday_link', 'pitcher')], by = c('gameday_link', 'num'))
  pitch$bb <- ifelse(pitch$pitch_type %in% c('SL', 'CB', 'CU', 'KC', 'KN', 'EP', 'CH'), 1, 0)
  pitch$ot <- ifelse(pitch$bb == 1, 0, 1)
  drops <- c('des','des_es','tfs','tfs_zulu','sv_id','play_guid','end_speed',
             'sz_top','sz_bot','y0','nasty','cc','mt','inning_side','inning','next_','on_1b',
             'on_2b','on_3b','count','url', 'gameday_link','num','id','type_confidence')
  pitch <-  pitch[,!(names(pitch) %in% drops)]

  ## read in existing data & save new data to individual CSV file
  setwd(directory)
  write.csv(pitch, paste0(daterange,'_pitch.csv'), row.names = F)
  o <- read.csv(existing, stringsAsFactors = F)
  
  ## Stack old & new data to create new full dataset
  
  pitch <- as.data.frame(rbind(o, pitch))
  write.csv(pitch, 'pitch.csv', row.names = F)
  return(pitch)
}
pitchRxAtBat <- function(newFile, directory, daterange) {
  data <- newFile[['atbat']]
  data <- data[,c('batter', 'pitcher', 'num', 'inning', 'o', 'stand', 'p_throws', 'event', 'batter_name',
                  'pitcher_name', 'score', 'home_team_runs', 'away_team_runs', 'inning_side', 'gameday_link',
                  'date')]
  
  library(splitstackshape)
  data <- as.data.frame(cSplit(data,
                               'gameday_link',
                               sep = '_'))
  data <- data[,c(1:15,20,21)]
  colnames(data)[16:17] <- c('road', 'home')
  data$road <- substr(data$road, 1, 3)
  data$home <- substr(data$home, 1, 3)

  ## Convert all columns to proper format
  
  data$score[is.na(data$score)] <- 'F'
  data$home_team_runs <- as.numeric(data$home_team_runs)
  data$away_team_runs <- as.numeric(data$away_team_runs)
  data$date <- as.Date(data$date, format = '%Y_%m_%d')
  data$inning <- as.numeric(data$inning)
  data$o <- as.numeric(data$o)
  
  ## Order dataset by day by game by batter
  
  data <- data[order(data$date, data$home, data$num),]
  
  ## Note - necessary to break out outcome any more granularly? This covers all at-bat level outcomes
  ## Included in daily fantasy scoring
  
  data$outcome <- ifelse(data$event == 'Single', 1,
                         ifelse(data$event == 'Double', 2,
                                ifelse(data$event == 'Triple', 3,
                                       ifelse(data$event == 'Home Run', 4,
                                              ifelse(data$event == 'Walk' | data$event == 'Intent Walk' | data$event == 'Hit By Pitch', 5,
                                                     ifelse(data$event == 'Strikeout' | data$event == 'Strikeout - DP', 6, 7))))))
  ## Create file for new data, then read in existing data & stack existing & new

  setwd(directory)
  write.csv(data, paste0(daterange,'_atbat.csv'), row.names = FALSE)
  return(data)
}

pitch <- pitchRxPitch('pitch.csv', dat, '~/Documents/Northwestern/498/MLB Scraper/pitchRx Data/new', '2016_05_04-2016_05_07')
newAtbat <- pitchRxAtBat(dat, '~/Documents/Northwestern/498/MLB Scraper/pitchRx Data/new', '2016_05_04-2016_05_07')

atbat <- read.csv('atbat.csv', colClasses = c('numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'character',
                                              'character', 'character', 'character', 'character', 'character', 
                                              'numeric', 'numeric', 'character', 'Date', 'character', 'character', 'numeric'))
atbat <- as.data.frame(rbind(atbat, newAtbat))
write.csv(atbat, 'atbat.csv', row.names = F)

## Converts data into the proper format

dataConvert <- function(df) {
  df[,'score'] <- as.logical(df[,'score'])
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                         as.factor)
  return(df)
}
atbat <- dataConvert(atbat)

## Run to match player names with their codes - not necessary for modeling, used for matching player names with RotoGrinders
## Will also have to create a manual match list between RotoGrinders names & these name

playerMatch <- function(newDF, var1, var2, directory) {
  g <- newDF[,c(var1, var2)]
  g$dups <- duplicated(g[,1])
  g <- filter(g, dups == F)
  g$dups <- NULL
  g[,2] <- ifelse(g[,1] == 666560, 'Byung-ho Park',
           ifelse(g[,1] == 624424, 'Michael Conforto',
           ifelse(g[,1] == 628329, 'Rusney Castillo',
           ifelse(g[,1] == 607074, 'Carlos Rodon',
           ifelse(g[,2] == 'Carlos Matias', 'Carlos Martinez',
           ifelse(g[,2] == 'C.C. Sabathia', 'CC Sabathia',
           ifelse(g[,2] == 'Christopher Archer', 'Chris Archer',
           ifelse(g[,2] == 'Dae Ho Lee', 'Dae-Ho Lee',
           ifelse(g[,2] == 'Daniel Santana', 'Danny Santana',
           ifelse(g[,2] == 'Daniel Valencia', 'Danny Valencia',
           ifelse(g[,2] == 'Senger Peralta', 'David Peralta',
           ifelse(g[,2] == 'Douglas Fister', 'Doug Fister',
           ifelse(g[,2] == 'Devaris Gordon', 'Dee Gordon',
           ifelse(g[,2] == 'Delino DeShieldsJr.', 'Delino DeShields',
           ifelse(g[,2] == 'Frederick Freeman', 'Freddie Freeman',
           ifelse(g[,2] == 'Howard Kendrick', 'Howie Kendrick',
           ifelse(g[,2] == 'Ivan De JesusJr.', 'Ivan De Jesus',
           ifelse(g[,2] == 'Jacob Realmuto', 'J.T. Realmuto',
           ifelse(g[,2] == 'Jacob DeGrom', 'Jacob deGrom',
           ifelse(g[,2] == 'Jacob Marisnick', 'Jake Marisnick',
           ifelse(g[,2] == 'JR Murphy', 'John Murphy',
           ifelse(g[,2] == 'Jonathan Moscot', 'Jon Moscot',
           ifelse(g[,2] == 'Jonathan Gray', 'Jon Gray',
           ifelse(g[,2] == 'Jonathan Jay', 'Jon Jay',
           ifelse(g[,2] == 'Jonathon Niese', 'Jon Niese',
           ifelse(g[,2] == 'Khristopher Davis', 'Khris Davis',
           ifelse(g[,2] == 'Manuel Machado', 'Manny Machado',
           ifelse(g[,2] == 'Matthew Adams', 'Matt Adams',
           ifelse(g[,2] == 'Matt Den Dekker', 'Matt den Dekker',
           ifelse(g[,2] == 'Matthew Duffy', 'Matt Duffy',
           ifelse(g[,2] == 'Matthew Joyce', 'Matt Joyce',
           ifelse(g[,2] == 'Matthew Wisler', 'Matt Wisler',
           ifelse(g[,2] == 'Mitchell Moreland', 'Mitch Moreland',
           ifelse(g[,2] == 'B.J. Upton', 'Melvin Upton Jr.',
           ifelse(g[,2] == 'Nathan Karns', 'Nate Karns',
           ifelse(g[,2] == 'Nicholas Tropeano', 'Nick Tropeano',
           ifelse(g[,2] == 'David Herrera', 'Odubel Herrera',
           ifelse(g[,2] == 'Raciel Iglesias', 'Raisel Iglesias',
           ifelse(g[,2] == 'Steven Souza', 'Steve Souza',
           ifelse(g[,2] == 'Timothy Beckham', 'Tim Beckham',
           ifelse(g[,2] == 'Wellington Castillo', 'Welington Castillo',
           ifelse(g[,2] == 'Zachary Davies', 'Zach Davies',
           ifelse(g[,2] == 'Zachary Cozart', 'Zack Cozart', as.character(g[,2]))))))))))))))))))))))))))))))))))))))))))))
  setwd(directory)
  write.csv(g, paste0(var2,'.csv'), row.names = F)
  return(g)
}

batterLookup <- playerMatch(atbat, 'batter', 'batter_name', '~/Documents/Northwestern/498/MLB Scraper/pitchRx Data/new')
pitcherLookup <- playerMatch(atbat, 'pitcher', 'pitcher_name', '~/Documents/Northwestern/498/MLB Scraper/pitchRx Data/new')

## Note pitchRxPitch pulls in historic as well as new info, so pitch df has everything
## pitchDataManip gets it in proper format for clustering

pitchDataManip <- function(df){
  breakingBall <- filter(df, bb == 1)
  fastball <- filter(df, bb != 1)
  fastball <- filter(fastball, !pitch_type %in% c('AB', 'SC'))

  ## Calculate mean & standard deviation of a bunch of variables - split by breaking balls & FB
  
  bb <- group_by(breakingBall, pitcher)
  bbSumm <- as.data.frame(
            summarise(bb, BmphMean = mean(start_speed),
                          BhMovMean = mean(abs(pfx_x)), BvMovMean = mean(pfx_z), 
                          BheightMean = mean(pz),
                          BmeanAngle = mean(abs(break_angle)), BmeanBreakLength = mean(break_length), 
                          BmeanSpinRate = mean(spin_rate)))
  
  fb <- group_by(fastball, pitcher)
  fbSumm <- as.data.frame(
            summarise(fb, FmphMean = mean(start_speed),
                      FhMovMean = mean(abs(pfx_x)), FvMovMean = mean(pfx_z), 
                      FheightMean = mean(pz),
                      FmeanAngle = mean(abs(break_angle)), FmeanBreakLength = mean(break_length), 
                      FmeanSpinRate = mean(spin_rate)))

  colnames(pitch)
  total <- group_by(df, pitcher)
  totalSumm <- as.data.frame(summarise(total, mphSD = sd(start_speed),
                         hMovSD = sd(pfx_x), vMovSD = sd(pfx_z),
                         locInitMean = mean(abs(x0)), heightInitMean = mean(z0),
                         locInitSD = sd(x0), heightInitSD = sd(z0),
                         spinDirSD = sd(spin_dir),
                         fbs = sum(ot), bbs = sum(bb)))
  totalSumm$bbPerc <- totalSumm$bbs / (totalSumm$bbs + totalSumm$fbs)
  totalSumm$fbPerc <- totalSumm$fbs / (totalSumm$fbs + totalSumm$bbs)
  
  SS <- dcast(df, pitcher ~ type, value.var = 'type', fun.aggregate = length)
  SS$total <- SS$B + SS$S + SS$SS + SS$X
  SS$SSPerc <- SS$SS / SS$total
  
  
  allSumm <- merge(fbSumm, bbSumm, by = 'pitcher')
  allSumm <- merge(allSumm, totalSumm, by = 'pitcher')
  allSumm <- merge(allSumm, SS[,c('pitcher','SSPerc')], by = 'pitcher')
  
  allSumm <- filter(allSumm, (fbs + bbs) > 100)
  
  return(allSumm)
}
pitchCluster <- pitchDataManip(pitch)

## Can optionally look at scatter plot of pitch variables if interested

scatPlot <- function(var1, var2) {
  ggplot(pitchCluster, aes_string(x = var1, y = var2)) +
    geom_point() +
    geom_smooth()
}
scatPlot('FmphMean', 'SSPerc')

## Runs clustering analysis of all pitchers - set up to have 4 clusters
## Note the number of 

pitchClustering <- function(df) {
  scaledDF <- scale(df[,c(2:23,26:length(df))])
  set.seed(500)
  km <- kmeans(scaledDF, 4, iter.max = 10)
  df$cluster <- km$cluster
  return(df)
}
finalPitch <- pitchClustering(pitchCluster)

## TO DO - fix clustering function to spit out the right output
## Update bw plot title and axes
## integrate clustering results into existing at bat data

bwPlot <- function(df, var2) {
  g <- ggplot(df, aes_string(x = factor('cluster'), y = var2)) +
  geom_boxplot(aes(colour = factor(cluster))) +
  xlab('Clusters') +
  ggtitle(paste0('How ', var2,' is distributed in each cluster'))
  g$labels$fill <- 'Clusters'
  g
}
bwPlot(finalPitch, colnames(finalPitch)[16])

## Creates modeling file - update with pitch type
## Not included in outcome variable: Batters - Stolen Base, Runs, RBIs
## Pitchers - Inning Pitched, Win, Earned Run Allowed, Complete Game, Complete Game Shutout

modelFile <- function(df, fp) {
  library(reshape2)
  
  ## Merge in pitcher type to dataset - based on clustering analysis
  df <- merge(df, fp[,c('pitcher', 'cluster')], by = 'pitcher', all.x = T)

  mixSpeed <- filter(df, cluster == 1)
  power <- filter(df, cluster == 2)
  sinker <- filter(df, cluster == 3)
  sidearm <- filter(df, cluster == 4)
  
  leftyMixSpeed <- filter(mixSpeed, mixSpeed$p_throws == 'L')
  leftyPower <- filter(power, power$p_throws == 'L')
  leftySinker <- filter(sinker, sinker$p_throws == 'L')
  leftySidearm <- filter(sidearm, sidearm$p_throws == 'L')
  leftyGen <- filter(df, p_throws == 'L')
  
  rightyMixSpeed <- filter(mixSpeed, mixSpeed$p_throws == 'R')
  rightyPower <- filter(power, power$p_throws == 'R')
  rightySinker <- filter(sinker, sinker$p_throws == 'R')
  rightySidearm <- filter(sidearm, sidearm$p_throws == 'R')
  rightyGen <- filter(df, p_throws == 'R')

  outcomeCalc <- function(df) {
    a <- dcast(df, batter~outcome, fun.aggregate = length, value.var = 'outcome')
    a$total <- a[,2] + a[,3] + a[,4] + a[,5] + a[,6] + a[,7] + a[,8]
    return(a)
  }
  
  leftyMixSpeed <- outcomeCalc(leftyMixSpeed)
  leftyPower <- outcomeCalc(leftyPower)
  leftySinker <- outcomeCalc(leftySinker)
  leftySidearm <- outcomeCalc(leftySidearm)
  leftyGen <- outcomeCalc(leftyGen)
  
  rightyMixSpeed <- outcomeCalc(rightyMixSpeed)
  rightyPower <- outcomeCalc(rightyPower)
  rightySinker <- outcomeCalc(rightySinker)
  rightySidearm <- outcomeCalc(rightySidearm)
  rightyGen <- outcomeCalc(rightyGen)
  
  perAB <- function(x) {
    for( i in 2:(length(x)-1)) {
      x[[i]] <- x[[i]] / x[['total']]
    }
    return(x)
  }

  leftyMixSpeed <- perAB(leftyMixSpeed)
  leftyPower <- perAB(leftyPower)
  leftySinker <- perAB(leftySinker)
  leftySidearm <- perAB(leftySidearm)
  leftyGen <- perAB(leftyGen)
  
  rightyMixSpeed <- perAB(rightyMixSpeed)
  rightyPower <- perAB(rightyPower)
  rightySinker <- perAB(rightySinker)
  rightySidearm <- perAB(rightySidearm)
  rightyGen <- perAB(rightyGen)
  
  ## Rename columns for future merging
  
  reNamer <- function(df, name) {
    colnames(df)[2:9] <- paste0(name,"_",colnames(df[2:9]))
    return <- df
  }
  
  leftyMixSpeed <- reNamer(leftyMixSpeed, 'leftyMixSpeed')
  leftyPower <- reNamer(leftyPower, 'leftyPower')
  leftySinker <- reNamer(leftySinker, 'leftySinker')
  leftySidearm <- reNamer(leftySidearm, 'leftySidearm')
  leftyGen <- reNamer(leftyGen, 'leftyGen')
  rightyMixSpeed <- reNamer(rightyMixSpeed, 'rightMixSpeed')
  rightyPower <- reNamer(rightyPower, 'rightyPower')
  rightySinker <- reNamer(rightySinker, 'rightySinker')
  rightySidearm <- reNamer(rightySidearm, 'rightySidearm')
  rightyGen <- reNamer(rightyGen, 'rightyGen')
  
  ## Pitcher stats - same as for batter, find probability of each outcome by righty/lefty
  
  pitchRightyOutcome <- filter(df, stand == 'R')
  pitchLeftyOutcome <- filter(df, stand == 'L')
  
  pitchRightyOutcome <- dcast(pitchRightyOutcome, pitcher ~ outcome, fun.aggregate = length, value.var = 'outcome')
  pitchRightyOutcome$total <- pitchRightyOutcome$`1` + pitchRightyOutcome$`2` + pitchRightyOutcome$`3` + pitchRightyOutcome$`4` + pitchRightyOutcome$`5` + pitchRightyOutcome$`6`+ pitchRightyOutcome$`7`
  pitchLeftyOutcome <- dcast(pitchLeftyOutcome, pitcher ~ outcome, fun.aggregate = length, value.var = 'outcome')
  pitchLeftyOutcome$total <- pitchLeftyOutcome$`1` + pitchLeftyOutcome$`2` + pitchLeftyOutcome$`3` + pitchLeftyOutcome$`4` + pitchLeftyOutcome$`5` + pitchLeftyOutcome$`6`+ pitchLeftyOutcome$`7`
  
  pitchRightyOutcome <- perAB(pitchRightyOutcome)
  pitchLeftyOutcome <- perAB(pitchLeftyOutcome)
  
  ## Create one dataset used for modeling that has batter & pitcher stats for each at bat, as well as outcome
  
  ## Merge with all lefty/righty datasets

  modelDF <- df[,c('batter', 'pitcher', 'stand', 'p_throws', 'home', 'outcome', 'cluster')]
  modelDF <- merge(modelDF, rightyMixSpeed, by = c('batter'), all.x = T)
  modelDF <- merge(modelDF, rightyPower, by = c('batter'), all.x = T)
  modelDF <- merge(modelDF, rightySinker, by = c('batter'), all.x = T)
  modelDF <- merge(modelDF, rightySidearm, by = c('batter'), all.x = T)
  modelDF <- merge(modelDF, rightyGen, by = c('batter'), all.x = T)
  modelDF <- merge(modelDF, leftyMixSpeed, by = c('batter'), all.x = T)
  modelDF <- merge(modelDF, leftyPower, by = c('batter'), all.x = T)
  modelDF <- merge(modelDF, leftySinker, by = c('batter'), all.x = T)
  modelDF <- merge(modelDF, leftySidearm, by = c('batter'), all.x = T)
  modelDF <- merge(modelDF, leftyGen, by = c('batter'), all.x = T)
  modelDF$cluster <- as.numeric(modelDF$cluster)
  modelDF$cluster[is.na(modelDF$cluster)] <- 5

  modelDF$batOne <- ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 1, modelDF$leftyMixSpeed_1,
                    ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 2, modelDF$leftyPower_1,
                    ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 3, modelDF$leftySinker_1,
                    ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 4, modelDF$leftySidearm_1,
                    ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 5,  modelDF$leftyGen_1,
                    ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 1, modelDF$rightMixSpeed_1,
                    ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 2, modelDF$rightyPower_1,
                    ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 3, modelDF$rightySinker_1,
                    ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 4, modelDF$rightySidearm_1,
                    modelDF$rightyGen_1)))))))))
  modelDF$batTwo <- ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 1, modelDF$leftyMixSpeed_2,
                           ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 2, modelDF$leftyPower_2,
                                  ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 3, modelDF$leftySinker_2,
                                         ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 4, modelDF$leftySidearm_2,
                                                ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 5,  modelDF$leftyGen_2,
                                                       ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 1, modelDF$rightMixSpeed_2,
                                                              ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 2, modelDF$rightyPower_2,
                                                                     ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 3, modelDF$rightySinker_2,
                                                                            ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 4, modelDF$rightySidearm_2,
                                                                                   modelDF$rightyGen_2)))))))))
  modelDF$batThree <- ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 1, modelDF$leftyMixSpeed_3,
                           ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 2, modelDF$leftyPower_3,
                                  ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 3, modelDF$leftySinker_3,
                                         ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 4, modelDF$leftySidearm_3,
                                                ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 5,  modelDF$leftyGen_3,
                                                       ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 1, modelDF$rightMixSpeed_3,
                                                              ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 2, modelDF$rightyPower_3,
                                                                     ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 3, modelDF$rightySinker_3,
                                                                            ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 4, modelDF$rightySidearm_3,
                                                                                   modelDF$rightyGen_3)))))))))
  modelDF$batFour <- ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 1, modelDF$leftyMixSpeed_4,
                             ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 2, modelDF$leftyPower_4,
                                    ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 3, modelDF$leftySinker_4,
                                           ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 4, modelDF$leftySidearm_4,
                                                  ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 5,  modelDF$leftyGen_4,
                                                         ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 1, modelDF$rightMixSpeed_4,
                                                                ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 2, modelDF$rightyPower_4,
                                                                       ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 3, modelDF$rightySinker_4,
                                                                              ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 4, modelDF$rightySidearm_4,
                                                                                     modelDF$rightyGen_4)))))))))
  modelDF$batFive <- ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 1, modelDF$leftyMixSpeed_5,
                            ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 2, modelDF$leftyPower_5,
                                   ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 3, modelDF$leftySinker_5,
                                          ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 4, modelDF$leftySidearm_5,
                                                 ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 5,  modelDF$leftyGen_5,
                                                        ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 1, modelDF$rightMixSpeed_5,
                                                               ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 2, modelDF$rightyPower_5,
                                                                      ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 3, modelDF$rightySinker_5,
                                                                             ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 4, modelDF$rightySidearm_5,
                                                                                    modelDF$rightyGen_5)))))))))
  modelDF$batSix <- ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 1, modelDF$leftyMixSpeed_6,
                            ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 2, modelDF$leftyPower_6,
                                   ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 3, modelDF$leftySinker_6,
                                          ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 4, modelDF$leftySidearm_6,
                                                 ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 5,  modelDF$leftyGen_6,
                                                        ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 1, modelDF$rightMixSpeed_6,
                                                               ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 2, modelDF$rightyPower_6,
                                                                      ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 3, modelDF$rightySinker_6,
                                                                             ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 4, modelDF$rightySidearm_6,
                                                                                    modelDF$rightyGen_6)))))))))
  modelDF$batSeven <- ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 1, modelDF$leftyMixSpeed_7,
                            ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 2, modelDF$leftyPower_7,
                                   ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 3, modelDF$leftySinker_7,
                                          ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 4, modelDF$leftySidearm_7,
                                                 ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 5,  modelDF$leftyGen_7,
                                                        ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 1, modelDF$rightMixSpeed_7,
                                                               ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 2, modelDF$rightyPower_7,
                                                                      ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 3, modelDF$rightySinker_7,
                                                                             ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 4, modelDF$rightySidearm_7,
                                                                                    modelDF$rightyGen_7)))))))))
  modelDF$batTotal <- ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 1, modelDF$leftyMixSpeed_total,
                            ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 2, modelDF$leftyPower_total,
                                   ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 3, modelDF$leftySinker_total,
                                          ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 4, modelDF$leftySidearm_total,
                                                 ifelse(modelDF$p_throws == 'L' & modelDF$cluster == 5,  modelDF$leftyGen_total,
                                                        ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 1, modelDF$rightMixSpeed_total,
                                                               ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 2, modelDF$rightyPower_total,
                                                                      ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 3, modelDF$rightySinker_total,
                                                                             ifelse(modelDF$p_throws == 'R' & modelDF$cluster == 4, modelDF$rightySidearm_total,
                                                                                    modelDF$rightyGen_total))))))))) 
  modelDF$batOne <- ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'L', modelDF$leftyGen_1,
                           ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'R', modelDF$rightyGen_1, modelDF$batOne))
  modelDF$batTwo <- ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'L', modelDF$leftyGen_2,
                           ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'R', modelDF$rightyGen_2, modelDF$batTwo))
  modelDF$batThree <- ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'L', modelDF$leftyGen_3,
                           ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'R', modelDF$rightyGen_3, modelDF$batThree))
  modelDF$batFour <- ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'L', modelDF$leftyGen_4,
                           ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'R', modelDF$rightyGen_4, modelDF$batFour))
  modelDF$batFive <- ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'L', modelDF$leftyGen_5,
                           ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'R', modelDF$rightyGen_5, modelDF$batFive))
  modelDF$batSix <- ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'L', modelDF$leftyGen_6,
                           ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'R', modelDF$rightyGen_6, modelDF$batSix))
  modelDF$batSeven <- ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'L', modelDF$leftyGen_7,
                           ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'R', modelDF$rightyGen_7, modelDF$batSeven))
  modelDF$batTotal <- ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'L', modelDF$leftyGen_total,
                             ifelse(modelDF$batTotal < 10 & modelDF$p_throws == 'R', modelDF$rightyGen_total, modelDF$batTotal))
  
  modelDF <- modelDF[,c('batter', 'pitcher', 'stand', 'p_throws', 'home', 'cluster',
                        'batOne', 'batTwo', 'batThree', 'batFour' ,'batFive', 'batSix', 'batSeven', 'batTotal',
                        'outcome')]

  modelDF <- merge(modelDF, pitchLeftyOutcome, by = c('pitcher'), all.x = T)
  modelDF <- merge(modelDF, pitchRightyOutcome, by = c('pitcher'), all.x = T)
  modelDF$pitchOne <- ifelse(modelDF$stand == 'L', modelDF$`1.x`, modelDF$`1.y`)
  modelDF$pitchTwo <- ifelse(modelDF$stand == 'L', modelDF$`2.x`, modelDF$`2.y`)
  modelDF$pitchThree <- ifelse(modelDF$stand == 'L', modelDF$`3.x`, modelDF$`3.y`)
  modelDF$pitchFour <- ifelse(modelDF$stand == 'L', modelDF$`4.x`, modelDF$`4.y`)
  modelDF$pitchFive <- ifelse(modelDF$stand == 'L', modelDF$`5.x`, modelDF$`5.y`)
  modelDF$pitchSix <- ifelse(modelDF$stand == 'L', modelDF$`6.x`, modelDF$`6.y`)
  modelDF$pitchSeven <- ifelse(modelDF$stand == 'L', modelDF$`7.x`, modelDF$`7.y`)
  modelDF$pitchTotal <- ifelse(modelDF$stand == 'L', modelDF$total.x, modelDF$total.y)
  head(modelDF)
  modelDF <- modelDF[,c('batter', 'pitcher', 'stand', 'p_throws', 'home', 'cluster',
                        'batOne', 'batTwo', 'batThree', 'batFour' ,'batFive', 'batSix', 'batSeven', 'batTotal',
                        'pitchOne', 'pitchTwo', 'pitchThree', 'pitchFour' ,'pitchFive', 'pitchSix', 'pitchSeven', 'pitchTotal',
                        'outcome')]
  rounder <- function(x) {
    round(x, 4)
  }
  
  modelDF[,c(7:21)] <- as.data.frame(sapply(modelDF[,c(7:21)], rounder))
  write.csv(modelDF, 'modelFile.csv', row.names = F)
  return(modelDF)
}
fullData <- modelFile(atbat, finalPitch)

###### MODELING !! ######

## use fullData dataset ##
head(fullData)
