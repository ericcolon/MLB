library(pitchRx)
library(RSQLite)
library(dplyr)
library(data.table)
library(splitstackshape)
library(reshape2)
library(DescTools)

################################ MLB Data Pull - Using Pitch fX dataset #################################

## To start - make sure you have all required files in your working directory
## By "full", I mean all data prior to the date range you put in for pitchRxScraper
## All other data can be generated within this script, but just need to have that baseline

## Insert desired start & end date, and directory you want to put the file in - see example "new"

## Function to scrape data from pitch Fx database - saved as df name 'dat'
## Currently updated through 5/20/2016 - contains all of 2015 season + all data through 5/20/2016 for 2016 season
## Run below code to get up to date through 5/31

setwd('~/Desktop/Data Upload')
date <- '2016-06-13'

pitchRxScraper <- function(startDate, endDate) {
  dat <- scrape(start = startDate, end = endDate)
  return(dat)
}
dat <- pitchRxScraper('2016-06-10', '2016-06-12')

## Create pitcher & at-bat files - these will be saved to your working directory

## Use pitches per AB turn into projected output

pitchRxPitch <- function(existing, newFile, directory, daterange){
  pitch <- newFile[['pitch']]
  test <- newFile[['atbat']]
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
  
  ## Make sure there is a file in there already
  
  if(file.exists(existing)) {
    o <- read.csv(existing, stringsAsFactors = F)
    pitch <- as.data.frame(rbind(o, pitch))
  }
  ## Stack old & new data to create new full dataset
  
  write.csv(pitch, 'pitch.csv', row.names = F)
  return(pitch)
}

pitchRxAtBat <- function(existing, newFile, directory, daterange) {
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
  
  ## Read in old data if it exists
  if(file.exists(existing)) {
    oldAtbat <- read.csv(existing, colClasses = c('numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'character',
                                                  'character', 'character', 'character', 'character', 'character', 
                                                  'numeric', 'numeric', 'character', 'Date', 'character', 'character', 'numeric'))
    data <- as.data.frame(rbind(data, oldAtbat))
  }
  write.csv(data, 'atbat.csv', row.names = F)
  return(data)
}

pitch <- pitchRxPitch('pitch.csv', dat, '~/Desktop/Data Upload', '2016_06_10-2016_06_12')
atbat <- pitchRxAtBat('atbat.csv', dat, '~/Desktop/Data Upload', '2016_06_10-2016_06_12')

steals <- function(data) {
  dataset <- data[['atbat']]
  dataRunner <- data[['runner']]
  
  dataset <- dataset[,c('pitcher', 'batter', 'event', 'num', 'o', 'inning_side', 'inning', 'score','gameday_link')]
  dataset$outcome <- ifelse(dataset$event == 'Single', 1,
                            ifelse(dataset$event == 'Double', 2,
                                   ifelse(dataset$event == 'Triple', 3,
                                          ifelse(dataset$event == 'Home Run', 4,
                                                 ifelse(dataset$event == 'Walk' | dataset$event == 'Intent Walk' | dataset$event == 'Hit By Pitch', 5,
                                                        ifelse(dataset$event == 'Strikeout' | dataset$event == 'Strikeout - DP', 6, 7))))))
  
  dataset$grouper <- paste0(dataset$batter, "_", dataset$outcome)
  dataset$grouperP <- paste0(dataset$pitcher, "_", dataset$outcome)
  
  dataset$counter <- 1
  
  ## Read in historic data & stack with new data
  
  if(file.exists('batSteals.csv')){
    oldBatSteals <- read.csv('batSteals.csv', stringsAsFactors = F)
    dataset <- as.data.frame(rbind(dataset, oldBatSteals))  
  }
  
  ## Write full file back to CSV
  
  write.csv(dataset, 'batSteals.csv', row.names = F)
  
  dataRunner <- dataRunner[,c('id', 'event', 'event_num', 'inning_side', 'inning', 'num', 'score', 'rbi', 'earned', 'gameday_link')]
  
  dataRunner$cs <- ifelse(dataRunner$event %in% c('Caught Stealing 2B',
                                                  'Caught Stealing 3B',
                                                  'Caught Stealing home',
                                                  'Picked off stealing 2B',
                                                  'Picked off stealing 3B',
                                                  'Picked off stealing home',
                                                  'Pickoff 2B',
                                                  'Pickoff 3B',
                                                  'Pickoff 1B'), 1, 0)
  dataRunner$sb <- ifelse(dataRunner$event %in% c('Stolen Base 2B',
                                                  'Stolen Base 3B',
                                                  'Stolen Base home'), 1, 0)
  
  sbs <- filter(dataRunner, sb == 1 | cs == 1)
  sbs <- merge(sbs[,c('gameday_link','inning','inning_side','id','num','event','cs','sb')], dataset[,c('pitcher','batter','event','inning','inning_side','num','gameday_link', 'grouper', 'grouperP')], by.x = c('gameday_link','inning', 'inning_side','id'), by.y = c('gameday_link', 'inning', 'inning_side', 'batter'))
  sbs <- filter(sbs, num.x >= num.y)
  sbs$uniques <- paste0(sbs$gameday_link,sbs$inning,sbs$inning_side,sbs$id,sbs$num,sbs$event.x)
  sbs$dups <- duplicated(sbs$uniques)
  sbs <- filter(sbs, dups == F)
  sbs$dups <- NULL
  sbs$uniques <- NULL
  
  ## Write new data to individual CSV file
  
  if(file.exists('steals.csv')) {
    oldStealsData <- read.csv('steals.csv', stringsAsFactors = F)
    sbs <- as.data.frame(rbind(sbs, oldStealsData))
  }
  
  ## Write full data to full CSV file
  
  write.csv(sbs, 'steals.csv', row.names = F)
  
  ## Create outcome type variable
  
  sbGroup <- group_by(sbs, grouper)
  sbGroup <- as.data.frame(summarise(sbGroup,
                                     sb = sum(sb),
                                     cs = sum(cs)))
  
  ## Calc percentage of each outcome on total dataset - divide that number SB / CS by those numbers
  
  dsGroup <- group_by(dataset, grouper)
  dsGroup <- as.data.frame(summarise(dsGroup,
                                     counter = sum(counter)))
  
  dsGroup <- merge(dsGroup, sbGroup, by = 'grouper', all.x = T)
  dsGroup[is.na(dsGroup)] <- 0
  dsGroup$sbPerc <- dsGroup$sb / dsGroup$counter
  dsGroup$csPerc <- dsGroup$cs / dsGroup$counter
  dsGroup <- as.data.frame(cSplit(dsGroup, 'grouper', sep = '_'))
  
  ## Create totals
  
  sbGroupTB <- group_by(sbs, id)
  sbGroupTB <- as.data.frame(summarise(sbGroupTB,
                                       sb = sum(sb),
                                       cs = sum(cs)))
  
  ## Create probabilities of steals & caught stealing based on batter outcome
  
  sbPerc <- dcast(dsGroup, grouper_1 ~ grouper_2, value.var = 'sbPerc')
  sbPerc[is.na(sbPerc)] <- 0
  colnames(sbPerc) <- c('batter', 'singleSB', 'doubleSB', 'tripleSB', 'hrSB', 'walkSB', 'strikeoutSB', 'otherSB')
  
  csPerc <- dcast(dsGroup, grouper_1 ~ grouper_2, value.var = 'csPerc')
  csPerc[is.na(csPerc)] <- 0
  colnames(csPerc) <- c('batter', 'singleCS', 'doubleCS', 'tripleCS', 'hrCS', 'walkCS', 'strikeoutCS', 'otherCS')
  
  sbBat <- merge(sbPerc, csPerc, by = 'batter')
  
  ## Merge in total numbrs
  
  sbBat <- merge(sbBat, sbGroupTB, by.x = 'batter', by.y = 'id', all.x = T)
  sbBat[is.na(sbBat)] <- 0
  
  ## Repeat exercise for pitcher
  ## Create outcome type variable
  
  sbGroup <- group_by(sbs, grouperP)
  sbGroup <- as.data.frame(summarise(sbGroup,
                                     sb = sum(sb),
                                     cs = sum(cs)))
  
  dsGroup <- group_by(dataset, grouperP)
  dsGroup <- as.data.frame(summarise(dsGroup,
                                     counter = sum(counter)))
  
  dsGroup <- merge(dsGroup, sbGroup, by = 'grouperP', all.x = T)
  dsGroup[is.na(dsGroup)] <- 0
  dsGroup$sbPerc <- dsGroup$sb / dsGroup$counter
  dsGroup$csPerc <- dsGroup$cs / dsGroup$counter
  dsGroup <- as.data.frame(cSplit(dsGroup, 'grouperP', sep = '_'))
  
  ## Calculate totals (for outlier detection)
  
  sbGroupTP <- group_by(sbs, pitcher)
  sbGroupTP <- as.data.frame(summarise(sbGroupTP,
                                       sbP = sum(sb),
                                       csP = sum(cs)))
  
  ## Create probabilities of steals & caught stealing based on batter outcome
  
  sbPercP <- dcast(dsGroup, grouperP_1 ~ grouperP_2, value.var = 'sbPerc')
  sbPercP[is.na(sbPercP)] <- 0
  colnames(sbPercP) <- c('pitcher', 'singleSBP', 'doubleSBP', 'tripleSBP', 'hrSBP', 'walkSBP', 'strikeoutSBP', 'otherSBP')
  
  csPercP <- dcast(dsGroup, grouperP_1 ~ grouperP_2, value.var = 'csPerc')
  csPercP[is.na(csPercP)] <- 0
  colnames(csPercP) <- c('pitcher', 'singleCSP', 'doubleCSP', 'tripleCSP', 'hrCSP', 'walkCSP', 'strikeoutCSP', 'otherCSP')
  
  ## Merge in total numbers
  
  sbPitch <- merge(sbPercP, csPercP, by = 'pitcher')
  sbPitch <- merge(sbPitch, sbGroupTP, by = 'pitcher', all.x = T)
  sbPitch[is.na(sbPitch)] <- 0
  
  theList <- list()
  theList[[1]] <- sbBat
  theList[[2]] <- sbPitch
  
  return(theList)
  
}

batterSteals <- steals(dat)

batSteals <- batterSteals[[1]]
pitchSteals <- batterSteals[[2]]

## Calc RBIs 

rbis <- function(data) {
  rbiBat <- data[['atbat']]
  rbiBat <- rbiBat[,c('pitcher','batter','event','num','gameday_link')]
  rbiBat$outcome <- ifelse(rbiBat$event == 'Single', 1,
                           ifelse(rbiBat$event == 'Double', 2,
                                  ifelse(rbiBat$event == 'Triple', 3,
                                         ifelse(rbiBat$event == 'Home Run', 4,
                                                ifelse(rbiBat$event == 'Walk' | rbiBat$event == 'Intent Walk' | rbiBat$event == 'Hit By Pitch', 5,
                                                       ifelse(rbiBat$event == 'Strikeout' | rbiBat$event == 'Strikeout - DP', 6, 7))))))
  
  rbiBat$grouper <- paste0(rbiBat$batter, "_", rbiBat$outcome)
  rbiBat$grouperP <- paste0(rbiBat$pitcher, "_", rbiBat$outcome)
  rbiBat$counter <- 1
  
  ## CREATE STACKER W/ OLD DATA
  
  if(file.exists('rbiDataFull.csv')) {
    oldRBIBat <- read.csv('rbiDataFull.csv', stringsAsFactors = F)
    rbiBat <- as.data.frame(rbind(rbiBat, oldRBIBat))
  }
  
  write.csv(rbiBat, 'rbiDataFull.csv', row.names = F)
  
  ## Create rbi data using runner dataset
  
  rbidf <- data[['runner']]
  rbidf <- rbidf[,c('num', 'rbi', 'gameday_link')]
  rbidf[is.na(rbidf)] <- 0
  rbidf$rbi <- ifelse(rbidf$rbi == 'T', 1, 0)
  rbidf <- filter(rbidf, rbi > 0)
  rbidf$play <- paste0(rbidf$gameday_link,"-",rbidf$num)
  rbiTotal <- group_by(rbidf, play)
  rbiTotal <- as.data.frame(summarise(rbiTotal,
                                      rbis = sum(rbi)))
  
  rbiTotal <- as.data.frame(cSplit(rbiTotal, 'play', sep = '-'))
  rbiTotal <- merge(rbiTotal, rbiBat, by.x = c('play_1', 'play_2'), by.y = c('gameday_link','num'), all.x = T)
  
  ## CREATE STACKER W/ OLD DATA
  
  if(file.exists('rbiData.csv')){
    oldRBIData <- read.csv('rbiData.csv', stringsAsFactors = F)
    rbiTotal <- as.data.frame(rbind(rbiTotal, oldRBIData))
  }
  
  write.csv(rbiTotal, 'rbiData.csv', row.names = F)
  
  ## Calc average RBIs per outcome type - batter
  
  batterRBI <- group_by(rbiTotal, grouper)
  batterRBI <- as.data.frame(summarise(batterRBI,
                                       rbis = sum(rbis)))
  
  batterOutcome <- group_by(rbiBat, grouper)
  batterOutcome <- as.data.frame(summarise(batterOutcome,
                                           total = sum(counter)))
  
  batterOutcome <- merge(batterOutcome, batterRBI, by = 'grouper', all.x = T)
  batterOutcome[is.na(batterOutcome)] <- 0
  batterOutcome$rbiPerc <- batterOutcome$rbis / batterOutcome$total
  
  ## Split grouper into two columns for batter id & outcome type
  
  batterOutcome <- as.data.frame(cSplit(batterOutcome, 'grouper', sep = '_'))
  
  ## Cast outcome by batter
  
  batterOutcomeF <- dcast(batterOutcome, grouper_1 ~ grouper_2, value.var = 'rbiPerc')
  batterOutcomeF[is.na(batterOutcomeF)] <- 0
  
  ## Merge back with total at bats per player
  
  batOutcomeAB <- group_by(batterOutcome, grouper_1)
  batOutcomeAB <- as.data.frame(summarise(batOutcomeAB,
                                          total = sum(total)))
  
  batterOutcomeF <- merge(batterOutcomeF, batOutcomeAB, by = 'grouper_1')
  
  ## Calc average RBIs per outcome type - pitcher
  
  pitcherRBI <- group_by(rbiTotal, grouperP)
  pitcherRBI <- as.data.frame(summarise(pitcherRBI,
                                        rbis = sum(rbis)))
  
  pitcherOutcome <- group_by(rbiBat, grouperP)
  pitcherOutcome <- as.data.frame(summarise(pitcherOutcome,
                                            total = sum(counter)))
  
  pitcherOutcome <- merge(pitcherOutcome, pitcherRBI, by = 'grouperP', all.x = T)
  pitcherOutcome[is.na(pitcherOutcome)] <- 0
  pitcherOutcome$rbiPerc <- pitcherOutcome$rbis / pitcherOutcome$total
  
  ## Split grouper into two columns for batter id & outcome type
  
  pitcherOutcome <- as.data.frame(cSplit(pitcherOutcome, 'grouperP', sep = '_'))
  
  ## Cast outcome by pitcher
  
  pitcherOutcomeF <- dcast(pitcherOutcome, grouperP_1 ~ grouperP_2, value.var = 'rbiPerc')
  pitcherOutcomeF[is.na(pitcherOutcomeF)] <- 0
  
  ## Merge back with total at bats per player
  
  batOutcomeAB <- group_by(pitcherOutcome, grouperP_1)
  batOutcomeAB <- as.data.frame(summarise(batOutcomeAB,
                                          total = sum(total)))
  
  pitcherOutcomeF <- merge(pitcherOutcomeF, batOutcomeAB, by = 'grouperP_1')
  
  
  theList <- list()
  theList[[1]] <- batterOutcomeF
  theList[[2]] <- pitcherOutcomeF
  return(theList)
}

batterRBIs <- rbis(dat)

batRBIs <- batterRBIs[[1]]
pitchRBIs <- batterRBIs[[2]]

colnames(batRBIs) <- c('batter','rbiB_1', 'rbiB_2', 'rbiB_3', 'rbiB_4', 'rbiB_5', 'rbiB_6', 'rbiB_7', 'batTotal')
colnames(pitchRBIs) <- c('pitcher','rbiP_1', 'rbiP_2', 'rbiP_3', 'rbiP_4', 'rbiP_5', 'rbiP_6', 'rbiP_7', 'pitchTotal')

## Calc Runs

runs <- function(data) {
  d <- dat[['atbat']]
  e <- dat[['runner']]
  b <- d[,c('pitcher','batter','num','inning','event','gameday_link')]
  cc <- e[,c('id','num','inning','gameday_link','score','earned')]
  b$outcome <- b$outcome <- ifelse(b$event == 'Single', 1,
                                   ifelse(b$event == 'Double', 2,
                                          ifelse(b$event == 'Triple', 3,
                                                 ifelse(b$event == 'Home Run', 4,
                                                        ifelse(b$event == 'Walk' | b$event == 'Intent Walk' | b$event == 'Hit By Pitch', 5,
                                                               ifelse(b$event == 'Strikeout' | b$event == 'Strikeout - DP', 6, 7))))))
  
  ## Calculate total outcome counts by batter & pitcher
  
  runBat <- b[,c('pitcher','batter','outcome')]
  
  ## Save file & Read in old data
  
  if(file.exists('runBat.csv')){
    oldRunBat <- read.csv('runBat.csv', stringsAsFactors = F)
    runBat <- as.data.frame(rbind(runBat, oldRunBat))
  }
  
  write.csv(runBat, 'runBat.csv', row.names = F)
  
  ## Create totals by outcome type - pitcher
  
  runPitch <- dcast(runBat, pitcher ~ outcome, value.var = 'outcome', fun.aggregate = length)
  runPitch[is.na(runPitch)] <- 0
  runPitch$total <- runPitch$`1` + runPitch$`2` + runPitch$`3` + runPitch$`4` + runPitch$`5` + runPitch$`6` + runPitch$`7`
  colnames(runPitch) <- c('pitcher','totalOne','totalTwo','totalThr','totalFou','totalFiv','totalSix','totalSev','total') 
  
  ## Group by - to account for stacking of data
  
  runPitch <- group_by(runPitch, pitcher)
  runPitch <- as.data.frame(summarise(runPitch,
                                      runOne = sum(totalOne),
                                      runTwo = sum(totalTwo),
                                      runThree = sum(totalThr),
                                      runFour = sum(totalFou),
                                      runFive = sum(totalFiv),
                                      runSeven = sum(totalSev)))
  
  ## Create totals by outcome type - batter
  
  runBat <- dcast(runBat, batter ~ outcome, value.var = 'outcome', fun.aggregate = length)
  runBat[is.na(runBat)] <- 0
  runBat$total <- runBat$`1` + runBat$`2` + runBat$`3` + runBat$`4` + runBat$`5` + runBat$`6` + runBat$`7`
  colnames(runBat) <- c('batter','totalOne','totalTwo','totalThr','totalFou','totalFiv','totalSix','totalSev','total')
  
  runBat <- group_by(runBat, batter)
  runBat <- as.data.frame(summarise(runBat,
                                    runOne = sum(totalOne),
                                    runTwo = sum(totalTwo),
                                    runThree = sum(totalThr),
                                    runFour = sum(totalFou),
                                    runFive = sum(totalFiv),
                                    runSeven = sum(totalSev)))
  
  
  ## Calc runs scored by outcome type
  
  cc <- filter(cc, score == 'T')
  bb <- b[,c('pitcher','batter','num','inning','event','gameday_link','outcome')]
  cc <- cc[,c('id','num','inning','gameday_link','score','earned')]
  
  mer <- merge(bb,cc, by.x = c('batter','gameday_link','inning') , by.y = c('id','gameday_link','inning'))
  mer <- filter(mer, num.x <= num.y)
  mer <- mer[order(mer$gameday_link,mer$inning),]
  mer$earned <- ifelse(mer$earned == 'T', 1, 0)
  mer$earned[is.na(mer$earned)] <- 0
  mer$score <- ifelse(mer$score == 'T', 1, 0)
  
  runsDF <- mer[,c('batter','pitcher','score','earned','outcome')]
  
  ## Read old data & save to CSV
  
  if(file.exists('runsTotal.csv')){
    oldRuns <- read.csv('runsTotal.csv', stringsAsFactors = F)
    runsDF <- as.data.frame(rbind(runsDF, oldRuns))
  }
  
  write.csv(runsDF, 'runsTotal.csv', row.names = F)
  
  ## Create totals by outcome type - pitcher
  
  runsDFP <- dcast(runsDF, pitcher ~ outcome, value.var = 'outcome', fun.aggregate = length)
  runsDFP[is.na(runsDFP)] <- 0
  runsDFP$total <- runsDFP$`1` + runsDFP$`2` + runsDFP$`3` + runsDFP$`4` + runsDFP$`5` + runsDFP$`7`
  
  ## Group by - to account for stacking of data
  
  runsDFP <- group_by(runsDFP, pitcher)
  runsDFP <- as.data.frame(summarise(runsDFP,
                                     runOneB = sum(`1`),
                                     runTwoB = sum(`2`),
                                     runThreeB = sum(`3`),
                                     runFourB = sum(`4`),
                                     runFiveB = sum(`5`),
                                     runSevenB = sum(`7`)))
  
  ## Create totals by outcome type - batter
  
  runsDFB <- dcast(runsDF, batter ~ outcome, value.var = 'outcome', fun.aggregate = length)
  runsDFB[is.na(runsDFB)] <- 0
  runsDFB$total <- runsDFB$`1` + runsDFB$`2` + runsDFB$`3` + runsDFB$`4` + runsDFB$`5`+ runsDFB$`7`
  
  runsDFB <- group_by(runsDFB, batter)
  runsDFB <- as.data.frame(summarise(runsDFB,
                                     runOneB = sum(`1`),
                                     runTwoB = sum(`2`),
                                     runThreeB = sum(`3`),
                                     runFourB = sum(`4`),
                                     runFiveB = sum(`5`),
                                     runSevenB = sum(`7`)))
  
  ## Create percentages for batters
  
  runsBatter <- merge(runBat, runsDFB, by = 'batter', all.x = T)
  runsBatter[is.na(runsBatter)] <- 0
  runsBatter$runsOnePerc <- ifelse(runsBatter$runOne < 15, 0.20, runsBatter$runOneB / runsBatter$runOne)
  runsBatter$runsTwoPerc <- ifelse(runsBatter$runTwo < 15, 0.40, runsBatter$runTwoB / runsBatter$runTwo)
  runsBatter$runsThreePerc <- 0.75
  runsBatter$runsFourPerc <- 1
  runsBatter$runsFivePerc <- ifelse(runsBatter$runFive < 15, 0.20, runsBatter$runFiveB / runsBatter$runFive)
  runsBatter$runsSevenPerc <- ifelse(runsBatter$runSeven < 15, 0.015, runsBatter$runSevenB / runsBatter$runSeven)
  runsBatter <- runsBatter[,c('batter','runsOnePerc','runsTwoPerc','runsThreePerc','runsFourPerc','runsFivePerc','runsSevenPerc')]
  
  ## Create percentages for pitchers
  
  runsPitcher <- merge(runPitch, runsDFP, by = 'pitcher', all.x = T)
  runsPitcher[is.na(runsPitcher)] <- 0
  runsPitcher$runsOnePercP <- ifelse(runsPitcher$runOne < 15, 0.20, runsPitcher$runOneB / runsPitcher$runOne)
  runsPitcher$runsTwoPercP <- ifelse(runsPitcher$runTwo < 15, 0.40, runsPitcher$runTwoB / runsPitcher$runTwo)
  runsPitcher$runsThreePercP <- 0.75
  runsPitcher$runsFourPercP <- 1
  runsPitcher$runsFivePercP <- ifelse(runsPitcher$runFive < 15, 0.20, runsPitcher$runFiveB / runsPitcher$runFive)
  runsPitcher$runsSevenPercP <- ifelse(runsPitcher$runSeven < 15, 0.015, runsPitcher$runSevenB / runsPitcher$runSeven)
  runsPitcher <- runsPitcher[,c('pitcher','runsOnePercP','runsTwoPercP','runsThreePercP','runsFourPercP','runsFivePercP','runsSevenPercP')]
  
  ## Calc earned runs per pitcher
  
  er <- group_by(runsDF, pitcher)
  er <- as.data.frame(summarise(er, 
                                totalRuns = sum(score),
                                earnedRuns = sum(earned)))
  
  er$erPerc <- er$earnedRuns / er$totalRuns
  er <- er[,c('pitcher','totalRuns','erPerc')]
  
  ## Merge earned runs with pitchers
  
  runsPitcher <- merge(runsPitcher, er, by = 'pitcher', all.x = T)
  runsPitcher[is.na(runsPitcher)] <- 0
  
  theList <- list()
  theList[[1]] <- runsBatter
  theList[[2]] <- runsPitcher
  
  return(theList)
}

runsData <- runs(dat)

batRuns <- runsData[[1]]
pitchRuns <- runsData[[2]]

## Calc number of pitches per pitcher & per team (from a batting perspective)

pitchRxPitches <- function(directory, oldFile, newFile){
  atbat <- newFile[['atbat']]
  atbat <- atbat[,c('pitcher','num','inning','inning_side','o','pitcher_name','gameday_link')]
  atbat <- atbat[order(atbat$gameday_link,atbat$num),]
  atbat$lag.inning_side <- lag(atbat$inning_side)
  atbat$lag.inning_side <- ifelse(is.na(atbat$lag.inning_side),'',atbat$lag.inning_side)
  atbat$lead.pitcher <- lead(atbat$pitcher)
  atbat$lead.pitcher <- ifelse(is.na(atbat$lead.pitcher),'',atbat$lead.pitcher)
  atbat$lag.o <- lag(atbat$o)
  atbat$lag.o <- ifelse(is.na(atbat$lag.o),0,atbat$lag.o)
  atbat$lag.o <- ifelse(atbat$lag.o==3,0,atbat$lag.o)
  atbat$lead.o <- lead(atbat$o)
  atbat$lead.o <- ifelse(is.na(atbat$lead.o),0,atbat$lead.o)
  atbat$lead.o <- ifelse(atbat$lead.o==3,0,atbat$lead.o)
  atbat$outs <- atbat$o-atbat$lag.o
  games <- sort(unique(atbat$gameday_link))
  atbat$gameday_link.temp <- atbat$gameday_link
  atbat$pitcher.game <- paste(atbat$pitcher,atbat$gameday_link)
  atbat <- data.frame(cSplit(atbat,'gameday_link.temp',sep = '_'))
  atbat$gameday_link.temp_5 <- as.character(atbat$gameday_link.temp_5)
  atbat$gameday_link.temp_6 <- as.character(atbat$gameday_link.temp_6)
  atbat$opponent <- ifelse(atbat$inning_side=='top',atbat$gameday_link.temp_6,atbat$gameday_link.temp_5)
  atbat$opponent <- substr(atbat$opponent,1,3)
  atbat$start <- ifelse(atbat$num==1|(atbat$inning==1 & atbat$inning_side=='bottom' & (atbat$lag.inning_side!=atbat$inning_side)),1,0)
  atbat$ones <- 1
  pitchers.list <- split(atbat,atbat$pitcher.game)
  pitchers <- lapply(pitchers.list, function(x){
    num.rows <- nrow(x)
    x$pitcher.game <- NULL
    x <- x[order(x$gameday_link,x$num),]
    #x$start <- ifelse(x$num==1 |  (x$inning==1 & x$inning_side=='bottom' & (x$lag.inning_side!=x$inning_side)),1,0)
    x$start <- ifelse(x$start[1]==1,1,0)
    x$total.outs <- sum(x$outs)
    x$batters.faced <- num.rows
    x$first.batter.inning <- x$inning[1]
    x$first.batter.outs <- x$lag.o[1]
    x$first.batter <- paste(x$inning[1],x$lag.o[1])
    x$last.batter <- paste(x$inning[num.rows],x$o[num.rows])
    x$innings.pitched <- x$total.outs/3
    x <- x[1,]
  })
  pitchers <- do.call(rbind.data.frame, pitchers)
  rownames(pitchers) <- NULL
  pitchers <- pitchers[,c('pitcher','pitcher_name','gameday_link','opponent','start','total.outs','batters.faced',
                          'first.batter','last.batter','first.batter.inning','first.batter.outs','innings.pitched')]
  
  ## Read in old data
  
  setwd(directory)
  if(file.exists(oldFile)){
    oldData <- read.csv(oldFile, stringsAsFactors = F)
    pitchers <- as.data.frame(rbind(pitchers, oldData))    
  }
  
  write.csv(pitchers, 'numPitches.csv', row.names = F)
  
  ## Create average number of pitchers by pitcher
  
  pitchers <- data.table(pitchers)
  pitcher.summary <- pitchers[,':='(mean.outs=mean(total.outs),mean.batters=mean(batters.faced),
                                    mean.entry.outs=mean(3*(first.batter.inning-1)+first.batter.outs)),by=pitcher]
  pitcher.summary <- unique(pitcher.summary,by=c('pitcher'))[,c('pitcher','pitcher_name','mean.outs','mean.batters','mean.entry.outs'),with=F]
  pitchers <- pitchers[,c("pitcher","pitcher_name","gameday_link","opponent","start","total.outs","batters.faced",
                          "first.batter","last.batter","first.batter.inning","first.batter.outs","innings.pitched" ),with=F]
  pitcher.summary <- pitcher.summary[order(pitcher.summary$pitcher_name),]
  pitcher.summary$pitcher_name <- NULL
  return(pitcher.summary)
}
pitchRxBatPitches <- function(directory, oldFile, newFile){
  atbat <- newFile[['atbat']]
  atbat <- atbat[,c('pitcher','num','inning','inning_side','o','pitcher_name','gameday_link')]
  atbat <- atbat[order(atbat$gameday_link,atbat$num),]
  atbat$lag.inning_side <- lag(atbat$inning_side)
  atbat$lag.inning_side <- ifelse(is.na(atbat$lag.inning_side),'',atbat$lag.inning_side)
  atbat$lead.pitcher <- lead(atbat$pitcher)
  atbat$lead.pitcher <- ifelse(is.na(atbat$lead.pitcher),'',atbat$lead.pitcher)
  atbat$lag.o <- lag(atbat$o)
  atbat$lag.o <- ifelse(is.na(atbat$lag.o),0,atbat$lag.o)
  atbat$lag.o <- ifelse(atbat$lag.o==3,0,atbat$lag.o)
  atbat$lead.o <- lead(atbat$o)
  atbat$lead.o <- ifelse(is.na(atbat$lead.o),0,atbat$lead.o)
  atbat$lead.o <- ifelse(atbat$lead.o==3,0,atbat$lead.o)
  atbat$outs <- atbat$o-atbat$lag.o
  games <- sort(unique(atbat$gameday_link))
  atbat$gameday_link.temp <- atbat$gameday_link
  atbat$pitcher.game <- paste(atbat$pitcher,atbat$gameday_link)
  atbat <- data.frame(cSplit(atbat,'gameday_link.temp',sep = '_'))
  atbat$gameday_link.temp_5 <- as.character(atbat$gameday_link.temp_5)
  atbat$gameday_link.temp_6 <- as.character(atbat$gameday_link.temp_6)
  atbat$opponent <- ifelse(atbat$inning_side=='top',atbat$gameday_link.temp_6,atbat$gameday_link.temp_5)
  atbat$opponent <- substr(atbat$opponent,1,3)
  atbat$start <- ifelse(atbat$num==1|(atbat$inning==1 & atbat$inning_side=='bottom' & (atbat$lag.inning_side!=atbat$inning_side)),1,0)
  atbat$ones <- 1
  temp.atbat <- data.table(atbat)
  temp.atbat$ones <- 1
  team.outs <- temp.atbat[,.(total.outs=sum(outs),total.atbats=sum(ones)),by=opponent]
  team.outs <- team.outs[order(team.outs$opponent),]
  colnames(team.outs)[1] <- 'team'
  team.outs <- team.outs[!team.outs$team %in% c('aas','nas')] ## remove all-star games
  temp.atbat <- data.frame(atbat)
  team.games <- split(temp.atbat[,c('gameday_link','opponent')],temp.atbat$opponent)
  team.games <- lapply(team.games,function(x){
    x <- x[!duplicated(x$gameday_link),]
    x$num.games <- nrow(x)
    return(x)})
  team.games <- do.call(rbind.data.frame, team.games)
  team.games$team <- substr(rownames(team.games),1,3)
  rownames(team.games) <- NULL
  team.games <- team.games[,c(4,1:3)]
  team.games <- team.games[,c(3,4)]
  team.games <- team.games[!duplicated(team.games),]
  team.games <- team.games[!team.games$opponent %in% c('aas','nas'),]
  team.games <- team.games[order(team.games$opponent),]
  colnames(team.games)[1] <- 'team'
  team.games <- merge(team.games,team.outs,by='team')
  
  ## Read in old data
  if(file.exists(oldFile)){
    oldData <- read.csv(oldFile, stringsAsFactors = F)
    team.gamesTotal <- merge(oldData, team.games, by = 'team', all.x = T)
    team.gamesTotal[is.na(team.gamesTotal)] <- 0
    team.gamesTotal$num.games <- team.gamesTotal$num.games.x + team.gamesTotal$num.games.y
    team.gamesTotal$total.outs <- team.gamesTotal$total.outs.x + team.gamesTotal$total.outs.y
    team.gamesTotal$total.atbats <- team.gamesTotal$total.atbats.x + team.gamesTotal$total.atbats.y
    team.games <- team.gamesTotal[,c(1,8:10)] 
  }
  
  write.csv(team.games, 'teamPitches.csv', row.names = F)
  
  pitchers <- read.csv('numPitches.csv', stringsAsFactors = F)
  pitchers <- data.table(pitchers)
  pitcher.summary <- pitchers[,':='(mean.outs=mean(total.outs),mean.batters=mean(batters.faced),
                                    mean.entry.outs=mean(3*(first.batter.inning-1)+first.batter.outs)),by=pitcher]
  pitcher.summary <- unique(pitcher.summary,by=c('pitcher'))[,c('pitcher','pitcher_name','mean.outs','mean.batters','mean.entry.outs'),with=F]
  pitchers <- pitchers[,c("pitcher","pitcher_name","gameday_link","opponent","start","total.outs","batters.faced",
                          "first.batter","last.batter","first.batter.inning","first.batter.outs","innings.pitched" ),with=F]
  team.summary.vs.starters <- pitchers[pitchers$start==1][,.(mean.outs.vs.starters=mean(total.outs),
                                                             mean.batters.vs.starters=mean(batters.faced)),by=opponent]
  team.summary.vs.starters <- team.summary.vs.starters[order(team.summary.vs.starters$opponent),]
  colnames(team.summary.vs.starters)[1] <- 'team'
  team.games <- merge(team.games,team.summary.vs.starters,by='team')
  team.games$outsPerGame <- team.games$total.outs/team.games$num.games
  team.games$AtBatsPerGame <- team.games$total.atbats/team.games$num.games
  team.games$mean.outs.vs.bullpen <- team.games$outsPerGame-team.games$mean.outs.vs.starters
  team.games$mean.batters.vs.bullpen <- team.games$AtBatsPerGame-team.games$mean.batters.vs.starters
  team.games <- team.games[,c('team','mean.outs.vs.starters','mean.batters.vs.starters','mean.outs.vs.bullpen','mean.batters.vs.bullpen')]
  return(team.games)
}

pitcherPitches <- pitchRxPitches('~/Desktop/Data Upload', 'numPitches.csv', dat)
batterPitches <- pitchRxBatPitches('~/Desktop/Data Upload', 'teamPitches.csv', dat)

## Run to match player names with their codes - not necessary for modeling, used for matching player names with RotoGrinders
## Will also have to create a manual match list between RotoGrinders names & these name

playerMatch <- function(newDF, var1, var2, directory) {
  if(var1 == 'pitcher') {
    g <- newDF[,c(var1, var2, 'road', 'home', 'date', 'inning_side', 'p_throws')]
  } else {
    g <- newDF[,c(var1, var2, 'road', 'home', 'date','inning_side')]
  }
  g <- g[rev(order(g$date)),]
  g$dups <- duplicated(g[,1])
  g <- filter(g, dups == F)
  g$dups <- NULL
  g$team <- ifelse(g$inning_side == 'top' & var1 == 'batter', g$road,
                   ifelse(g$inning_side == 'bottom' & var1 == 'pitcher', g$road, g$home))
  g$road <- NULL
  g$home <- NULL
  g$inning_side <- NULL
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
                                                                                                                                                                                          ifelse(g[,2] == 'Jungho Kang', 'Jung-Ho Kang',
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
                                                                                                                                                                                                                                                                                                                        ifelse(g[,2] == 'Zachary Cozart', 'Zack Cozart', as.character(g[,2])))))))))))))))))))))))))))))))))))))))))))))
  setwd(directory)
  write.csv(g, paste0(var2,'.csv'), row.names = F)
  return(g)
}

batterLookup <- playerMatch(atbat, 'batter', 'batter_name', '~/Desktop/Data Upload')
pitcherLookup <- playerMatch(atbat, 'pitcher', 'pitcher_name', '~/Desktop/Data Upload')

## Converts data into the proper format

dataConvert <- function(df) {
  df[,'score'] <- as.logical(df[,'score'])
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                         as.factor)
  return(df)
}
atbat <- dataConvert(atbat)

## Note pitchRxPitch pulls in historic as well as new info, so pitch df has everything
## pitchDataManip gets it in proper format for clustering
pitchDataManip <- function(df){
  df <- pitch
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
  library(dplyr)
  fb <- group_by(fastball, pitcher)
  fbSumm <- as.data.frame(
    summarise(fb, FmphMean = mean(start_speed),
              FhMovMean = mean(abs(pfx_x)), FvMovMean = mean(pfx_z), 
              FheightMean = mean(pz),
              FmeanAngle = mean(abs(break_angle)), FmeanBreakLength = mean(break_length), 
              FmeanSpinRate = mean(spin_rate)))
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

scatPlot <- function(dataframe, var1, var2) {
  ggplot(dataframe, aes_string(x = var1, y = var2)) +
    geom_point() +
    geom_smooth()
}
scatPlot(pitchCluster,'locInitMean', 'heightInitMean')

## Runs clustering analysis of all pitchers - set up to have 4 clusters
## Note the number of 

pitchClustering <- function(df) {
  scaledDF <- scale(df[,c(2:23,26:length(df))])
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
bwPlot(finalPitch, colnames(finalPitch)[27])
colnames(finalPitch)

## Creates modeling file - update with pitch type
## Not included in outcome variable: Batters - Runs
## Pitchers - Win, Earned Run Allowed, Complete Game, Complete Game Shutout

batterModelFile <- function(df, fp) {
  library(reshape2)
  
  ## Merge in pitcher type to dataset - based on clustering analysis
  df <- merge(df, fp[,c('pitcher', 'cluster')], by = 'pitcher', all.x = T)
  library(dplyr)
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
  
  ## Pull in cluster number & pitcher throwing type
  
  leftyMixSpeed$cluster <- 1
  leftyPower$cluster <- 2
  leftySinker$cluster <-3
  leftySidearm$cluster <- 4
  leftyGen$cluster <- 5
  rightyMixSpeed$cluster <- 1
  rightyPower$cluster <- 2
  rightySinker$cluster <- 3
  rightySidearm$cluster <- 4
  rightyGen$cluster <- 5
  
  leftyMixSpeed$p_throws <- 'L'
  leftyPower$p_throws <- 'L'
  leftySinker$p_throws <-'L'
  leftySidearm$p_throws <- 'L'
  leftyGen$p_throws <- 'L'
  rightyMixSpeed$p_throws <- 'R'
  rightyPower$p_throws <- 'R'
  rightySinker$p_throws <- 'R'
  rightySidearm$p_throws <- 'R'
  rightyGen$p_throws <- 'R'
  
  ## Create dataframe that stacks all types of pitchers
  
  totalBat <- as.data.frame(rbind(leftyMixSpeed,
                                  rbind(leftyPower,
                                        rbind(leftySinker,
                                              rbind(leftySidearm,
                                                    rbind(leftyGen,
                                                          rbind(rightyMixSpeed,
                                                                rbind(rightyPower,
                                                                      rbind(rightySinker,
                                                                            rbind(rightySidearm, rightyGen))))))))))
  return(totalBat)
}
pitcherModelFile <- function(df) {
  ## Pitcher stats - same as for batter, find probability of each outcome by righty/lefty
  pitchRightyOutcome <- filter(df, stand == 'R')
  pitchLeftyOutcome <- filter(df, stand == 'L')
  
  pitchRightyOutcome <- dcast(pitchRightyOutcome, pitcher ~ outcome, fun.aggregate = length, value.var = 'outcome')
  pitchRightyOutcome$total <- pitchRightyOutcome$`1` + pitchRightyOutcome$`2` + pitchRightyOutcome$`3` + pitchRightyOutcome$`4` + pitchRightyOutcome$`5` + pitchRightyOutcome$`6`+ pitchRightyOutcome$`7`
  pitchLeftyOutcome <- dcast(pitchLeftyOutcome, pitcher ~ outcome, fun.aggregate = length, value.var = 'outcome')
  pitchLeftyOutcome$total <- pitchLeftyOutcome$`1` + pitchLeftyOutcome$`2` + pitchLeftyOutcome$`3` + pitchLeftyOutcome$`4` + pitchLeftyOutcome$`5` + pitchLeftyOutcome$`6`+ pitchLeftyOutcome$`7`
  
  perAB <- function(x) {
    for( i in 2:(length(x)-1)) {
      x[[i]] <- x[[i]] / x[['total']]
    }
    return(x)
  }
  
  pitchRightyOutcome <- perAB(pitchRightyOutcome)
  pitchLeftyOutcome <- perAB(pitchLeftyOutcome)
  
  pitchRightyOutcome$stand <- 'R'
  pitchLeftyOutcome$stand <- 'L'
  pitchTotal <- as.data.frame(rbind(pitchRightyOutcome, pitchLeftyOutcome))
  
  return(pitchTotal)
}

batMod <- batterModelFile(atbat, finalPitch)
pitchMod <- pitcherModelFile(atbat)

## Calculate Aggregate bullpen stats

bullpenAggregation <- function(pitcherLookup, pitcherPitches, pitchModeling) {
  
  ## Merge pitcherlookup database with pitcherPitches to determine who is a reliever
  
  pitchingTotal <- merge(pitcherLookup, pitcherPitches, on = 'pitcher', all.x = T)
  pitchingTotal$type <- ifelse(pitchingTotal$mean.entry.outs > 9, 'RP', 'SP')
  
  ## Create bullpen database
  
  bullpen <- filter(pitchingTotal, type == 'RP')
  bullpen2 <- bullpen
  bullpen$stand <- 'R'
  bullpen2$stand <- 'L'
  bullpen <- as.data.frame(rbind(bullpen, bullpen2))
  bullpen <- merge(bullpen, pitchMod, by = c('pitcher', 'stand'), all.x = T)
  bullpen <- filter(bullpen, total > 5)
  bullpen$teamHand <- paste0(bullpen$team,'_',bullpen$stand)
  
  ## Calc Average Bullpen Outs relative to batters
  ## Group by team to find total outs registered as a team by the bullpen
  
  bullpenG <- group_by(bullpen, teamHand)
  bullpenG <- as.data.frame(summarise(bullpenG, totalBatters = sum(total)))
  bullpenNew <- merge(bullpen, bullpenG, by = 'teamHand', all.x = T)
  bullpenNew$cont <- bullpenNew$total / bullpenNew$totalBatters
  
  ## Group by again to calculate mean difference in batters faced vs. outs gotten by team
  
  bullpenG <- group_by(bullpenNew, team)
  bullpenG <- as.data.frame(summarise(bullpenG,
                                      allBatters = sum(total)))
  bullpenNew <- merge(bullpenNew, bullpenG, by = 'team')
  bullpenNew$contTotal <- bullpenNew$total / bullpenNew$allBatters
  bullpenNew$contCalc <- bullpenNew$contTotal * (bullpenNew$mean.batters / bullpenNew$mean.outs)
  
  bullpenG <- group_by(bullpenNew, team)
  bullpenG <- as.data.frame(summarise(bullpenG,
                                      outsRatio = sum(contCalc)))
  bullpenNew <- merge(bullpenNew, bullpenG, by = 'team')
  bullpenNew <- bullpenNew[,c('team', 'stand', 'pitcher', 'cont','outsRatio')]
  return(bullpenNew)
}
bullpenData <- bullpenAggregation(pitcherLookup, pitcherPitches, pitchMod)

## Create one dataset used for modeling that has batter & pitcher stats for each at bat, as well as outcome

## If creating file for the first time - use modelDF as modelData
## Options for type are: new & historic. Use new for new predictions, historic for model building

modelFile <- function(predictData, clusterData, batterData, pitcherData, modelingData, type) {
  if(type == 'historic'){
    df <- merge(predictData, clusterData[,c('pitcher', 'cluster')], on = 'pitcher', all.x = T)
    df$cluster[is.na(df$cluster)] <- 5
    modelDF <- df[,c('batter', 'pitcher', 'stand', 'p_throws', 'home', 'outcome', 'cluster')]
  } else {
    modelDF <- predictData
    modelDF$lineupSpot <- NULL
    modelDF$pitchTeam <- NULL
    modelDF$type <- NULL
  }
  modelDF <- merge(modelDF, batterData, on = c('batter', 'p_throws', 'cluster'), all.x = T)
  
  ## For batters with less than 15 ABs change cluster to match up with generic lefty/righty splits
  ## Purpose - not enough sample size for pitching clusters
  
  modelDF$cluster <- ifelse(modelDF$total < 5, 5, modelDF$cluster)
  if(type == 'historic'){
    modelDF <- modelDF[,c('batter', 'pitcher', 'p_throws', 'cluster', 'stand', 'home', 'outcome')]
  } else {
    modelDF <- modelDF[,c('batter', 'pitcher', 'p_throws', 'cluster', 'stand')]
  }
  modelDF$p_throws <- as.character(modelDF$p_throws)
  modelDF$stand <- as.character(modelDF$stand)
  
  ## Remerge after converting cluster to generic for batters with low sample size
  
  modelDF <- merge(modelDF, batterData, on = c('batter', 'p_throws', 'cluster'), all.x = T)
  
  ## Convert missing (no historic data) to 0, then batters with very few atbats to averages
  ## 30 cutoff is arbitrary, as are 0.75 & 1.25 multipliers
  ## Idea is people with few at bats are more likely to be worse than those who have more
  
  modelDF[is.na(modelDF)] <- 0
  modelDF$`1` <- ifelse(modelDF$total < 10, .0755, modelDF$`1`)
  modelDF$`2` <- ifelse(modelDF$total < 10, .0223, modelDF$`2`)
  modelDF$`3` <- ifelse(modelDF$total < 10, .0025, modelDF$`3`)
  modelDF$`4` <- ifelse(modelDF$total < 10, .0135, modelDF$`4`)
  modelDF$`5` <- ifelse(modelDF$total < 10, .0432, modelDF$`5`)
  modelDF$`6` <- ifelse(modelDF$total < 10, .3068, modelDF$`6`)
  modelDF$`7` <- ifelse(modelDF$total < 10, .5361, modelDF$`7`)
  head(modelDF)
  if(type == 'historic') {
    colnames(modelDF)[8:15] <- c('batOne', 'batTwo', 'batThree', 'batFour', 'batFive', 'batSix', 'batSeven', 'batTotal')
  } else {
    colnames(modelDF)[6:13] <- c('batOne', 'batTwo', 'batThree', 'batFour', 'batFive', 'batSix', 'batSeven', 'batTotal')
  }
  ## Merge in Pitcher Data
  
  modelDF <- merge(modelDF, pitcherData, by = c('pitcher', 'stand'), all.x = T)
  
  ## Take care of pitchers with less than five batters faced
  
  modelDF[is.na(modelDF)] <- 0
  modelDF$`1` <- ifelse(modelDF$total < 10, .2267, modelDF$`1`)
  modelDF$`2` <- ifelse(modelDF$total < 10, .0670, modelDF$`2`)
  modelDF$`3` <- ifelse(modelDF$total < 10, .0075, modelDF$`3`)
  modelDF$`4` <- ifelse(modelDF$total < 10, .0405, modelDF$`4`)
  modelDF$`5` <- ifelse(modelDF$total < 10, .1299, modelDF$`5`)
  modelDF$`6` <- ifelse(modelDF$total < 10, .1023, modelDF$`6`)
  modelDF$`7` <- ifelse(modelDF$total < 10, .4259, modelDF$`7`)
  if(type == 'historic') {
    colnames(modelDF)[16:length(modelDF)] <- c('pitchOne', 'pitchTwo', 'pitchThree', 'pitchFour', 'pitchFive', 'pitchSix', 'pitchSeven', 'pitchTotal')
  } else {
    colnames(modelDF)[14:length(modelDF)] <- c('pitchOne', 'pitchTwo', 'pitchThree', 'pitchFour', 'pitchFive', 'pitchSix', 'pitchSeven', 'pitchTotal')
  }
  if(type == 'historic') {
    write.csv(modelDF, 'modelFile.csv', row.names = F)  
  } else {
    write.csv(modelDF, paste0('modelFileNew_',date,'.csv', row.names = F))
  }
  modelDF <- modelDF[order(-modelDF$batter, modelDF$pitcher),]
  modelDF$batter <- ifelse(modelDF$batter == 0, row.names(modelDF), modelDF$batter)
  return(modelDF)
}
newData <- modelFile(atbat, finalPitch, batMod, pitchMod, modelData, 'historic')

###### MODELING !! ######
## use fullData dataset ##

library(xgboost)
library(devtools)
library(Ckmeans.1d.dp)

rand <- runif(nrow(newData), min = 0, max = 1)
newData$rand <- rand

train <- filter(newData, rand > .25)
test <- filter(newData, rand <= .25)

y <- train$outcome - 1
x <- test$outcome

train <- train[,c(8:14,16:22)]
test <- test[,c(8:14,16:22)]

trainMatrix <- as.matrix(train)
testMatrix <- as.matrix(test)

## XGboost

numberOfClasses <- length(unique(y))

params <- list('objective' = 'multi:softprob',
               'eval_metric' = 'mlogloss',
               'num_class' = numberOfClasses,
               'eta' = .05,
               'subsample' = .70,
               'max_delta_step' = 5
)

cv.nround <- 5
cv.nfold <- 3

boost.cv <- xgb.cv(param=params, 
                   data=trainMatrix, 
                   label = y, 
                   nfold = cv.nfold, 
                   nrounds = cv.nround)

boost2 <- xgboost(param = params,
                  data = trainMatrix,
                  label = y,
                  nrounds = 150)

## Optional Model Evaluation

#model <- xgb.dump(boost2, with.stats = T)

#names <- dimnames(trainMatrix)[[2]]

#importance_matrix <- xgb.importance(names, model = boost2)

#xgb.plot.importance(importance_matrix)
#importance <- xgb.importance(feature_names = names, model = boost2)

#importance$Gain <- round(importance$Gain, 3)
#importance$Cover <- round(importance$Cover, 3)
#importance$Frequence <- round(importance$Frequence, 3)

#importance

## Turn predictions into evaluation using test dataset

prob <- as.data.frame(matrix(predict(boost2, testMatrix), ncol = 7, byrow = T))

prob$outcome <- x
prob$error <- as.numeric(ifelse(prob$outcome == 7, 1 - prob$V7,
                                ifelse(prob$outcome == 6, 1 - prob$V6,
                                       ifelse(prob$outcome == 5, 1 - prob$V5,
                                              ifelse(prob$outcome == 4, 1 - prob$V4,
                                                     ifelse(prob$outcome == 3, 1 - prob$V3,
                                                            ifelse(prob$outcome == 2, 1 - prob$V2,
                                                                   ifelse(prob$outcome == 1, 1 - prob$V1, 0))))))))

## Predicted probabilities relative to accuracy

err <- 0

err[1] <- round((1- mean(prob[prob$outcome == 1,'error'])) / (nrow(prob[prob$outcome == 1,]) / nrow(prob)),3)
err[2] <- round((1- mean(prob[prob$outcome == 2,'error'])) / (nrow(prob[prob$outcome == 2,]) / nrow(prob)),3)
err[3] <- round((1- mean(prob[prob$outcome == 3,'error'])) / (nrow(prob[prob$outcome == 3,]) / nrow(prob)),3)
err[4] <- round((1- mean(prob[prob$outcome == 4,'error'])) / (nrow(prob[prob$outcome == 4,]) / nrow(prob)),3)
err[5] <- round((1- mean(prob[prob$outcome == 5,'error'])) / (nrow(prob[prob$outcome == 5,]) / nrow(prob)),3)
err[6] <- round((1- mean(prob[prob$outcome == 6,'error'])) / (nrow(prob[prob$outcome == 6,]) / nrow(prob)),3)
err[7] <- round((1- mean(prob[prob$outcome == 7,'error'])) / (nrow(prob[prob$outcome == 7,]) / nrow(prob)),3)

err

####### ROTOGRINDERS SALARY DATA PULL ###########

## Insert date in YYYY-MM-DD form & location you'd like to save file in quotes
## ex - salaryData('2016-04-05', '~/Documents/Northwestern/498/MLB Scraped')

## Pull rotogrinders data from given day

salaryData <- function(day, location) {
  
  ## Load required packages
  
  library(rvest)
  library(splitstackshape)
  
  ## Create list of daily fantasy sites - for url. 
  ## Also create empty list for storing list of sites, & number for iterating
  ## Note - could add sites to this list, & function would be able to handle
  
  sites <- c('draftkings', 'fanduel', 'yahoo')
  urlList <- list()
  a <- 0
  ## Get URL using day parameter & list of fantasy sites
  
  for (s in sites) {
    a <- a + 1
    siteURL <- read_html(paste0('https://rotogrinders.com/lineups/mlb?date=',day,'&site=',s))
    urlList[[a]] <- siteURL
  }
  
  ## Create empty list for storing salaries
  
  salaryList <- list()
  
  ## Loop through each site & pull salaries for each available player
  
  for (c in 1:length(sites))  { 
    
    ## Pull relevant information from grinders website
    
    grinders <- urlList[[1]] %>%
      html_nodes('.players .player-popup , .mascot, .meta, .status .stats, .stats .salary, .position, .pname .player-popup') %>%
      html_text(trim =T)
    ## Make missing entries 0 - note this is for players who are playing but do NOT have a salary
    ## This is important to note because players with salary 0 should be ignored
    
    grinders[grinders==""] <- '0'
    
    ## Remove extraneous text coming from rvest pull
    
    grinders <- gsub('\\$', '', grinders)
    grinders <- gsub('\\n', '', grinders)
    grinders <- gsub("                            ", "_", grinders)
    grinders <- gsub("                        ", "_", grinders)

    ## Put player data into a data frame organized by game
    if(length(grinders) %% 78 != 0) {
      message('Some Unknown Person or People is/are playing in one/some of the games')
      missingCheck <- function() {
        len <- length(grinders)
        probs <- c(rep(T, len))
        b <- 78 - length(grinders) %% 78
        checker <- '_'
        for (i in c(42,82,120,160,198,238,276,316,354,394,432,472,510,550,588,628,666,706,744,784,822,862,900,940,978,1018,1056,1096,1134)) {
          check1 <- ifelse(substr(grinders[i],2,2) %in% checker, 'none', length(grinders[1:i]))
          if (check1 != 'none') break
          if (i > length(grinders)) break
        }
        id1 <- c(seq_along(probs), check1 - 4.5, check1 - 4.4, check1 - 4.3, check1 - 4.3)
        id2 <- c(seq_along(probs), check1 - 2.5, check1 - 2.4, check1 + 22.5, check1 + 22.6, check1 + 22.7, check1 + 22.8)
        id3 <- c(seq_along(probs), check1 - 2.5, check1 - 2.4)
        for (i in 1:b) {
          grinders[len+i] <- 'missing_missing'
        }
        
        grinders <- if(b%%4 == 0) {
          grinders[order(id1)]
        } else if (b%%6 == 0 & b%%3 == 0) {
          grinders[order(id2)]
        } else grinders[order(id3)]
        return(grinders)
      }
      grinders <- missingCheck()
    }
    
    grinders <- as.data.frame(matrix(grinders, 
                                     nrow = (length(grinders)/78), 
                                     byrow=T), 
                              stringsAsFactors = F)
    
    ## Create a list of NL teams - used for removing pitchers being double counted in games played in NL Parks    
    
    nlList <- c('Marlins', 'Mets', 'Nationals', 'Phillies', 'Braves',
                'Cubs', 'Pirates', 'Reds', 'Brewers', 'Cardinals',
                'Rockies', 'Dbacks', 'Padres', 'Giants', 'Dodgers')
    
    ## Create an empty list for formatting players within the same DF
    
    playerStack <- list()
    
    ## Function that stacks players by lineup spot within each game
    ## Necessary based on formatting of original data pull
    
    stacker <- function(df) {
      l <- 0
      for (i in seq(5, 37, 4)) {
        l <- l + 1
        n <- i + 1
        j <- i + 2
        q <- i + 3
        d <- df[,c(1,2,i,n,j,q)]
        d$lineupSpot <- l
        d$side <- 'either'
        colnames(d) <- c('team', 'opponent', 'player', 'position', 'stand','salary','lineupSpot','side')
        playerStack[[l]] <- d
      }
      s <- 0
      for (i in seq(43, 75, 4)) {
        s <- s + 1
        l <- l + 1
        n <- i + 1
        j <- i + 2
        q <- i + 3
        d <- df[,c(2,1,i,n,j,q)]
        d$lineupSpot <- s
        d$side <- 'either'
        colnames(d) <- c('team', 'opponent', 'player', 'position', 'stand','salary','lineupSpot','side')
        playerStack[[l]] <- d
      }
      
      ## Note - pitchers handled separately here because of additional extraneous data that must be removed
      
      l <- l + 1
      roadPitch <- grinders[,c(1:4)]
      roadPitch$side <- 'road'
      homePitch <- grinders[,c(2,1,41,42)]
      homePitch$side <- 'home'
      colnames(roadPitch) <- c('team','opponent','player','attributes','side')
      colnames(homePitch) <- c('team','opponent','player','attributes','side')
      roadPitch <- roadPitch[!roadPitch$opponent %in% nlList,]
      homePitch <- homePitch[!homePitch$team %in% nlList,]
      totalPitch <- as.data.frame(rbind(roadPitch, homePitch), stringsAsFactors = F)
      totalPitch <- as.data.frame(cSplit(totalPitch, 
                                         'attributes', 
                                         sep='_'), stringsAsFactors = F)
      totalPitch <- totalPitch[,c(1:4,6,5)]
      totalPitch$position <- 'pitcher'
      totalPitch$lineupSpot <- 9
      totalPitch <- totalPitch[,c(1:3,7,6,5,8,4)]
      colnames(totalPitch) <- c('team', 'opponent', 'player', 'position', 'stand', 'salary', 'lineupSpot', 'side')
      totalPitch[,c(1:3)] <- sapply(totalPitch[,c(1:3)], as.character)
      colnames(totalPitch)
      colnames(d)
      ## Stack pitchers onto existing postional players file, then convert to data frame
      
      playerStack[[l]] <- totalPitch
      playerDF <- do.call(rbind.data.frame, playerStack)
      playerDF$salary <- gsub('K', 
                              '', 
                              playerDF$salary)
      playerDF$salary <- as.numeric(as.character(playerDF$salary))
      playerDF <- playerDF[order(playerDF$team,
                                 playerDF$opponent,
                                 playerDF$lineupSpot),]
      
      ## Change column name to salary, and add in variable that indicates the site the salaries come from
      
      playerDF$site <- sites[c]
      return(playerDF)
    }
    
    ## Run this function for each site, then stack on top of each other
    
    grindersList <- stacker(grinders)
    salaryList[[c]] <- grindersList
  }
  
  ## Final output is a dataframe that has the salaries for each site stacked on top of one and other
  
  grindersDF <- do.call(rbind.data.frame, salaryList) 
  grindersDF$salary[is.na(grindersDF$salary)] <- 0
  setwd(location)
  write.csv(grindersDF, paste0(day,"_DailyFantasy.csv"), row.names = F)
  return(grindersDF)
}
newSalary <- salaryData('2016-06-13', '~/Desktop/Data Upload/Scraper')

setwd('~/Desktop/Data Upload')

## Convert Rotogrinders pull into modeling format

newSalaryData <- newSalary
clusterData <- pitchCluster

salaryModel <- function(newSalaryData, clusterData) {
  
  nlList <- c('Marlins', 'Mets', 'Nationals', 'Phillies', 'Braves',
              'Cubs', 'Pirates', 'Reds', 'Brewers', 'Cardinals',
              'Rockies', 'Dbacks', 'Padres', 'Giants', 'Dodgers')
  
  ## Convert team names that come from Rotogrinders to match PitchRx
  teamLookup <- c('Angels', 'Astros', 'Athletics', 'Blue Jays', 'Braves','Brewers','Cardinals','Cubs','Dbacks','Dodgers','Giants','Indians',
                  'Mariners','Marlins','Mets','Nationals','Orioles','Padres','Phillies','Pirates','Rangers','Rays','Red Sox','Reds','Rockies',
                  'Royals','Tigers','Twins','White Sox','Yankees')
  teamAbbrev <- c('ana', 'hou', 'oak', 'tor', 'atl', 'mil', 'sln', 'chn', 'ari', 'lan', 'sfn',
                  'cle', 'sea', 'mia', 'nyn', 'was', 'bal', 'sdn', 'phi', 'pit', 'tex', 'tba', 
                  'bos', 'cin', 'col', 'kca', 'det', 'min', 'cha', 'nya')
  teamLookup <- as.data.frame(cbind(teamLookup, teamAbbrev))
  newSalaryData <- merge(newSalaryData, teamLookup, by.x = 'team', by.y = 'teamLookup')
  newSalaryData <- merge(newSalaryData, teamLookup, by.x = 'opponent', by.y = 'teamLookup')
  colnames(newSalaryData)[10] <- 'team2'
  colnames(newSalaryData)[11] <- 'opponent2'
  
  ## Match Pitcher Name from Rotogrinders to name in PitchRx to get pitcher Code
  
  pitchersRoto <- filter(newSalaryData, position %in% c('SP','P','pitcher'))
  pitchersRoto$dups <- duplicated(pitchersRoto$player)
  pitchersRoto <- filter(pitchersRoto, dups == F)
  pitchersRoto$dups <- NULL
  pitchersRoto <- merge(pitchersRoto, pitcherLookup, by.x = 'player', by.y = 'pitcher_name', all.x = T)
  pitchersRoto <- pitchersRoto[,c('pitcher', 'stand','team2', 'opponent2')]
  colnames(pitchersRoto) <- c('pitcher', 'p_throws', 'pitchTeam', 'pitchOpponent')
  
  ## Match Pitcher Name from Rotogrinders to name in PitchRx to get batter Code
  
  battersRoto <- newSalaryData
  battersRoto$exclude <- ifelse(battersRoto$position == 'pitcher' & battersRoto$side == 'home' & !battersRoto$team %in% nlList, 1,
                                ifelse(battersRoto$position == 'pitcher' & battersRoto$side == 'road' & !battersRoto$opponent %in% nlList, 1, 0))
  battersRoto <- filter(battersRoto, exclude == 0)
  battersRoto$dups <- duplicated(battersRoto$player)
  battersRoto <- filter(battersRoto, dups == F)
  battersRoto$dups <- NULL
  battersRoto <- battersRoto[order(battersRoto$team, battersRoto$lineupSpot),]
  battersRoto <- merge(battersRoto, batterLookup, by.x = 'player', by.y = 'batter_name', all.x = T)
  battersRoto <- battersRoto[,c('batter', 'stand', 'team2', 'lineupSpot', 'opponent2')]
  colnames(battersRoto)[3] <- 'batTeam'
  colnames(battersRoto)[5] <- 'batOpponent'
  
  ## Merge data together to get pitcher-batter matchups for each game
  ## First the starting pitchers
  
  spMatchups <- merge(pitchersRoto, battersRoto, by.x = 'pitchOpponent', by.y = 'batTeam', all.x = T)
  spMatchups$type <- 'SP'
  spMatchups <- spMatchups[,c('pitcher', 'pitchTeam', 'p_throws', 'type', 'batter', 'pitchOpponent', 'stand', 'lineupSpot')]
  colnames(spMatchups)[6] <- 'batTeam'

    ## Create bullpen database

  pitchingTotal <- merge(pitcherLookup, pitcherPitches, on = 'pitcher', all.x = T)
  pitchingTotal$type <- ifelse(pitchingTotal$mean.entry.outs > 9, 'RP', 'SP')
  
  ## Now create all bullpen possible matchups
  
  bullpen <- filter(pitchingTotal, type == 'RP')
  bullMatchups <- merge(battersRoto, bullpen[,c(1,4,5,9)], by.x = 'batOpponent', by.y = 'team')
  bullMatchups <- bullMatchups[,c('pitcher', 'batOpponent', 'p_throws', 'type', 'batter', 'batTeam', 'stand', 'lineupSpot')]
  colnames(bullMatchups)[2] <- 'pitchTeam'
  
  ## Also merge in clustering data to get pitcher's cluster 
  
  totalRoto <- as.data.frame(rbind(spMatchups, bullMatchups))
  totalRoto <- merge(totalRoto, finalPitch[,c('pitcher', 'cluster')], on = 'pitcher', all.x = T)
  totalRoto$cluster[is.na(totalRoto$cluster)] <- 5
  totalRoto$stand <- ifelse(totalRoto$p_throws == 'R' & totalRoto$stand == 'S', 'L',
                            ifelse(totalRoto$p_throws == 'L' & totalRoto$stand == 'S', 'R', totalRoto$stand))
  totalRoto <- totalRoto[order(-totalRoto$batter, totalRoto$pitcher),]
  return(totalRoto)
}
totalRoto <- salaryModel(newSalary, finalPitch)

## Create predictions on new data

newDayData <- modelFile(totalRoto, finalPitch, batMod, pitchMod, modelData, 'new')

newDayData <- as.matrix(newDayData[,c(6:12,14:20)])

probNew <- as.data.frame(matrix(predict(boost2, newDayData), ncol = 7, byrow = T))

## Combine predictions with pitcher Info

## Take expected outcome from model & merge with salary & player info

predictionAggs <- function(probs, predictionFile, bullpenData, salaryInfo, batLookup, pitchLookup, sbB, sbP, rbiB, rbiP, runB, runP, date) {
  
  ## Calculate the number of at bats for each batter vs starters & bullpen
  
  probTotal <- as.data.frame(cbind(predictionFile[,1:8], probs))
  
  ## Pull out just starter information
  
  probStarter <- filter(probTotal, type == 'SP')
  probStarter <- merge(probStarter, pitcherPitches, by = 'pitcher', all.x = T)
  probStarter <- merge(probStarter, batterPitches, by.x ='batTeam', by.y = 'team')
  probStarter$starterOuts <- ifelse(is.na(probStarter$mean.entry.outs), ceiling(probStarter$mean.outs.vs.starters * 0.75), ceiling((probStarter$mean.outs.vs.starters + probStarter$mean.outs) / 2))
  probStarter$starterBatters <- ifelse(is.na(probStarter$mean.entry.outs), ceiling(probStarter$mean.outs.vs.starters * 0.75), ceiling((probStarter$mean.batters.vs.starters + probStarter$mean.batters) / 2))
  probStarter$bullpenOuts <- 27 - probStarter$starterOuts
  
  ## Get bullpen details
  bullpenOutsRatio <- bullpenData
  bullpenOutsRatio$dups <- duplicated(bullpenOutsRatio$team)
  bullpenOutsRatio <- filter(bullpenOutsRatio, dups == F)
  bullpenOutsRatio$dups <- NULL
  probStarter <- merge(probStarter, bullpenOutsRatio[,c('team', 'outsRatio')], by.x = 'pitchTeam', by.y = 'team', all.x = T)
  probStarter$bullpenBatters <- ceiling(probStarter$bullpenOuts * probStarter$outsRatio)
  probStarter$starterMultiplier <- floor((probStarter$starterBatters - probStarter$lineupSpot) / 9 + 1)
  probStarter$bullpenOrder <- probStarter$lineupSpot - (probStarter$starterBatters %% 9)
  probStarter$bullpenOrder <- ifelse(probStarter$bullpenOrder < 1, probStarter$bullpenOrder + 9, probStarter$bullpenOrder)
  probStarter$bullpenMulitplier <- floor(((probStarter$bullpenBatters - probStarter$bullpenOrder) / 9) + 1)
  bullpenMult <- probStarter[,c('pitcher', 'batter', 'pitchTeam', 'batTeam', 'bullpenMulitplier')]
  probStarter$singleAggProb <- probStarter$V1 * probStarter$starterMultiplier
  probStarter$doubleAggProb <- probStarter$V2 * probStarter$starterMultiplier
  probStarter$tripleAggProb <- probStarter$V3 * probStarter$starterMultiplier
  probStarter$hrAggProb <- probStarter$V4 * probStarter$starterMultiplier
  probStarter$walkAggProb <- probStarter$V5 * probStarter$starterMultiplier
  probStarter$strikeoutAggProb <- probStarter$V6 * probStarter$starterMultiplier
  probStarter$otherAggProb <- probStarter$V7 * probStarter$starterMultiplier
  
  ## Merge in Runs data - same fashion as SB/RBI
  ## No arbitrary cut off necessary - handled in runs code (better method than RBIs)
  ## Still need to handle missing cases (when batter/pitcher has never shown up before)
  
  probStarter <- merge(probStarter, runB, by = 'batter', all.x = T)
  probStarter <- merge(probStarter, runP, by = 'pitcher', all.x = T)
  probStarter[is.na(probStarter)] <- 0
  
  ## Calc expected runs per outcome based on batter & pitcher averages
  
  probStarter$run_1 <- ifelse(probStarter$runsFourPerc == 0 & probStarter$runsFourPercP == 0,
                              .20,
                              ifelse(probStarter$runsFourPercP == 0,
                                     probStarter$runsOnePerc,
                                     ifelse(probStarter$runsFourPerc == 0,
                                            probStarter$runsOnePercP,
                                            (probStarter$runsOnePerc + probStarter$runsOnePercP) / 2)))
  
  probStarter$run_2 <- ifelse(probStarter$runsFourPerc == 0 & probStarter$runsFourPercP == 0,
                              .40,
                              ifelse(probStarter$runsFourPercP == 0,
                                     probStarter$runsTwoPerc,
                                     ifelse(probStarter$runsFourPerc == 0,
                                            probStarter$runsTwoPercP,
                                            (probStarter$runsTwoPerc + probStarter$runsTwoPercP) / 2)))
  
  probStarter$run_3 <- 0.75
  probStarter$run_4 <- 1
  probStarter$run_5 <- ifelse(probStarter$runsFourPerc == 0 & probStarter$runsFourPercP == 0,
                              .20,
                              ifelse(probStarter$runsFourPercP == 0,
                                     probStarter$runsFivePerc,
                                     ifelse(probStarter$runsFourPerc == 0,
                                            probStarter$runsFivePercP,
                                            (probStarter$runsFivePerc + probStarter$runsFivePercP) / 2)))
  probStarter$run_7 <- ifelse(probStarter$runsFourPerc == 0 & probStarter$runsFourPercP == 0,
                              .015,
                              ifelse(probStarter$runsFourPercP == 0,
                                     probStarter$runsSevenPerc,
                                     ifelse(probStarter$runsFourPerc == 0,
                                            probStarter$runsSevenPercP,
                                            (probStarter$runsSevenPerc + probStarter$runsSevenPercP) / 2)))
  
  ## Calc Total run probability
  
  probStarter$runTotal <- probStarter$run_1 * probStarter$singleAggProb +
    probStarter$run_2 * probStarter$doubleAggProb +
    probStarter$run_3 * probStarter$tripleAggProb +
    probStarter$run_4 * probStarter$hrAggProb +
    probStarter$run_5 * probStarter$walkAggProb +
    probStarter$run_7 * probStarter$otherAggProb
  
  ## Need to create earned runs for pitchers by multiplying runs for batters * percentage
  
  probStarter$erPerc <- ifelse(probStarter$totalRuns < 20, 0.95, probStarter$erPerc)
  probStarter$erTotal <- probStarter$runTotal * probStarter$erPerc
  
  ## Merge in RBI data - same fashion as SB 
  ## Pick cut off for replacement values (20 ABs .. pretty arbitrary but a good start)
  
  probStarter <- merge(probStarter, rbiB, by = 'batter', all.x = T)
  probStarter <- merge(probStarter, rbiP, by = 'pitcher', all.x = T)
  probStarter[is.na(probStarter)] <- 0
  
  ## Calc expected RBIs per outcome based on batter & pitcher averages
  
  probStarter$rbi_1 <- ifelse(probStarter$batTotal < 20 & probStarter$pitchTotal < 20,
                              0.2010735,
                              ifelse(probStarter$batTotal < 20, 
                                     probStarter$rbiP_1,
                                     ifelse(probStarter$pitchTotal < 20,
                                            probStarter$rbiB_1,
                                            (probStarter$rbiB_1 + probStarter$rbiP_1) / 2)))
  
  probStarter$rbi_2 <- ifelse(probStarter$batTotal < 20 & probStarter$pitchTotal < 20,
                              0.40,
                              ifelse(probStarter$batTotal < 20, 
                                     probStarter$rbiP_2,
                                     ifelse(probStarter$pitchTotal < 20,
                                            probStarter$rbiB_2,
                                            (probStarter$rbiB_2 + probStarter$rbiP_2) / 2)))
  
  probStarter$rbi_3 <- ifelse(probStarter$batTotal < 20 & probStarter$pitchTotal < 20,
                              0.5875556,
                              ifelse(probStarter$batTotal < 20, 
                                     probStarter$rbiP_3,
                                     ifelse(probStarter$pitchTotal < 20,
                                            probStarter$rbiB_3,
                                            (probStarter$rbiB_3 + probStarter$rbiP_3) / 2)))
  
  probStarter$rbi_4 <- ifelse(probStarter$batTotal < 20 & probStarter$pitchTotal < 20,
                              1.565931,
                              ifelse(probStarter$batTotal < 20, 
                                     probStarter$rbiP_4,
                                     ifelse(probStarter$pitchTotal < 20,
                                            probStarter$rbiB_4,
                                            (probStarter$rbiB_4 + probStarter$rbiP_4) / 2)))
  
  probStarter$rbi_5 <- ifelse(probStarter$batTotal < 20 & probStarter$pitchTotal < 20,
                              0.01751584,
                              ifelse(probStarter$batTotal < 20, 
                                     probStarter$rbiP_5,
                                     ifelse(probStarter$pitchTotal < 20,
                                            probStarter$rbiB_5,
                                            (probStarter$rbiB_5 + probStarter$rbiP_5) / 2)))
  
  probStarter$rbi_6 <- 0
  probStarter$rbi_7 <- ifelse(probStarter$batTotal < 20 & probStarter$pitchTotal < 20,
                              0.02535321,
                              ifelse(probStarter$batTotal < 20, 
                                     probStarter$rbiP_7,
                                     ifelse(probStarter$pitchTotal < 20,
                                            probStarter$rbiB_7,
                                            (probStarter$rbiB_7 + probStarter$rbiP_7) / 2)))
  
  ## Calc total RBI averages 
  
  probStarter$rbiTotal <- probStarter$rbi_1 * probStarter$singleAggProb +
    probStarter$rbi_2 * probStarter$doubleAggProb +
    probStarter$rbi_3 * probStarter$tripleAggProb +
    probStarter$rbi_4 * probStarter$hrAggProb +
    probStarter$rbi_5 * probStarter$walkAggProb +
    probStarter$rbi_7 * probStarter$otherAggProb
  
  ## Merge in SB / CS %%s - add in RBIs & Runs here in the futuer
  
  probStarter <- merge(probStarter, sbB, by = 'batter', all.x = T)
  probStarter <- merge(probStarter, sbP, by = 'pitcher', all.x = T)
  probStarter$singleSB[is.na(probStarter$singleSB)] <- .03
  probStarter$doubleSB[is.na(probStarter$doubleSB)] <- .01
  probStarter$singleCS[is.na(probStarter$singleCS)] <- .02
  probStarter$doubleCS[is.na(probStarter$doubleCS)] <- .005
  probStarter[is.na(probStarter)] <- 0
  probStarter$sbpt <- probStarter$sbP + probStarter$csP
  probStarter$sbbt <- probStarter$sb + probStarter$cs
  
  ## Create SB / CS Probs 
  ## Calc'd by taking batter & pitcher averages of steals & CS by outcome type - also includes low sample size handling
  
  probStarter$sbSingleAggProb <- ifelse(probStarter$sbpt < 3 & probStarter$sbbt < 3, mean(probStarter$singleSB) / 2.5,
                                        ifelse(probStarter$sbpt < 3, probStarter$singleSB,
                                               ifelse(probStarter$sbbt < 3, probStarter$singleSBP,
                                                      (probStarter$singleSB + probStarter$singleSBP) / 2)))
  probStarter$sbDoubleAggProb <- ifelse(probStarter$sbpt < 3 & probStarter$sbbt < 3, mean(probStarter$doubleSB) / 2.5,
                                        ifelse(probStarter$sbpt < 3, probStarter$doubleSB,
                                               ifelse(probStarter$sbbt < 3, probStarter$doubleSBP,
                                                      (probStarter$doubleSB + probStarter$doubleSBP) / 2)))
  probStarter$sbTripleAggProb <- ifelse(probStarter$sbpt < 3 & probStarter$sbbt < 3, mean(probStarter$tripleSB) / 2.5,
                                        ifelse(probStarter$sbpt < 3, probStarter$tripleSB,
                                               ifelse(probStarter$sbbt < 3, probStarter$tripleSBP,
                                                      (probStarter$tripleSB + probStarter$tripleSBP) / 2)))
  probStarter$sbHrAggProb <- 0
  probStarter$sbWalkAggProb <- ifelse(probStarter$sbpt < 3 & probStarter$sbbt < 3, mean(probStarter$walkSB) / 2.5,
                                      ifelse(probStarter$sbpt < 3, probStarter$walkSB,
                                             ifelse(probStarter$sbbt < 3, probStarter$walkSBP,
                                                    (probStarter$walkSB + probStarter$walkSBP) / 2)))
  probStarter$sbStrikeoutAggProb <- ifelse(probStarter$sbpt < 3 & probStarter$sbbt < 3, mean(probStarter$strikeoutSB) / 2.5,
                                           ifelse(probStarter$sbpt < 3, probStarter$strikeoutSB,
                                                  ifelse(probStarter$sbbt < 3, probStarter$strikeoutSBP,
                                                         (probStarter$strikeoutSB + probStarter$strikeoutSBP) / 2)))
  probStarter$sbOtherAggProb <- ifelse(probStarter$sbpt < 3 & probStarter$sbbt < 3, mean(probStarter$otherSB) / 2.5,
                                       ifelse(probStarter$sbpt < 3, probStarter$otherSB,
                                              ifelse(probStarter$sbbt < 3, probStarter$otherSBP,
                                                     (probStarter$otherSB + probStarter$otherSBP) / 2)))
  
  ## Caught Stealing
  
  probStarter$csSingleAggProb <- ifelse(probStarter$sbpt < 3 & probStarter$sbbt < 3, mean(probStarter$singleCS) / 2.5,
                                        ifelse(probStarter$sbpt < 3, probStarter$singleCS,
                                               ifelse(probStarter$sbbt < 3, probStarter$singleCSP,
                                                      (probStarter$singleCS + probStarter$singleCSP) / 2)))
  probStarter$csDoubleAggProb <- ifelse(probStarter$sbpt < 3 & probStarter$sbbt < 3, mean(probStarter$doubleCS) / 2.5,
                                        ifelse(probStarter$sbpt < 3, probStarter$doubleCS,
                                               ifelse(probStarter$sbbt < 3, probStarter$doubleCSP,
                                                      (probStarter$doubleCS + probStarter$doubleCSP) / 2)))
  probStarter$csTripleAggProb <- ifelse(probStarter$sbpt < 3 & probStarter$sbbt < 3, mean(probStarter$tripleCS) / 2.5,
                                        ifelse(probStarter$sbpt < 3, probStarter$tripleCS,
                                               ifelse(probStarter$sbbt < 3, probStarter$tripleCSP,
                                                      (probStarter$tripleCS + probStarter$tripleCSP) / 2)))
  probStarter$csHrAggProb <- 0
  probStarter$csWalkAggProb <- ifelse(probStarter$sbpt < 3 & probStarter$sbbt < 3, mean(probStarter$walkCS) / 2.5,
                                      ifelse(probStarter$sbpt < 3, probStarter$walkCS,
                                             ifelse(probStarter$sbbt < 3, probStarter$walkCSP,
                                                    (probStarter$walkCS + probStarter$walkCSP) / 2)))
  probStarter$csStrikeoutAggProb <- ifelse(probStarter$sbpt < 3 & probStarter$sbbt < 3, mean(probStarter$strikeoutCS) / 2.5,
                                           ifelse(probStarter$sbpt < 3, probStarter$strikeoutCS,
                                                  ifelse(probStarter$sbbt < 3, probStarter$strikeoutCSP,
                                                         (probStarter$strikeoutCS + probStarter$strikeoutCSP) / 2)))
  probStarter$csOtherAggProb <- ifelse(probStarter$sbpt < 3 & probStarter$sbbt < 3, mean(probStarter$otherCS) / 2.5,
                                       ifelse(probStarter$sbpt < 3, probStarter$otherCS,
                                              ifelse(probStarter$sbbt < 3, probStarter$otherCSP,
                                                     (probStarter$otherCS + probStarter$otherCSP) / 2)))
  
  ## Create total SB & CS variables
  
  probStarter$sbPerc <- probStarter$singleAggProb * probStarter$sbSingleAggProb +
    probStarter$doubleAggProb * probStarter$sbDoubleAggProb +
    probStarter$tripleAggProb * probStarter$sbTripleAggProb +
    probStarter$walkAggProb * probStarter$sbWalkAggProb +
    probStarter$strikeoutAggProb * probStarter$sbStrikeoutAggProb +
    probStarter$otherAggProb * probStarter$sbOtherAggProb
  
  probStarter$csPerc <- probStarter$singleAggProb * probStarter$csSingleAggProb +
    probStarter$doubleAggProb * probStarter$csDoubleAggProb +
    probStarter$tripleAggProb * probStarter$csTripleAggProb +
    probStarter$walkAggProb * probStarter$csWalkAggProb +
    probStarter$strikeoutAggProb * probStarter$csStrikeoutAggProb +
    probStarter$otherAggProb * probStarter$csOtherAggProb
  
  ## Note not pulling pitcher info for now - can do that easily & should produce a separate expected outcome
  
  probStarterHit <- probStarter[,c('batter','singleAggProb','doubleAggProb','tripleAggProb','hrAggProb','walkAggProb','strikeoutAggProb','otherAggProb', 'sbPerc', 'csPerc', 'rbiTotal', 'runTotal')]
  probStarterPitch <- probStarter[,c('pitcher','starterOuts','singleAggProb','doubleAggProb','tripleAggProb','hrAggProb','walkAggProb','strikeoutAggProb','otherAggProb', 'sbPerc', 'csPerc', 'erTotal')]
  
  ## Calculate each batters expected outcome vs. the aggregate bullpen
  
  probBullpen <- as.data.frame(cbind(predictionFile[,c('batter', 'pitcher','lineupSpot','type','stand','pitchTeam','batTeam')], probs))
  probBullpen <- filter(probBullpen, type == 'RP')
  probBullpen <- merge(probBullpen, bullpenData[,c('pitcher','stand','cont')], by = c('pitcher', 'stand'))
  probBullpen$singleOutcome <- probBullpen$V1 * probBullpen$cont
  probBullpen$doubleOutcome <- probBullpen$V2 * probBullpen$cont
  probBullpen$tripleOutcome <- probBullpen$V3 * probBullpen$cont
  probBullpen$hrOutcome <- probBullpen$V4 * probBullpen$cont
  probBullpen$walkOutcome <- probBullpen$V5 * probBullpen$cont
  probBullpen$strikeoutOutcome <- probBullpen$V6 * probBullpen$cont
  probBullpen$otherOutcome <- probBullpen$V7 * probBullpen$cont
  
  ## Group by batter
  
  probBullpenG <- group_by(probBullpen, batter)
  probBullpenG <- as.data.frame(summarise(probBullpenG, 
                                          singleOutcome = sum(singleOutcome),
                                          doubleOutcome = sum(doubleOutcome),
                                          tripleOutcome = sum(tripleOutcome),
                                          hrOutcome = sum(hrOutcome),
                                          walkOutcome = sum(walkOutcome),
                                          strikeoutOutcome = sum(strikeoutOutcome),
                                          otherOutcome = sum(otherOutcome)))
  probBullpenG <- merge(probBullpenG, bullpenMult[,c('batter','bullpenMulitplier')], by.x = substr('batter',1 ,6), by.y = 'batter', all.x = T)
  
  ## Multiply aggregate bullpen predictions vs. number of expected bullpen at bats
  
  probBullpenG$singleAggProb <- probBullpenG$singleOutcome * probBullpenG$bullpenMulitplier
  probBullpenG$doubleAggProb <- probBullpenG$doubleOutcome * probBullpenG$bullpenMulitplier
  probBullpenG$tripleAggProb <- probBullpenG$tripleOutcome * probBullpenG$bullpenMulitplier
  probBullpenG$hrAggProb <- probBullpenG$hrOutcome * probBullpenG$bullpenMulitplier
  probBullpenG$walkAggProb <- probBullpenG$walkOutcome * probBullpenG$bullpenMulitplier
  probBullpenG$strikeoutAggProb <- probBullpenG$strikeoutOutcome * probBullpenG$bullpenMulitplier
  probBullpenG$otherAggProb <- probBullpenG$otherOutcome * probBullpenG$bullpenMulitplier
  
  ## Merge with runs data
  ## Note just using batter runs % - bullpen too variable for pitcher to matter
  
  probBullpenG <- merge(probBullpenG, runB, by = 'batter', all.x = T)
  probBullpenG$run_1 <- ifelse(probBullpenG$runsFourPerc == 0, 0.20, probBullpenG$runsOnePerc)
  probBullpenG$run_2 <- ifelse(probBullpenG$runsFourPerc == 0, 0.40, probBullpenG$runsTwoPerc)
  probBullpenG$run_3 <- 0.75
  probBullpenG$run_4 <- 1
  probBullpenG$run_5 <- ifelse(probBullpenG$runsFourPerc == 0, 0.20, probBullpenG$runsFivePerc)
  probBullpenG$run_7 <- ifelse(probBullpenG$runsFourPerc == 0, 0.015, probBullpenG$runsSevenPerc)
  
  ## Create run probabilities
  ## Also need to create earned runs for pitchers by multiplying runs for batters * percentage
  
  probBullpenG$runTotal <- probBullpenG$run_1 * probBullpenG$singleAggProb +
    probBullpenG$run_2 * probBullpenG$doubleAggProb +
    probBullpenG$run_3 * probBullpenG$tripleAggProb +
    probBullpenG$run_4 * probBullpenG$hrAggProb +
    probBullpenG$run_5 * probBullpenG$walkAggProb +
    probBullpenG$run_7 * probBullpenG$otherAggProb
  
  ## Merge with RBIs data
  ## Note just using batter RBIs % - bullpen too variable for pitcher to matter
  
  probBullpenG <- merge(probBullpenG, rbiB, by = 'batter', all.x = T)
  probBullpenG$rbi_1 <- ifelse(probBullpenG$batTotal < 20, 0.2010735, probBullpenG$rbiB_1)
  probBullpenG$rbi_2 <- ifelse(probBullpenG$batTotal < 20, 0.40, probBullpenG$rbiB_2)
  probBullpenG$rbi_3 <- ifelse(probBullpenG$batTotal < 20, 0.5875556, probBullpenG$rbiB_3)
  probBullpenG$rbi_4 <- ifelse(probBullpenG$batTotal < 20, 1.565931, probBullpenG$rbiB_4)
  probBullpenG$rbi_5 <- ifelse(probBullpenG$batTotal < 20, 0.01751584, probBullpenG$rbiB_5)
  probBullpenG$rbi_6 <- 0
  probBullpenG$rbi_7 <- ifelse(probBullpenG$batTotal < 20, 0.02535321, probBullpenG$rbiB_7)
  
  ## Create RBI probabilities
  
  probBullpenG$rbiTotal <- probBullpenG$rbi_1 * probBullpenG$singleAggProb +
    probBullpenG$rbi_2 * probBullpenG$doubleAggProb +
    probBullpenG$rbi_3 * probBullpenG$tripleAggProb +
    probBullpenG$rbi_4 * probBullpenG$hrAggProb +
    probBullpenG$rbi_5 * probBullpenG$walkAggProb +
    probBullpenG$rbi_7 * probBullpenG$otherAggProb
  
  ## Merge with steals data
  ## Note just using batter steal % - bullpen too variable for pitcher to matter
  
  probBullpenG <- merge(probBullpenG, sbB, by = 'batter', all.x = T)
  probBullpenG$singleSB <- ifelse(probBullpenG$sb + probBullpenG$cs > 3, probBullpenG$singleSB, .01)
  probBullpenG$walkSB <- ifelse(probBullpenG$sb + probBullpenG$cs > 3, probBullpenG$walkSB, .01)
  probBullpenG$singleCS <- ifelse(probBullpenG$sb + probBullpenG$cs > 3, probBullpenG$singleCS, .01)
  probBullpenG$walkCS <- ifelse(probBullpenG$sb + probBullpenG$cs > 3, probBullpenG$walkCS, .01)
  
  ## Create steal probabilities
  
  probBullpenG$sbPerc <- probBullpenG$singleSB * probBullpenG$singleAggProb +
    probBullpenG$doubleSB * probBullpenG$doubleAggProb +
    probBullpenG$tripleSB * probBullpenG$tripleAggProb +
    probBullpenG$walkSB * probBullpenG$walkAggProb +
    probBullpenG$strikeoutSB * probBullpenG$strikeoutAggProb +
    probBullpenG$otherSB * probBullpenG$otherAggProb
  
  probBullpenG$csPerc <- probBullpenG$singleCS * probBullpenG$singleAggProb +
    probBullpenG$doubleCS * probBullpenG$doubleAggProb +
    probBullpenG$tripleCS * probBullpenG$tripleAggProb +
    probBullpenG$walkCS * probBullpenG$walkAggProb +
    probBullpenG$strikeoutCS * probBullpenG$strikeoutAggProb +
    probBullpenG$otherCS * probBullpenG$otherAggProb
  
  
  probBullpenG <- probBullpenG[,c('batter','singleAggProb','doubleAggProb','tripleAggProb','hrAggProb','walkAggProb','strikeoutAggProb','otherAggProb', 'sbPerc', 'csPerc', 'rbiTotal','runTotal')]
  
  ## Stack batter outcomes vs starters & bullpen
  
  totalBatterProb <- as.data.frame(rbind(probStarterHit, probBullpenG)) 
  
  ## Create DF to exclude pitchers (pitcher batting stats don't count)
  
  totalBatterEx <- merge(totalBatterProb, pitchLookup, by.x = 'batter', by.y = 'pitcher')
  totalBatterEx <- merge(totalBatterEx, pitchMod, by.x = 'batter', by.y = 'pitcher')
  totalBatterEx <- filter(totalBatterEx, total > 10)
  totalBatterEx$dups <- duplicated(totalBatterEx$batter)
  totalBatterEx <- filter(totalBatterEx, dups == F)
  totalBatterExclude <- unique(totalBatterEx$batter)
  
  totalBatterProb <- filter(totalBatterProb, !batter %in% totalBatterExclude)
  totalBatterG <- group_by(totalBatterProb, batter)
  totalBatterG <- as.data.frame(summarise(totalBatterG,
                                          singleAggProb = sum(singleAggProb),
                                          doubleAggProb = sum(doubleAggProb),
                                          tripleAggProb = sum(tripleAggProb),
                                          hrAggProb = sum(hrAggProb),
                                          walkAggProb = sum(walkAggProb),
                                          strikeoutAggProb = sum(strikeoutAggProb),
                                          otherAggProb = sum(otherAggProb),
                                          sbPerc = sum(sbPerc),
                                          rbiTotal = sum(rbiTotal),
                                          runTotal = sum(runTotal)))
  
  ## Create expected totals by site
  
  totalBatterG$dk <- round((totalBatterG$singleAggProb * 3 +
                              totalBatterG$doubleAggProb * 5 +
                              totalBatterG$tripleAggProb * 8 +
                              totalBatterG$hrAggProb * 10 +
                              totalBatterG$walkAggProb * 2 +
                              totalBatterG$sbPerc * 5 +
                              totalBatterG$rbiTotal * 2 +
                              totalBatterG$runTotal * 2), 2)
  
  totalBatterG$fd <- round((totalBatterG$singleAggProb * 3 +
                              totalBatterG$doubleAggProb * 6 +
                              totalBatterG$tripleAggProb * 9 +
                              totalBatterG$hrAggProb * 12 +
                              totalBatterG$walkAggProb * 3 +
                              totalBatterG$sbPerc * 6 +
                              totalBatterG$rbiTotal * 3.5 +
                              totalBatterG$runTotal * 3.2), 2)
  
  totalBatterG$yahoo <- round((totalBatterG$singleAggProb * 2 +
                                 totalBatterG$doubleAggProb * 4 +
                                 totalBatterG$tripleAggProb * 6 +
                                 totalBatterG$hrAggProb * 8 +
                                 totalBatterG$walkAggProb * 2 +
                                 totalBatterG$sbPerc * 4 +
                                 totalBatterG$rbiTotal * 2 +
                                 totalBatterG$runTotal * 2), 2)
  
  totalBatterG <- totalBatterG[,c('batter','dk','fd','yahoo')]
  
  ## Create dataset housing pitcher information - group all atbats by pitcher
  
  totalPitcherG <- group_by(probStarterPitch, pitcher)
  totalPitcherG <- as.data.frame(summarise(totalPitcherG, 
                                           singleAggProb = sum(singleAggProb),
                                           doubleAggProb = sum(doubleAggProb),
                                           tripleAggProb = sum(tripleAggProb),
                                           hrAggProb = sum(hrAggProb),
                                           walkAggProb = sum(walkAggProb),
                                           strikeoutAggProb = sum(strikeoutAggProb),
                                           otherAggProb = sum(otherAggProb),
                                           outs = (sum(starterOuts)/9),
                                           runs = sum(erTotal)))
  
  totalPitcherG$dk <- ((totalPitcherG$outs / 3) * 2.25 +
                         totalPitcherG$strikeoutAggProb * 2 +
                         (totalPitcherG$singleAggProb + totalPitcherG$doubleAggProb + totalPitcherG$tripleAggProb + totalPitcherG$hrAggProb + totalPitcherG$walkAggProb) * -0.6 +
                         totalPitcherG$runs * -2)
  
  totalPitcherG$fd <- ((totalPitcherG$outs / 3) * 3 +
                         totalPitcherG$strikeoutAggProb * 3 +
                         totalPitcherG$runs * -3)
  
  totalPitcherG$yahoo <- (totalPitcherG$outs * 0.6 +
                            totalPitcherG$strikeoutAggProb * 2 + 
                            totalPitcherG$runs * -2)
  
  totalPitcherG <- totalPitcherG[,c('pitcher', 'dk', 'fd', 'yahoo')]
  
  ## Stack pitcher & batter outcomes into one data frame
  
  colnames(totalPitcherG)[1] <- 'player_code'
  colnames(totalBatterG)[1] <- 'player_code'
  
  totalPlayerOutcome <- as.data.frame(rbind(totalPitcherG, totalBatterG))
  totalPlayerOutcome <- group_by(totalPlayerOutcome, player_code)
  totalPlayerOutcome <- as.data.frame(summarise(totalPlayerOutcome,
                                                dk = sum(dk),
                                                fd = sum(fd),
                                                yahoo = sum(yahoo)))
  colnames(totalPlayerOutcome) <- c('player_code', 'dkExp', 'fdExp', 'yahooExp')
  
  ## Create DF with salary info, positions & lineup spot for each player
  
  salaryInfo <- salaryInfo[,c(3:4,6,7,9)]
  sal <- dcast(salaryInfo, player ~ site, value.var = c('salary'))
  pos <- dcast(salaryInfo, player ~ site, value.var = c('position'))
  colnames(pos) <- c('player', 'dkPos', 'fdPos', 'yahooPos')
  salaryInfo$dups <- duplicated(salaryInfo$player)
  salaryInfo <- filter(salaryInfo, dups == F)
  salaryInfo$dups <- NULL
  sal <- merge(sal, pos, by = 'player')
  sal <- merge(sal, salaryInfo[,c('player','lineupSpot')], by = 'player')
  
  ## Create one total player lookup file

  pL <- pitchLookup[,c('pitcher', 'pitcher_name','team','date')]
  bL <- batLookup[,c('batter','batter_name','team','date')]
  colnames(pL) <- c('player_code', 'player', 'team','date')
  colnames(bL) <- c('player_code', 'player', 'team','date')
  playerLookupTotal <- as.data.frame(rbind(bL, pL))
  
  ## Remove duplicates (comes from pitchers showing up twice) - no data loss
  
  playerLookupTotal$date <- as.Date(playerLookupTotal$date, format = '%Y-%m-%d')
  playerLookupTotal <- playerLookupTotal[rev(order(playerLookupTotal$date)),]
  playerLookupTotal$dups <- duplicated(playerLookupTotal$player_code)
  playerLookupTotal <- filter(playerLookupTotal, dups == F)
  playerLookupTotal$date <- NULL
  playerLookupTotal$dups <- NULL

  ## Merge with salary info to get player names
  
  sal <- merge(sal, playerLookupTotal, by = 'player', all.x = T)
  sal[is.na(sal)] <- 'NoName'
  
  ## Merge Salary info with projected outcome
  
  sal <- merge(sal, totalPlayerOutcome, by = 'player_code', all.x = T)
  colnames(sal)[3:5] <- c('dkSal', 'fdSal', 'yahooSal')
  sal$dkPos <- ifelse(sal$dkPos %in% c('RP','pitcher','P'),'SP',sal$dkPos)
  sal$fdPos <- ifelse(sal$fdPos %in% c('RP','pitcher','P'),'SP',sal$fdPos)
  sal$yahooPos <- ifelse(sal$yahooPos %in% c('RP', 'pitcher','P'),'SP',sal$yahooPos)
  write.csv(sal, paste0('newPreds',date,'.csv'), row.names = F)
  return(sal)
}

## Generate final predictions

finalPreds <- predictionAggs(probNew, totalRoto, bullpenData, newSalary, batterLookup, pitcherLookup, batSteals, pitchSteals,batRBIs, pitchRBIs,batRuns, pitchRuns, '2016-06-13')

scatPlotPreds <- function(dataframe, var1, var2, site, position) {
  bb <- dataframe[grep(position,dataframe[,site]),]
  ggplot(bb, aes_string(x = var1, y = var2, label = 'player')) +
    geom_point() +
    geom_text(aes(label = player)) +
    geom_smooth()
}

scatPlotPreds(finalPreds, 'fdSal', 'fdExp','fdPos', 'OF')
