####### ROTOGRINDERS SALARY DATA PULL ###########

## Insert date in YYYY-MM-DD form & location you'd like to save file in quotes
## ex - salaryData('2016-04-05', '~/Documents/Northwestern/498/MLB Scraped')

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

    grinders <- urlList[[c]] %>%
                html_nodes('.players .player-popup , .mascot, .meta, .stats .salary, .position, .pname .player-popup') %>%
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
    
    grinders <- as.data.frame(matrix(grinders, 
                                     nrow = (length(grinders)/60), 
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
      for (i in seq(5, 29, 3)) {
        l <- l + 1
        n <- i + 1
        j <- i + 2
        d <- df[,c(1,2,i,n,j)]
        d$lineupSpot <- l
        colnames(d) <- c('team', 'opponent', 'player', 'position', 'attributes_2','lineupSpot')
        playerStack[[l]] <- d
      }
      s <- 0
      for (i in seq(34, 58, 3)) {
        s <- s + 1
        l <- l + 1
        n <- i + 1
        j <- i + 2
        d <- df[,c(2,1,i,n,j)]
        d$lineupSpot <- s
        colnames(d) <- c('team', 'opponent', 'player', 'position', 'attributes_2','lineupSpot')

        playerStack[[l]] <- d
      }
      
      ## Note - pitchers handled separately here because of additional extraneous data that must be removed
      
      l <- l + 1
      roadPitch <- grinders[,c(1:4)]
      homePitch <- grinders[,c(2,1,32,33)]
      colnames(roadPitch) <- c('team','opponent','player','attributes')
      colnames(homePitch) <- c('team','opponent','player','attributes')
      roadPitch <- roadPitch[!roadPitch$opponent %in% nlList,]
      homePitch <- homePitch[!homePitch$team %in% nlList,]
      totalPitch <- as.data.frame(rbind(roadPitch, homePitch), stringsAsFactors = F)
      totalPitch <- as.data.frame(cSplit(totalPitch, 
                                         'attributes', 
                                         sep='_'), stringsAsFactors = F)
      totalPitch <- totalPitch[,c(1:3,5)]
      totalPitch$position <- 'pitcher'
      totalPitch$lineupSpot <- 9
      totalPitch <- totalPitch[,c(1:3,5,4,6)]
      totalPitch[,c(1:3)] <- sapply(totalPitch[,c(1:3)], as.character)
      
      ## Stack pitchers onto existing postional players file, then convert to data frame
      
      playerStack[[l]] <- totalPitch
      playerDF <- do.call(rbind.data.frame, playerStack)
      playerDF$attributes_2 <- gsub('K', 
                                    '', 
                                    playerDF$attributes_2)
      playerDF$attributes_2 <- as.numeric(as.character(playerDF$attributes_2))
      playerDF <- playerDF[order(playerDF$team,
                                 playerDF$opponent,
                               playerDF$lineupSpot),]
      
      ## Change column name to salary, and add in variable that indicates the site the salaries come from
      
      colnames(playerDF)[5] <- 'salary'
      playerDF$site <- sites[c]
      return(playerDF)
    }
    
    ## Run this function for each site, then stack on top of each other
    
    grindersList <- stacker(grinders)
    salaryList[[c]] <- grindersList
  }
  
  ## Final output is a dataframe that has the salaries for each site stacked on top of one and other
  
  grindersDF <- do.call(rbind.data.frame, salaryList) 
  setwd(location)
  write.csv(grindersDF, paste0(day,"_DailyFantasy.csv"), row.names = F)
}

## Run function to save file at desired location
  
salaryData('2016-04-05', '~/Documents/Northwestern/498/MLB Scraper')

  
  

  