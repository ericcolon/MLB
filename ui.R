library(shiny)
library(plotly)
library(splitstackshape)
library(dplyr)
library(stringr)

## Create DF that has new days data

finalPreds2 <- read.csv(paste0('finalPreds_',Sys.Date(),'.csv'), stringsAsFactors = F)

## Create User Interface

shinyUI(fluidPage(
  
  ## Title of the page
  
  headerPanel('Daily Fantasy Picks'),
  
  ## Create sidebar that has positions & players to select from each position
  ## Figure out how to make drop downs & font size smaller
  
  sidebarPanel(
    
    ## Attempt at making font smaller, don't think it's working
    
    tags$style(type='text/css', ".selectize-input { font-size: 8px; line-height: 8px;} .selectize-dropdown { font-size: 8px; line-height: 8px; }"),
    
    ## Create inputs users can select from - one for each roster slot
    
    selectInput(inputId = 'FantasySite',
                label = 'Fantasy Baseball Site',
                choices= c('Draft Kings', 'FanDuel', 'Yahoo'),
                selected = 'FanDuel',selectize=F,multiple=F),
    selectInput(inputId = 'position',
                label = 'Select Position for Chart',
                choices=c('All','SP','C','1B','2B','3B','SS','OF'),
                selected = 'All',selectize=F,multiple=F),
    selectInput(inputId = 'pitcher1',
                label = 'Pitcher1',
                choices= as.character(finalPreds2[grep('SP', finalPreds2$dkPos), 2]),
                selected = as.character(finalPreds2[grep('SP', finalPreds2$dkPos)[1], 2]),selectize=F,multiple=F),
    
    ## DraftKings has two pitcher slots
    
    p("Select a 2nd Pitcher for DK"),
    selectInput(inputId = 'pitcher2',
                label = 'Pitcher2',
                choices= as.character(finalPreds2[grep('SP', finalPreds2$dkPos), 2]),
                selected = as.character(finalPreds2[grep('SP', finalPreds2$dkPos)[2], 2]),selectize=F,multiple=F),
    selectInput(inputId = 'catcher',
                label = 'Catcher',
                choices = as.character(finalPreds2[grep('C', finalPreds2$dkPos), 2]),
                selected = as.character(finalPreds2[grep('C', finalPreds2$dkPos)[1], 2]),selectize=F,multiple=F),
    selectInput(inputId = 'firstBase',
                label = 'First Base',
                choices = as.character(finalPreds2[grep('1B', finalPreds2$dkPos), 2]),
                selected = as.character(finalPreds2[grep('1B', finalPreds2$dkPos)[1], 2]),selectize=F,multiple=F),
    selectInput(inputId = 'secondBase',
                label = 'Second Base',
                choices = as.character(finalPreds2[grep('2B', finalPreds2$dkPos), 2]),
                selected = as.character(finalPreds2[grep('2B', finalPreds2$dkPos)[1], 2]),selectize=F,multiple=F),
    selectInput(inputId = 'thirdBase',
                label = 'Third Base',
                choices = as.character(finalPreds2[grep('3B', finalPreds2$dkPos), 2]),
                selected = as.character(finalPreds2[grep('SS', finalPreds2$dkPos)[1], 2]),selectize=F,multiple=F),
    selectInput(inputId = 'shortstop',
                label = 'Shortstop',
                choices = as.character(finalPreds2[grep('SS', finalPreds2$dkPos), 2]),
                selected = as.character(finalPreds2[grep('SS', finalPreds2$dkPos)[1], 2]),selectize=F,multiple=F),
    selectInput(inputId = 'outfield1',
                label = 'Outfield1',
                choices = as.character(finalPreds2[grep('OF', finalPreds2$dkPos), 2]),
                selected = as.character(finalPreds2[grep('OF', finalPreds2$dkPos)[1], 2]),selectize=F,multiple=F),
    selectInput(inputId = 'outfield2',
                label = 'Outfield2',
                choices = as.character(finalPreds2[grep('OF', finalPreds2$dkPos), 2]),
                selected = as.character(finalPreds2[grep('OF', finalPreds2$dkPos)[2], 2]),selectize=F,multiple=F),
    selectInput(inputId = 'outfield3',
                label = 'Outfield3',
                choices = as.character(finalPreds2[grep('OF', finalPreds2$dkPos), 2]),
                selected = as.character(finalPreds2[grep('OF', finalPreds2$dkPos)[3], 2]),selectize=F,multiple=F),
    width = 4
  ),
  
  ## Create Main Panel to display data

  mainPanel(
    
    ## Create two tabs - one for player stats, one for chart
    
    tabsetPanel(type = 'tab',
                
                ## Player Stats tab
                
                tabPanel('Projected Stats', dataTableOutput('playerStats')),
    
                ## Chart tab
    
                tabPanel('Projected Stats Viz', plotlyOutput('chart'))
                         
    )
  )
))

