library(shiny)
library(plotly)

finalPreds2 <- finalPreds

shinyUI(fluidPage(
  headerPanel('Daily Fantasy Picking'),
  sidebarPanel(
    selectInput(inputId = 'FantasySite',
                label = 'Fantasy Baseball Site',
                choices= c('Draft Kings', 'FanDuel', 'Yahoo'),
                selected = 'FanDuel',selectize=F,multiple=F),
    selectInput(inputId = 'position',
                label = 'Select Position for Chart',
                choices=c('All','P','C','1B','2B','3B','SS','OF'),
                selected = 'All',selectize=F,multiple=F),
    selectInput(inputId = 'pitcher1',
                label = 'Pitcher1',
                choices= as.character(finalPreds2[grep('SP', finalPreds2$dkPos), 2]),
                selected = as.character(finalPreds2[grep('SP', finalPreds2$dkPos)[1], 2]),selectize=F,multiple=F),
    p("If playing with Draft Kings, select a second pitcher"),
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
    selectInput(inputId = 'outfield1',
                label = 'Outfield1',
                choices = as.character(finalPreds2[grep('OF', finalPreds2$dkPos), 2]),
                selected = as.character(finalPreds2[grep('OF', finalPreds2$dkPos)[2], 2]),selectize=F,multiple=F),
    selectInput(inputId = 'outfield1',
                label = 'Outfield1',
                choices = as.character(finalPreds2[grep('OF', finalPreds2$dkPos), 2]),
                selected = as.character(finalPreds2[grep('OF', finalPreds2$dkPos)[3], 2]),selectize=F,multiple=F)
  ),
  
  mainPanel(
    
    ## Create two tabs - one for player stats, one for chart
    
    tabsetPanel(type = 'tab',
                
                ## Player Stats tab
                
                tabPanel('Projected Stats', dataTableOutput('playerStats'))),
    
                ## Chart tab
    
                tabPanel('Projected Stats Viz', plotlyOutput('chart'))
    
  )
))

