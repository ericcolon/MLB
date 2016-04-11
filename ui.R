
require('devtools')
require('shinyapps')
require(rCharts)

shinyUI(pageWithSidebar(
  headerPanel('Team Maryland Fantasy Baseball Lineup Optimizer'),
  sidebarPanel(
    radioButtons('optimizer','Optimize Lineup',c('yes','no'),inline=TRUE),
    selectInput(inputId = 'optimal',
                label = 'Optimal Lineup',
                choices = c('yes','no'),
                selected = ''),
    selectInput(inputId = 'pitcher',
                label = 'Pitcher',
                choices = c('','Chris Sale','Zach Greinke','Clayton Kershaw'),
                selected = 'Chris Sale',selectize=F,multiple=F),
    selectInput(inputId = 'catcher',
                label = 'Catcher',
                choices = c('','Buster Posey','Salvi Perez','Yadier Molina'),
                selected = 'Buster Posey',selectize=F,multiple=F),
    selectInput(inputId = 'firstBase',
                label = 'First Base',
                choices = c('','Jose Abreu','Miguel Cabrera','Prince Fielder'),
                selected = 'Jose Abreu',selectize=F,multiple=F),
    selectInput(inputId = 'secondBase',
                label = 'Second Base',
                choices = c('','Dee Gordon','Jose Altuve','Jason Kipnis'),
                selected = 'Dee Gordon',selectize=F,multiple=F),
    selectInput(inputId = 'thirdBase',
                label = 'Third Base',
                choices = c('','Yunel Escobar','Josh Donaldson','Matthew Duffy'),
                selected = 'Yunel Escobar',selectize=F,multiple=F),
    selectInput(inputId = 'shortstop',
                label = 'Shortstop',
                choices = c('','Xander Bogaerts','Tror Tulowitzki','Jhonny Peralta'),
                selected = 'Xander Bogaerts',selectize=F,multiple=F),
    selectInput(inputId = 'leftField',
                label = 'Left Field',
                choices = c('','David Peralta','Michael Brantley','Yoenis Cespedes'),
                selected = 'David Peralta',selectize=F,multiple=F),
    selectInput(inputId = 'centerField',
                label = 'Center Field',
                choices = c('','A.J. Pollock','Lorenzo Cain','Mike Trout'),
                selected = 'Mike Trout',selectize=F,multiple=F),
    selectInput(inputId = 'rightField',
                label = 'Right Field',
                choices = c('','Bryce Harper','Ender Inciarte','Nelson Cruz'),
                selected = 'Bryce Harper',selectize=F,multiple=F),
    selectInput(inputId = 'designatedHitter',
                label = 'Designated Hitter',
                choices = c('','Kendrys Morales','David Ortiz','Billy Butler'),
                selected = 'Kendrys Morales',selectize=F,multiple=F)

  ),
  
  mainPanel(
    tabsetPanel(type="tab",
                

                  tabPanel("Player Stats",dataTableOutput("player.stats"))
                #tabPanel("Top Crimes in Next Four Hours",tableOutput("freq.data")

      )
    )
))