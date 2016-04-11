library(shiny)
require(devtools)
library(Lahman)
library(rvest)
library(splitstackshape)
library(repmis)


file <- '_DailyFantasy.v1.csv'
mykey <- '62sw9gjrgwv5yfk'
grindersDF<- source_DropboxData(file,key=mykey, sep=",", header=TRUE)

master <- Master[,c('nameFirst','nameLast','bats','throws','birthYear','weight','height')]
master$player <- paste(master$nameFirst,master$nameLast,sep=' ')
master <- master[,c(8,3:7)]
master$age <- 2016-master$birthYear
master <- master[,-4]
grindersDF <- merge(grindersDF,master,by='player',all.x=TRUE,all.y=FALSE)
  
  ## Run function to save file at desired location
  #grindersDF <- salaryData('2016-04-05', 'C:/Baseball/')
  #grindersDF <- salaryData('2016-04-08')



shinyServer(function(input, output) {
  output$player.stats <- renderDataTable({

    grindersDF <- grindersDF[grindersDF$player == input$pitcher | grindersDF$player == input$catcher | 
                       grindersDF$player == input$firstBase | grindersDF$player == input$secondBase | 
                      grindersDF$player == input$thirdBase | grindersDF$player == input$shortstop |
                       grindersDF$player == input$leftField | grindersDF$player == input$centerField | 
                      grindersDF$player == input$rightField | grindersDF$player == input$designatedHitter,]
    grindersDF$pred.score <- 1 
    grindersDF <- grindersDF[,c(13,1:12)]
    grindersDF
    })
  })
 

  
