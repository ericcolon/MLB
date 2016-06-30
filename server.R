library(shiny)
library(plotly)
library(splitstackshape)
library(dplyr)
library(stringr)
library(rsconnect)
rsconnect::setAccountInfo(name = 'wesleypasfield', 
                          token = '2732D4C57528298E4546CF9DB272F2CF', 
                          secret = 'oM9XSnYYw1fOpl5OzpI0uVFDLNAj550v2N+EIeJX')

## For Deployment

finalPreds2 <- read.csv(paste0('finalPreds_',Sys.Date(),'.csv'), stringsAsFactors = F)

## To do - add comments
##       - add optimizer - linear algebra
##       - Increase marker size on chart
##       - Make Color scheme easier to see (yellow)

shinyServer(function(input, output) {
  
  ## Create table to show player stats
  
  ## If statement to show only selected site information
  
  output$playerStats <- renderDataTable({
    if(input$FantasySite=='Draft Kings'){
      finalPreds2$pos <- finalPreds2$dkPos
      finalPreds2$sal <- finalPreds2$dkSal
      finalPreds2$Exp <- finalPreds2$dkExp
    }else if(input$FantasySite=='FanDuel'){
      finalPreds2$pos <- finalPreds2$fdPos
      finalPreds2$sal <- finalPreds2$fdSal
      finalPreds2$Exp <- finalPreds2$fdExp
    }else{
      finalPreds2$pos <- finalPreds2$yahooPos
      finalPreds2$sal <- finalPreds2$yahooSal
      finalPreds2$Exp <- finalPreds2$yahooExp
    }
    
    ## Drop records with no data, and round predictions for visualization, then reduce dataset to essential viz vars
    
    finalPreds2 <- finalPreds2[!is.na(finalPreds2$Exp),]
    finalPreds2$Exp <- round(finalPreds2$Exp, 2)
    finalPreds2 <- finalPreds2[,c('player','team','lineupSpot','gametime','pos','sal', 'Exp')]
    
    ## Show just players selected on inputs
    
    finalPredsSP <- finalPreds2[finalPreds2$player == input$pitcher1,]
    finalPredsSP2 <- finalPreds2[finalPreds2$player == input$pitcher2,]
    finalPredsC <- finalPreds2[finalPreds2$player == input$catcher,]
    finalPreds1b <- finalPreds2[finalPreds2$player == input$firstBase,]
    finalPreds2b <- finalPreds2[finalPreds2$player == input$secondBase,]
    finalPredsSS <- finalPreds2[finalPreds2$player == input$shortstop,]
    finalPreds3B <- finalPreds2[finalPreds2$player == input$thirdBase,]
    finalPredsOF1 <- finalPreds2[finalPreds2$player == input$outfield1,]
    finalPredsOF2 <- finalPreds2[finalPreds2$player == input$outfield2,]
    finalPredsOF3 <- finalPreds2[finalPreds2$player == input$outfield3,]
    
    ## Customize per sites - salary caps & rosters slightly different
    
    ## Draft Kings
    
    if(input$FantasySite == 'Draft Kings'){
      finalPreds2 <- as.data.frame(rbind_all(list(finalPredsSP, finalPredsSP2, finalPredsC, finalPreds1b, finalPreds2b, finalPredsSS, finalPreds3B, finalPredsOF1, finalPredsOF2,finalPredsOF3)))
      finalPreds2$player <- as.character(finalPreds2$player)
      finalPreds2$team <- as.character(finalPreds2$team)
      finalPreds2$lineupSpot <- as.character(finalPreds2$lineupSpot)
      finalPreds2$pos <- as.character(finalPreds2$pos)
      
      ## Add up sum of salaries & expected output for current lineup
      
      totalSal <- sum(as.numeric(finalPreds2$sal))
      totalExp <- sum(as.numeric(finalPreds2$Exp))
      
      ## Sum up totals and display as last row in the table
      
      summary <- c('Total', '', '', 'Salary Cap = 50','',totalSal,totalExp)
      finalPreds2 <- as.data.frame(rbind(finalPreds2, summary))
    
    ## Fanduel    
      
    } else if(input$FantasySite == 'FanDuel') {
      finalPreds2 <- as.data.frame(rbind_all(list(finalPredsSP, finalPredsC, finalPreds1b, finalPreds2b, finalPredsSS, finalPreds3B, finalPredsOF1, finalPredsOF2,finalPredsOF3)))
      finalPreds2$player <- as.character(finalPreds2$player)
      finalPreds2$team <- as.character(finalPreds2$team)
      finalPreds2$lineupSpot <- as.character(finalPreds2$lineupSpot)
      finalPreds2$pos <- as.character(finalPreds2$pos)
      
      ## Add up sum of salaries & expected output for current lineup
      
      totalSal <- sum(as.numeric(finalPreds2$sal))
      totalExp <- sum(as.numeric(finalPreds2$Exp))
      
      ## Sum up totals and display as last row in the table
      
      summary <- c('Total', '', '', 'Salary Cap = 35','',totalSal,totalExp)
      finalPreds2 <- as.data.frame(rbind(finalPreds2, summary))
      
    ## Yahoo
        
    } else {
      finalPreds2 <- as.data.frame(rbind_all(list(finalPredsSP, finalPredsC, finalPreds1b, finalPreds2b, finalPredsSS, finalPreds3B, finalPredsOF1, finalPredsOF2,finalPredsOF3)))
      finalPreds2$player <- as.character(finalPreds2$player)
      finalPreds2$team <- as.character(finalPreds2$team)
      finalPreds2$lineupSpot <- as.character(finalPreds2$lineupSpot)
      finalPreds2$pos <- as.character(finalPreds2$pos)
      
      ## Add up sum of salaries & expected output for current lineup
      
      totalSal <- sum(as.numeric(finalPreds2$sal))
      totalExp <- sum(as.numeric(finalPreds2$Exp))
      
      ## Sum up totals and display as last row in the table
      
      summary <- c('Total', '', '', 'Salary Cap = 200','',totalSal,totalExp)
      finalPreds2 <- as.data.frame(rbind(finalPreds2, summary))
    }
  return(finalPreds2)
  })
  
  ## Create chart that vizualizes salary vs. expected outcome
  
  output$chart <- renderPlotly({
    
    ## Pull out salaries, positions & expected values for each site
    ## Add in a column that concatenates player & position (to deal with duplicates)
    
    if(input$FantasySite=='Draft Kings'){
      finalPreds2$Exp <- finalPreds2$dkExp
      finalPreds2$pos <- finalPreds2$dkPos
      finalPreds2$sal <- finalPreds2$dkSal
      finalPreds2$player_position <- paste(finalPreds2$player,finalPreds2$dkPos)
    }else if(input$FantasySite=='FanDuel'){
      finalPreds2$Exp <- finalPreds2$fdExp
      finalPreds2$pos <- finalPreds2$fdPos
      finalPreds2$sal <- finalPreds2$fdSal
      finalPreds2$player_position <- paste(finalPreds2$player,finalPreds2$fdPos)
    }else{
      finalPreds2$Exp <- finalPreds2$yahooExp
      finalPreds2$pos <- finalPreds2$yahooPos
      finalPreds2$sal <- finalPreds2$yahooSal
      finalPreds2$player_position <- paste(finalPreds2$player,finalPreds2$yahooPos)
    }
    
    ## Clean up gametime for filtering on chart
    
    finalPreds2$gametime <- paste0(substr(finalPreds2$gametime, 1,2)," ",str_sub(finalPreds2$gametime, start = -5))
    finalPreds2$gametime <- gsub(':','',finalPreds2$gametime)
    finalPreds2 <- finalPreds2[order(finalPreds2$gametime),]
    
    ## Display just position selected - unless selection is all
    
    if(input$position!='All'){
      finalPreds2 <- finalPreds2[finalPreds2$pos==input$position,]
    }
    
    ## Drop missing players & players with no salary, then order by salary
    
    fp2 <- filter(finalPreds2, player_code != 'NoName')
    fp2 <- filter(fp2, sal != 0)
    fp2 <- filter(fp2, !is.na(sal))
    fp2 <- fp2[order(fp2$sal),]
    
    ## Create loess regression line to use in conjuction with visualization
    
    mSh <- loess(fp2$Exp ~ fp2$sal)
    fSh <- with(predict(mSh, seq(min(fp2$sal),max(fp2$sal),(max(fp2$sal) - min(fp2$sal)) / 10), se = T), data.frame(fit, se.fit))
    
    ## Create plot that shows players selected in input with confidence interval overlaying loess line
    
    plot1 <- plot_ly(fp2, x = sal, y = round(Exp, 2), type = 'scatter', color = gametime, colors = 'Set3', name = 'Score vs. Salary', text = player, mode = 'markers') %>%
      add_trace(x = seq(min(fp2$sal),max(fp2$sal),(max(fp2$sal) - min(fp2$sal)) / 10), y = fSh$fit, type = 'scatter', mode = 'lines', name = 'My Score Line', line = list(color = 'blue', width = 3)) %>%
      add_trace(x = seq(min(fp2$sal),max(fp2$sal),(max(fp2$sal) - min(fp2$sal)) / 10), y = fSh$fit + 1.96 * fSh$se.fit, mode = "lines",
                fill = "tonexty", showlegend = F,line = list(color = toRGB("gray", alpha = 0.1),
                                                             fillcolor = toRGB("gray", alpha = 0.1))) %>%
      add_trace(x = seq(min(fp2$sal),max(fp2$sal),(max(fp2$sal) - min(fp2$sal)) / 10), y = fSh$fit - 1.96 * fSh$se.fit, mode = "lines",
                fill = "tonexty", showlegend = F, line = list(color = toRGB("gray", alpha = 0.1),
                                                              fillcolor = toRGB("gray", alpha = 0.1))) %>%
      
      layout(title = 'Salary vs. Expected Output', xaxis = list(title = 'Salary'), yaxis = list(title = 'Score'),autosize = F, width = 1000, height = 800)
  })
  
})