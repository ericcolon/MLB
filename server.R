finalPreds2 <- finalPreds
library(shiny)
library(plotly)
library(rsconnect)
rsconnect::setAccountInfo(name = 'wesleypasfield', token = '2732D4C57528298E4546CF9DB272F2CF', secret = 'oM9XSnYYw1fOpl5OzpI0uVFDLNAj550v2N+EIeJX')

shinyServer(function(input, output) {
  
  ## Create table to show player stats
  
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
    finalPreds2 <- finalPreds2[!is.na(finalPreds2$Exp),]
    finalPreds2$Exp <- round(finalPreds2$Exp, 2)
    finalPreds2 <- finalPreds2[,c('player','team','lineupSpot','gametime','pos','sal', 'Exp')]
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
    if(input$FantasySite == 'Draft Kings'){
      finalPreds2 <- as.data.frame(rbind_all(list(finalPredsSP, finalPredsSP2, finalPredsC, finalPreds1b, finalPreds2b, finalPredsSS, finalPreds3B, finalPredsOF1, finalPredsOF2,finalPredsOF3)))
      finalPreds2$player <- as.character(finalPreds2$player)
      finalPreds2$team <- as.character(finalPreds2$team)
      finalPreds2$lineupSpot <- as.character(finalPreds2$lineupSpot)
      finalPreds2$pos <- as.character(finalPreds2$pos)
      totalSal <- sum(as.numeric(finalPreds2$sal))
      totalExp <- sum(as.numeric(finalPreds2$Exp))
      summary <- c('Total', '', '', 'Salary Cap = 50','',totalSal,totalExp)
      finalPreds2 <- as.data.frame(rbind(finalPreds2, summary))
    } else if(input$FantasySite == 'FanDuel') {
      finalPreds2 <- as.data.frame(rbind_all(list(finalPredsSP, finalPredsC, finalPreds1b, finalPreds2b, finalPredsSS, finalPreds3B, finalPredsOF1, finalPredsOF2,finalPredsOF3)))
      finalPreds2$player <- as.character(finalPreds2$player)
      finalPreds2$team <- as.character(finalPreds2$team)
      finalPreds2$lineupSpot <- as.character(finalPreds2$lineupSpot)
      finalPreds2$pos <- as.character(finalPreds2$pos)
      totalSal <- sum(as.numeric(finalPreds2$sal))
      totalExp <- sum(as.numeric(finalPreds2$Exp))
      summary <- c('Total', '', '', 'Salary Cap = 35','',totalSal,totalExp)
      finalPreds2 <- as.data.frame(rbind(finalPreds2, summary))
    } else {
      finalPreds2 <- as.data.frame(rbind_all(list(finalPredsSP, finalPredsC, finalPreds1b, finalPreds2b, finalPredsSS, finalPreds3B, finalPredsOF1, finalPredsOF2,finalPredsOF3)))
      finalPreds2$player <- as.character(finalPreds2$player)
      finalPreds2$team <- as.character(finalPreds2$team)
      finalPreds2$lineupSpot <- as.character(finalPreds2$lineupSpot)
      finalPreds2$pos <- as.character(finalPreds2$pos)
      totalSal <- sum(as.numeric(finalPreds2$sal))
      totalExp <- sum(as.numeric(finalPreds2$Exp))
      summary <- c('Total', '', '', 'Salary Cap = 200','',totalSal,totalExp)
      finalPreds2 <- as.data.frame(rbind(finalPreds2, summary))
    }
  return(finalPreds2)
  })
  
  output$chart <- renderPlotly({
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
    finalPreds2$gametime <- paste0(substr(finalPreds2$gametime, 1,2)," ",str_sub(finalPreds2$gametime, start = -5))
    finalPreds2$gametime <- gsub(':','',finalPreds2$gametime)
    if(input$position!='All'){
      finalPreds2 <- finalPreds2[finalPreds2$pos==input$position,]
    }
    fp2 <- filter(finalPreds2, player_code != 'NoName')
    fp2 <- filter(fp2, sal != 0)
    fp2 <- filter(fp2, !is.na(sal))
    fp2 <- fp2[order(fp2$sal),]
    mSh <- loess(fp2$Exp ~ fp2$sal)
    fSh <- with(predict(mSh, seq(min(fp2$sal),max(fp2$sal),(max(fp2$sal) - min(fp2$sal)) / 10), se = T), data.frame(fit, se.fit))
    plot1 <- plot_ly(fp2, x = sal, y = round(Exp, 2), type = 'scatter', color = gametime, colors = 'Set3', name = 'Score vs. Salary', text = player, mode = 'markers') %>%
      add_trace(x = seq(min(fp2$sal),max(fp2$sal),(max(fp2$sal) - min(fp2$sal)) / 10), y = fSh$fit, type = 'scatter', mode = 'lines', name = 'My Score Line', line = list(color = 'blue', width = 3)) %>%
      add_trace(x = seq(min(fp2$sal),max(fp2$sal),(max(fp2$sal) - min(fp2$sal)) / 10), y = fSh$fit + 1.96 * fSh$se.fit, mode = "lines",
                fill = "tonexty", showlegend = F,line = list(color = toRGB("gray", alpha = 0.1),
                                                             fillcolor = toRGB("gray", alpha = 0.1))) %>%
      add_trace(x = seq(min(fp2$sal),max(fp2$sal),(max(fp2$sal) - min(fp2$sal)) / 10), y = fSh$fit - 1.96 * fSh$se.fit, mode = "lines",
                fill = "tonexty", showlegend = F, line = list(color = toRGB("gray", alpha = 0.1),
                                                              fillcolor = toRGB("gray", alpha = 0.1))) %>%
      
      layout(title = 'Salary vs. Expected Output', xaxis = list(title = 'Salary'), yaxis = list(title = 'Score'),autosize = F, width = 1200, height = 800)
  })
  
})
