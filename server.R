finalPreds2 <- finalPreds

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
    if(input$position!='All'){
      finalPreds2 <- finalPreds2[finalPreds2$pos==input$position,]
    }
    p <- plot_ly(data = finalPreds2, x = sal, y = Exp, model = markers,
                 color = gametime, colors = 'Set3') +
      p %>% add_trace(y = fitted(loess(Exp ~ sal)), x = sal)
    return (p)
  })
  
})