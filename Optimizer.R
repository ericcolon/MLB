## Optimization

## Create optimization data frame for FanDuel

finalPredsOpt <- filter(finalPreds, substr(gametime, 1, 1) >= 7)
finalPredsOpt <- finalPredsOpt[,c('player','fdSal','fdPos','fdExp')]
finalPredsOpt <- filter(finalPredsOpt, player != 'Lonnie Chisenhall')
finalPredsOpt <- filter(finalPredsOpt, fdExp > 0)
finalPredsMatrix <- rbind(as.numeric(finalPredsOpt$fdPos == 'C'),
                          as.numeric(finalPredsOpt$fdPos == '1B'),
                          as.numeric(finalPredsOpt$fdPos == '2B'),
                          as.numeric(finalPredsOpt$fdPos == 'SS'),
                          as.numeric(finalPredsOpt$fdPos == '3B'),
                          as.numeric(finalPredsOpt$fdPos == 'OF'),
                          as.numeric(finalPredsOpt$fdPos == 'SP'),
                          finalPredsOpt$fdSal)

## Objective Function

obj.func <- finalPredsOpt$fdExp
num.players <- nrow(finalPredsOpt)
var.types <- rep("B", num.players)
direction <- c('==','==','==','==','==','==','==','<=')
rhs <- c(1,1,1,1,1,3,1,35)

linOpt <- Rglpk_solve_LP(obj = obj.func, 
                         mat = finalPredsMatrix, 
                         dir = direction,
                         rhs = rhs,
                         types = var.types,
                         max = T)

finalPredsOpt$selector <- linOpt$solution
finalRoster <- filter(finalPredsOpt, selector == 1)
finalRoster[order(finalRoster$fdPos),]
sum(finalRoster$fdSal)


