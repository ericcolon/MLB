setwd('~/Downloads')

library(dplyr)


## Manipulate results file

fdResults <- read.csv(paste0('fanduel entry history ',gsub("-","",Sys.Date()),'.csv'), stringsAsFactors = F)
fdResults$X <- NULL
fdResults <- filter(fdResults, !is.na(Score))
fdResults <- filter(fdResults, !is.na(Opp.Score))
colnames(fdResults) <- c('EntryID','Sport','date','Title','SalaryCap','Score','WinScore','Position','Entries','Opp','Cost','Winning','Link')
fdResults$win <- as.numeric(ifelse(fdResults$Winning > 0, 1, 0))
fdResults$diff <- fdResults$Score - fdResults$WinScore
fdResults <- as.data.frame(cSplit(fdResults, 'Title', sep = '('))
fdResults <- fdResults[,c(1:14,length(fdResults))]
fdResults$Title_3 <- gsub(')', '', fdResults$Title_3)
colnames(fdResults)[length(fdResults)] <- 'TourneyType'
fdResults$un <- rownames(fdResults)
options(scipen=999)
fdResults$date <- as.Date(fdResults$date, format = '%m/%d/%y')
fdResults <- fdResults[order(fdResults$date),]

## Add in date lookup to group by day 1, 2 etc.

dates <- as.data.frame(unique(fdResults$date))
dates$name <- paste0('Day_',rownames(dates))
colnames(dates)[1] <- 'date'
fdResults <- merge(fdResults, dates, by = 'date')

## Create chart showing my score vs. winning score over time

m <- loess(Score ~ as.numeric(date), data = fdResults)
f <- with(predict(m, se = T), data.frame(fit, se.fit))

m2 <- loess(WinScore ~ as.numeric(date), data = fdResults)
f2 <- with(predict(m2, se = T), data.frame(fit, se.fit))


p <- plot_ly(fdResults, x = name, y = Score, type = 'scatter', mode = 'markers', name = 'My Score', markers = list(color = 'blue')) 
p <- add_trace(p, x = name, y = f$fit, mode = 'lines', name = 'My Score Line', line = list(color = 'blue', width = 5))
p <- add_trace(p, x = name, y = f$fit + 1.96 * f$se.fit, mode = "lines",
          fill = "tonexty", line = list(color = toRGB("gray90", alpha = 0.3),
                                        fillcolor = toRGB("gray90", alpha = 0.3)))
p <- add_trace(p, x = name, y = f$fit - 1.96 * f$se.fit, mode = "lines",
               fill = "tonexty", line = list(color = toRGB("gray90", alpha = 0.3),
                                             fillcolor = toRGB("gray90", alpha = 0.3)))
p <- add_trace(fdResults, x = name, y = WinScore, type = 'scatter', mode = 'markers', name = 'Winning Score', marker = list(color = 'red'))
p <- add_trace(p, x = name, y = f2$fit, mode = 'lines', name = 'Winning Score Line', line = list(color = 'red', width = 5))
p <- add_trace(p, x = name, y = f2$fit + 1.96 * f2$se.fit, mode = "lines",
               fill = "tonexty", line = list(color = toRGB("gray90", alpha = 0.3),
                                             fillcolor = toRGB("gray90", alpha = 0.3)))
p <- add_trace(p, x = name, y = f2$fit - 1.96 * f2$se.fit, mode = "lines",
               fill = "tonexty", line = list(color = toRGB("gray90", alpha = 0.3),
                                             fillcolor = toRGB("gray90", alpha = 0.3)))

p

mean(fdResults$Score)
sd(fdResults$WinScore) + mean(fdResults$WinScore)

x <- seq(mean(fdResults$Score) - 3* sd(fdResults$Score), mean(fdResults$Score) + 3 * sd(fdResults$Score), 1)
x2 <- seq(mean(fdResults$WinScore) - 3* sd(fdResults$WinScore), mean(fdResults$WinScore) + 3 * sd(fdResults$WinScore), 1)
y1 = dnorm(x, mean(fdResults$Score), sd(fdResults$Score))
y2 <- dnorm(x2, mean(fdResults$WinScore), sd(fdResults$WinScore))
n <- plot_ly(x = x, y = round(y1, 4), type = 'scatter', name = 'My Score Dist', line = list(color = 'blue', width = 10)) %>%
  add_trace(x = x2, y = round(y2, 4), type = 'scatter', name = 'Winning Score Dist', line = list(color = 'orange', width = 10)) %>%
  add_trace(x = c(mean(fdResults$Score), mean(fdResults$Score)), y = c(0, y1[(length(y1) / 2)]), showlegend = F, line = list(color = 'blue')) %>%
  add_trace(x = c(mean(fdResults$Score) + sd(fdResults$Score), mean(fdResults$Score) + sd(fdResults$Score)), y = c(0, y1[ceiling(length(x) / 2) + ceiling(sd(fdResults$Score))]), showlegend = F, line = list(color = 'blue')) %>%
  add_trace(x = c(mean(fdResults$Score) + (sd(fdResults$Score) * 2), mean(fdResults$Score) + (sd(fdResults$Score) * 2)), y = c(0, y1[ceiling(length(x) / 2) + ceiling(sd(fdResults$Score)*2)]), showlegend = F, line = list(color = 'blue')) %>%
  add_trace(x = c(mean(fdResults$Score) - sd(fdResults$Score), mean(fdResults$Score) - sd(fdResults$Score)), y = c(0, y1[ceiling(length(x) / 2 )- ceiling(sd(fdResults$Score))]), showlegend = F, line = list(color = 'blue')) %>%
  add_trace(x = c(mean(fdResults$Score) - (sd(fdResults$Score) * 2), mean(fdResults$Score) - (sd(fdResults$Score) * 2)), y = c(0, y1[ceiling(length(x) / 2) - ceiling(sd(fdResults$Score)*2)]), showlegend = F, line = list(color = 'blue')) %>%
  add_trace(x = x, y = y1, fill = "tozeroy", showlegend = F, name = 'My Score Dist Fill', line = list(color = 'blue')) %>%
  add_trace(x = c(mean(fdResults$WinScore), mean(fdResults$WinScore)), y = c(0, y2[(length(y2) / 2)]), showlegend = F, line = list(color = 'orange')) %>%
  add_trace(x = c(mean(fdResults$WinScore) + sd(fdResults$WinScore), mean(fdResults$WinScore) + sd(fdResults$WinScore)), y = c(0, y2[ceiling(length(x2) / 2) + ceiling(sd(fdResults$WinScore))]), showlegend = F, line = list(color = 'orange')) %>%
  add_trace(x = c(mean(fdResults$WinScore) + (sd(fdResults$WinScore) * 2), mean(fdResults$WinScore) + (sd(fdResults$WinScore) * 2)), y = c(0, y2[ceiling(length(x2) / 2) + ceiling(sd(fdResults$WinScore)*2)]), showlegend = F, line = list(color = 'orange')) %>%
  add_trace(x = c(mean(fdResults$WinScore) - sd(fdResults$WinScore), mean(fdResults$WinScore) - sd(fdResults$WinScore)), y = c(0, y2[ceiling(length(x2) / 2) - ceiling(sd(fdResults$WinScore))]), showlegend = F, line = list(color = 'orange')) %>%
  add_trace(x = c(mean(fdResults$WinScore) - (sd(fdResults$WinScore) * 2), mean(fdResults$WinScore) - (sd(fdResults$WinScore) * 2)), y = c(0, y2[ceiling(length(x2) / 2) - ceiling(sd(fdResults$WinScore)*2)]), showlegend = F, line = list(color = 'orange')) %>%
  add_trace(x = x2, y = y2, fill = "tozeroy", showlegend = F, name = 'Winning Score Dist Fill', line = list(color = 'orange'))
n

v <- plot_ly(x = x, y = y1, fill = "tonexty")

v
