library(tidyverse)
library(rvest)

Warren <- read_html("https://www.basketball-reference.com/players/w/warretj01/gamelog/2020") %>% 
  html_table()

All_Players <- read_html("https://www.basketball-reference.com/leagues/NBA_2020_per_game.html") %>% html_table()

Players <- data.frame(All_Players[[1]]) 
Players <- Players %>% select(Player, TRB, AST, PTS)
for(i in 2:4){
  Players[ ,i] <- as.numeric(Players[ ,i])
}
summary(Players)

Games2021 <- Warren[[8]] %>% data.frame() %>% filter(Rk != "Rk")
for(i in 11:29) {
  Games2021[,i] <- as.numeric(Games2021[,i])
}

Pre_Bubble <- Games2021[1:68,]
Bubble <- Games2021[66:71, ]

Home <- Pre_Bubble %>% filter(Var.6 == "@")
Away <- Pre_Bubble %>% filter(Var.6 == "")

Florida <- Pre_Bubble %>% filter(Opp %in% c("ORL", "MIA"))

zscore <- function(x, col, y){
  y_bar <- mean(x[,col], na.rm = TRUE)
  sd <- sd(x[,col], na.rm = TRUE)
  z <- (y - y_bar) / sd
  print(z)
}

zscore(Pre_Bubble, 28, 31)
zscore(Florida, 28, 31)

#3rd for SF in TS% in 2019-20

Type <- c("Pre-Bubble", "Away Games", "Florida", "Bubble")

Points <- c(mean(Pre_Bubble$PTS, na.rm = TRUE),
            mean(Away$PTS, na.rm = TRUE),
            mean(Florida$PTS, na.rm = TRUE), 
            mean(Bubble$PTS, na.rm = TRUE)
            )

Rebounds <- c(mean(Pre_Bubble$TRB, na.rm = TRUE), 
              mean(Away$TRB, na.rm = TRUE),
              mean(Florida$TRB, na.rm = TRUE), 
              mean(Bubble$TRB, na.rm = TRUE)
              )

Assists <- c(mean(Pre_Bubble$AST, na.rm = TRUE), 
             mean(Away$AST, na.rm = TRUE),
             mean(Florida$AST, na.rm = TRUE), 
             mean(Bubble$AST, na.rm = TRUE)
             )
GS <- c(mean(Pre_Bubble$GmSc, na.rm = TRUE),
        mean(Away$GmSc, na.rm = TRUE), 
        mean(Florida$GmSc, na.rm = TRUE), 
        mean(Bubble$GmSc, na.rm = TRUE)
        )

N <- c(68,35,5,6)

Stats_Table <- data.frame(Type, Points, Rebounds, Assists, GS, N)

Stat <- c("PTS", "REB", "AST")
BPts <- c(31, 6.33, 2)
Percentiles <- c("100%", "90%", "73%")
Percent_Table <- data.frame(Stat, BPts, Percentiles)

Percentile <- function(name, col){
  P <- Players %>% 
    filter(P == name)
  P <- P[,col]
  print(which(Players[,col] < P))
}

Percentile("LeBron James", 30)

