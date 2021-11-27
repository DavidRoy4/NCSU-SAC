library(tidyverse)
library(rvest)

Warren <- read_html("https://www.basketball-reference.com/players/w/warretj01/gamelog/2020") %>% 
  html_table()

All_Players <- read_html("https://www.basketball-reference.com/leagues/NBA_2020_per_game.html") %>% html_table()

Total_Players <- data.frame(All_Players[[1]]) 
Players <- Total_Players %>% select(Player, G, TRB, AST, PTS)
for(i in 2:5){
  Players[ ,i] <- as.numeric(Players[ ,i])
}

Salaries <- read.csv("NBA\\Salaries.csv") %>% select(Player, Salary_2019_to_20) %>% filter(Player %in% c(Players$Player))

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

Player_filter <- Players %>% filter(G > 20)
Percentile <- function(name,col){
  P <- Player_filter %>% filter(Player == name) %>% select(col) %>% as.numeric()
  col_num <- which(colnames(Players)==col)
  Percen <- length(which(Players[,col_num] > P)) / length(Players[,col_num])
  final <- c(P, 1-Percen)
  print(final)
}

Names <- c("TJ Warren", "Paul George", "Kawhi Leonard", "Jimmy Butler", "Khris Middleton")

PTS <- c(31, 
         Percentile("Paul George", "PTS")[1],
         Percentile("Kawhi Leonard", "PTS")[1],
         Percentile("Jimmy Butler", "PTS")[1], 
         Percentile("Khris Middleton", "PTS")[1])

PTS_per <- c("100%", 
             paste0(round(Percentile("Paul George", "PTS")[2],2) * 100, "%"), 
             paste0(round(Percentile("Kawhi Leonard", "PTS")[2],2) * 100, "%"),
             paste0(round(Percentile("Jimmy Butler", "PTS")[2], 2) * 100, "%"), 
             paste0(round(Percentile("Khris Middleton", "PTS")[2], 2) * 100, "%"))

AST <- c(2, 
         Percentile("Paul George", "AST")[1],
         Percentile("Kawhi Leonard", "AST")[1],
         Percentile("Jimmy Butler", "AST")[1], 
         Percentile("Khris Middleton", "AST")[1])

AST_per <- c("72%", 
             paste0(round(Percentile("Paul George", "AST")[2], 2) * 100, "%"), 
             paste0(round(Percentile("Kawhi Leonard", "AST")[2], 2) * 100, "%"),
             paste0(round(Percentile("Jimmy Butler", "AST")[2], 2) * 100, "%"), 
             paste0(round(Percentile("Khris Middleton", "AST")[2], 2) * 100, "%"))

REB <- c(6.33, 
         Percentile("Paul George", "TRB")[1],
         Percentile("Kawhi Leonard", "TRB")[1],
         Percentile("Jimmy Butler", "TRB")[1], 
         Percentile("Khris Middleton", "TRB")[1])

REB_per <- c("90%", 
             paste0(round(Percentile("Paul George", "TRB")[2],2) * 100, "%"), 
             paste0(round(Percentile("Kawhi Leonard", "TRB")[2],2) * 100, "%"),
             paste0(round(Percentile("Jimmy Butler", "TRB")[2],2) * 100, "%"), 
             paste0(round(Percentile("Khris Middleton", "TRB")[2], 2) * 100, "%"))

Percent_Table <- data.frame(Names, PTS, PTS_per, AST, AST_per, REB, REB_per)

TJ <- c("TJ Warren", 67, 6.33, 2, 31, 10810000)

Middleton <- Players %>% filter(Player == "Khris Middleton")

final_data <- Players %>% filter(Player %in% c("Paul George", "Kawhi Leonard", "Jimmy Butler")) 
Money <- Salaries %>% filter(Player %in% c("Paul George", "Kawhi Leonard", "Jimmy Butler"))

Middleton <- Middleton %>% mutate(Salary_2019_to_20 = Salaries[24,2])
final_data <- full_join(final_data, Money) %>% 
  rbind(TJ) %>% 
  rbind(Middleton)

final_data <- final_data %>% 
  mutate(Salary_Per = c("98.2%", "98.4%", "98.2%", "77.5%", "95.1%"), 
         Salary_num = c(98.2, 98.4, 98.2, 77.5, 95.1), 
         PTS_per = Percent_Table$PTS_per)

ggplot(final_data, aes(x = reorder(Player, desc(PTS)), y = PTS)) + 
  geom_segment(aes(x = 0, y = PTS, xend = Player, yend = PTS))
  geom_point(aes(size = Salary_Per)) +
  geom_text(aes(label = Salary_num), vjust = 1.5)
