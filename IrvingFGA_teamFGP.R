setwd("~/Desktop/celtics/")
library(ggplot2)
library(readxl)
Irving <- read_xlsx("./Irving.xlsx", sheet = 1)

Morris <- read_xlsx("./Morris.xlsx", sheet = 1)
Theis <- read_xlsx("./Theis.xlsx", sheet = 1)
Baynes <- read_xlsx("./Baynes.xlsx", sheet = 1)
Smart <- read_xlsx("./Smart.xlsx", sheet = 1)
Hayward <- read_xlsx("./Hayward.xlsx", sheet = 1)
Brown <- read_xlsx("./Brown.xlsx", sheet = 1)
Al <- read_xlsx("./Al.xlsx", sheet = 1)
Rozier <- read_xlsx("./Rozzier.xlsx", sheet = 1)
Tatum <- read_xlsx("./Tatum.xlsx", sheet = 1)

Irving$player <- "Irving"
Theis$player <- "Theis"
Baynes$player <- "Baynes"
Smart$player <- "Smart"
Hayward$player <- "Hayward"
Brown$player <- "Brown"
Al$player <- "Al"
Rozier$player <- "Rozier"
Tatum$player <- "Tatum"
Morris$player <- "Morris"


Celtics.All <- rbind(Irving, Morris, Theis, Baynes, Smart, Hayward, Brown, Al, Rozier, Tatum)
Celtics.All$Rk <- as.factor(Celtics.All$Rk)

library(dplyr)


irving_fa <- rep(NA, 82) # Kyrie Irving field goal attempt
other_fg <- rep(NA, 82) # Other players' field goal percentage

for(i in 1:82){
  game_log <- Celtics.All %>% select(Rk, player, `FG%`, FGA, FG)
  
  irving_fa[i] <- as.numeric(game_log %>% filter(player == "Irving", Rk == i) %>% select(FGA))
  others_gameLog <- game_log %>% filter(player != "Irving", Rk == i) %>% select(FG, FGA)
  other_fg[i] <- sum(others_gameLog$FG, na.rm = T)/sum(others_gameLog$FGA, na.rm = T)
  
}


#### In Game Win #############
for (i in 1:nrow(Celtics.All)){
  Celtics.All[i,"..8"] <- substr(Celtics.All[i,"..8"], start = 1, stop = 1)
}

Celtics.All$WL <- as.factor(Celtics.All$`..8` )

Celtics.All.win <- Celtics.All %>% filter(WL == "W") # Select the subset of games win
nWin <- length(unique(Celtics.All.win$Rk))
winRk <- as.integer(unique(Celtics.All.win$Rk))
irving_fa.w <- rep(NA, nWin) # Kyrie Irving field goal attempt
other_fg.w <- rep(NA, nWin) # Other players' field goal percentage

k <- 1
for(i in winRk){
  cat(i, " ")
  irving_fa.w[k] <- as.numeric(Celtics.All.win %>% filter(player == "Irving", Rk == i) %>% select(FGA))
  others_gameLog <- Celtics.All.win %>% filter(player != "Irving", Rk == i) %>% select(FG, FGA)
  other_fg.w[k] <- sum(others_gameLog$FG, na.rm = T)/sum(others_gameLog$FGA, na.rm = T)
  k <- k+1
}

perm.cor.test(other_fg.w, irving_fa.w, num.sim = 20000, method = "spearman")

summary(lm(irving_fa.w~other_fg.w))
IrvingFA.teamFG.win <- data.frame(irving = irving_fa.w, teammate = other_fg.w)
ggplot(IrvingFA.teamFG.win, aes(y = irving_fa.w, x = other_fg.w)) + geom_point(size = I(3)) + geom_smooth(method = "lm") +
  ggtitle("Irving's FGA vs Teammates's FG% (Win)")+ylab("Irving's Field Goal Attempt")+xlab("Teammates's Field Goal Percentage") +
  theme(title = element_text(size = 16, face = "bold"),axis.text.x = element_text(size = 10))




#### In Game Lose #############

Celtics.All.lose <- Celtics.All %>% filter(WL == "L") # Select the subset of games win
nLose <- length(unique(Celtics.All.lose$Rk))
LoseRk <- as.integer(unique(Celtics.All.lose$Rk))
irving_fa.l <- rep(NA, nLose) # Kyrie Irving field goal attempt
other_fg.l<- rep(NA, nLose) # Other players' field goal percentage

k <- 1
for(i in LoseRk){
  cat(i, " ")
  irving_fa.l[k] <- as.numeric(Celtics.All.lose %>% filter(player == "Irving", Rk == i) %>% select(FGA))
  others_gameLog <- Celtics.All.lose %>% filter(player != "Irving", Rk == i) %>% select(FG, FGA)
  other_fg.l[k] <- sum(others_gameLog$FG, na.rm = T)/sum(others_gameLog$FGA, na.rm = T)
  k <- k+1
}

perm.cor.test(other_fg.l, irving_fa.l, num.sim = 20000, method = "spearman")
IrvingFA.teamFG.Lose <- data.frame(irving = irving_fa.l, teammate = other_fg.l)
ggplot(IrvingFA.teamFG.Lose, aes(y = irving_fa.l, x = other_fg.l)) + geom_point(size = I(3)) + geom_smooth(method = "lm") +
  ggtitle("Irving's FGA vs Teammates's FG% (Lose)")+ylab("Irving's Field Goal Attempt")+xlab("Teammates's Field Goal Percentage") +
  theme(title = element_text(size = 16, face = "bold"),axis.text.x = element_text(size = 10))

