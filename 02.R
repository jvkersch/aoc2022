library(purrr)
library(dplyr)

# encoding: 0 = rock, 1 = paper, 2 = scissors.
guide <- read.csv("inputs/02.txt", sep = " ", header = FALSE,
                  col.names = c("elf", "me")) %>% map_df(~ as.numeric(factor(.x)) - 1)

payoff <- (guide$me - guide$elf + 1) %% 3 # 0: draw, 1: win, 2: loss
score <- 3*payoff + guide$me + 1
print(sum(score))

strategy <- (guide$elf + guide$me - 1) %% 3
score <- 3*guide$me + strategy + 1
print(sum(score))