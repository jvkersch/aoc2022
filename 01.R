library(purrr)

food <- read.csv("inputs/01.txt", blank.lines.skip = FALSE, header = FALSE)$V1
groups <- cumsum(is.na(food))
elves_with_food <- map_int(split(food, groups), ~ sum(.x, na.rm = TRUE))
cat(max(elves_with_food), "\n")

partial <- sort(elves_with_food, partial = length(elves_with_food) - 3)
max_three <- tail(partial, 3)
cat(sum(max_three))