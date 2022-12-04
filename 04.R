library(purrr)
library(stringr)

chores <- readLines("inputs/04.txt") |>
  map(~ str_match(.x, "(\\d+)-(\\d+),(\\d+)-(\\d+)")[2:5]) |> 
  unlist() |>
  map_int(strtoi) |>
  matrix(ncol = 4, byrow = TRUE)
colnames(chores) <- c("x1", "y1", "x2", "y2")
chores <- as.data.frame(chores)

with(chores, {
  contained <- ((x1 <= x2) & (y2 <= y1)) | ((x2 <= x1) & (y1 <= y2))
  overlaps <- ifelse(x1 <= x2, y1 >= x2, y2 >= x1)
  print(sum(contained))
  print(sum(overlaps))
})

