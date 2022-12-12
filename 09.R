library(purrr)

disp_tl <- list(
  R=c(4,1,2,4,4,5,4,7,8),
  L=c(2,3,6,5,6,6,8,9,6),
  U=c(4,5,6,7,8,9,8,8,8),
  D=c(2,2,2,1,2,3,4,5,6)
)
disp_x <- list(R=1, L=-1, U=0, D=0)
disp_y <- list(R=0, L=0, U=1, D=-1)
offset_x <- c(-1, 0, 1, -1, 0, 1, -1, 0, 1)
offset_y <- c(1, 1, 1, 0, 0, 0, -1, -1, -1)

move_rope <- function(motions) {
  npos <- sum(map_int(motions, 2)) + 1
  all_pos <- vector(mode = "list", length = 10)

  i <- 1
  hd_x <- 0
  hd_y <- 0
  tl <- 5
  all_pos[[i]] <- c(hd_x, hd_y, tl)
  for (motion in motions) {
    direction <- motion[[1]]
    howmany <- motion[[2]]
    for (j in seq(1, howmany)) {
      hd_x <- hd_x + disp_x[[direction]]
      hd_y <- hd_y + disp_y[[direction]]
      tl <- disp_tl[[direction]][[tl]]
      i <- i + 1
      all_pos[[i]] <- c(hd_x, hd_y, tl)
    }
  }
  
  all_pos
}
to_abs <- function(pos) {
  hd_x <- pos[[1]]
  hd_y <- pos[[2]]
  tl <- pos[[3]]
  c(hd_x + offset_x[[tl]], hd_y + offset_y[[tl]])
}

motions <- readLines("inputs/09.txt") |>
  strsplit(" ") |>
  map(~ list(.x[[1]], as.integer(.x[[2]])))

all_pos <- move_rope(motions)

n <- map(all_pos, to_abs) |>
  unique() |>
  length()
print(n)
