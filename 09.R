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

pos <- c(0, 0, 5)
all_tl <- list(c(0, 0))
update <- function(pos, motion) {
  direction <- motion[[1]]
  howmany <- motion[[2]]
  for (i in seq(1, howmany)) {
    pos[[1]] <- hd_x <- pos[[1]] + disp_x[[direction]]
    pos[[2]] <- hd_y <- pos[[2]] + disp_y[[direction]]
    pos[[3]] <- tl <- disp_tl[[direction]][[pos[[3]]]]
    all_tl <<- append(all_tl, list(c(hd_x + offset_x[tl], hd_y + offset_y[tl])))
  }
  pos
}
motions <- readLines("inputs/09.txt") |>
  strsplit(" ") |>
  map(~ list(.x[[1]], as.integer(.x[[2]])))

accumulate(motions, update, .init = pos)
print(length(unique(all_tl)))
