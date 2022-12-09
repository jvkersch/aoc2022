library(purrr)

visible_left <- function(s) {
  cummax(c(-1, s))[-length(s)-1] < s
}
visible_right <- function(s) rev(visible_left(rev(s)))
visible_both <- function(s) visible_left(s) | visible_right((s))

rows <- readLines("inputs/08.txt") |>
  strsplit("") |>
  map(as.integer)  # why doesn't as.numeric work here?
heights <- matrix(unlist(rows), nrow = length(rows), byrow = T)

visible <- t(apply(heights, 1, visible_both)) |
  apply(heights, 2, visible_both)
print(sum(visible))

# Scan leftwards from `i` to find the first element, `j`, in `s` 
# that is the same height or larger than `s[i]`. Return distance
# `i - j`.
scan_left <- function(s, i) {
  if (i == 1) 
    return(0)
  for (j in seq(i-1, 1)) {
    if (s[j] >= s[i]) break
  }
  return(i - j)
}
scan_right <- function(s, i) {
  scan_left(rev(s), length(s) - i + 1)
}
scan_all <- function(i, j) {
  (scan_left(heights[i,], j) *
     scan_right(heights[i,], j) *
     scan_left(heights[,j], i) * 
     scan_right(heights[,j], i))
}
max_view <- cross2(seq(1, nrow(heights)), seq(1, ncol(heights))) |>
  map_int(~ as.integer(scan_all(.x[[1]], .x[[2]]))) |>
  max()
print(max_view)