library(purrr)

str_to_chars <- function(s) unlist(strsplit(s, split = ""))

split_half <- function(el) {
  n <- length(el)
  list(el[1:(n/2)], el[(n/2+1):n])
}

first_common <- function(chunks) {
  reduce(chunks, intersect)[1]
}

encode <- function(char) {
  code <- utf8ToInt(char) - utf8ToInt("a")
  if (code < 0) code + 59L else code + 1L
}

inventories <- readLines("inputs/03.txt") |>
  map(str_to_chars)

print(inventories |> 
      map(split_half) |> 
      map_chr(first_common) |> 
      map_int(encode) |> 
      sum())

print(seq(1, length(inventories), by = 3) |>
      map(~ inventories[.x:(.x+2)]) |>
      map_chr(first_common) |>
      map_int(encode) |>
      sum())
    