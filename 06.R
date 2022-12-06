all_different <- function(chars) {
  length(unique(chars)) == length(chars)
}
  
scan <- function(s, l) {
  s <- unlist(strsplit(s, split = ""))
  for (i in seq_len(length(s) - l + 1)) {
    if (all_different(s[seq(i, i + l - 1)])) {
      return(i + l - 1)
    }
  }
  return(-1)
}

stream <- readLines("inputs/06.txt")[[1]]
print(scan(stream, 4))
print(scan(stream, 14))