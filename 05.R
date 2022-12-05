library(purrr)

parse_numline <- function(line) {
  length(scan(text = line, what = "", quiet = TRUE))
}

parse_column <- function(i, lines) {
  idx <- 4*i - 2
  column <- c()
  for (line in lines) {
    ch <- substr(line, idx, idx)
    if (ch == " ") {
      break
    }
    column <- c(column, ch)
  }
  return(column)
}

parse_instruction <- function(line) {
  strsplit(line, " ")[[1]][c(2, 4, 6)] |>
    map_int(as.integer)
}

split_at <- function(items, idx) {
  # split sequence into two so that items[idx] is the 
  # first element of the second list.
  if (idx == 1) {
    list(vector(mode = "character"), items)
  } else if (idx == length(items) + 1) {
    list(items, vector(mode = "character"))
  } else {
    list(items[1:(idx-1)], items[idx:length(items)])
  }
}

move <- function(stacks, instr, reverse = TRUE) {
  howmany <- instr[[1]]
  from_idx <- instr[[2]]
  to_idx <- instr[[3]]

  from <- stacks[[instr[[2]]]]
  s <- split_at(from, length(from) - howmany + 1)
  if (reverse) {
    s[[2]] = rev(s[[2]])
  }
  
  stacks[[to_idx]] <- c(stacks[[to_idx]], s[[2]])
  stacks[[from_idx]] <- s[[1]]

  return(stacks)
}

move_all <- function(stacks, instructions, reverse = TRUE) {
  for (instr in instructions) {
    stacks = move(stacks, instr, reverse = reverse)
  }
  stacks
}

load_data <- function(fname ) {
  data <- readLines(fname)
  cutoff <- which(data == "")
  ncols <- parse_numline(data[[cutoff-1]])
  
  stacks <- map(1:ncols, parse_column, data[seq(cutoff-2, 1)])
  instructions <- map(data[(cutoff+1):length(data)], parse_instruction)

  list(stacks=stacks, instructions=instructions)
}

data <- load_data("inputs/05.txt")

stacks1 <- move_all(data$stacks, data$instructions, reverse = TRUE)
print(paste(map_chr(stacks1, tail, n=1), collapse = ""))