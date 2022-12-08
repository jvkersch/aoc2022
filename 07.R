library(R6)
library(rlang)
library(purrr)

parse_command <- function(line) {
  items <- strsplit(line, " ")[[1]]
  list(what = items[[2]], args = if (length(items) == 3) items[[3]])
}

parse_ls_output <- function(line) {
  items <- strsplit(line, " ")[[1]]
  list(size = as.integer(items[[1]]), name = items[[2]])
}

env_values <- function(env) {
  unlist(map(env_names(env), ~ env[[.x]]))
}

Entry <- R6Class("Entry", list(
  name = NULL,
  isfile = FALSE,
  entries = NULL,
  size = 0,
  parent = NULL,
  initialize = function(name, isfile = FALSE, size = 0, parent = NULL) {
    self$name <- name
    self$isfile <- isfile
    self$entries <- new.env()
    self$size <- size
    self$parent <- parent
  },
  add_or_get = function(name, isfile, size) {
    if (!env_has(self$entries, name)) {
      self$entries[[name]] <- Entry$new(name, isfile, size, self)
    }
    self$entries[[name]]
  },
  add_or_get_subdir = function(name) 
    self$add_or_get(name, isfile = FALSE, 0),
  add_or_get_file = function(name, size) 
    self$add_or_get(name, isfile = TRUE, size),
  compute_size = function() {
    if (self$size == 0) {
      for (entry in env_values(self$entries)) {
        self$size <- self$size + entry$compute_size()
      }
    }
    self$size
  },
  directories = function() {
    keep(env_values(self$entries), ~ !(.x$isfile))
  }
))

root_dir <- Entry$new("/")
current <- root_dir

# Build up file system tree
lines <- readLines("inputs/07.txt")
for (line in lines) {
  if (line == "") next
  if (substr(line, 1, 1) == "$") { # Command
    cmd <- parse_command(line)
    if (cmd$what == "cd") {
      if (cmd$args == "..") { 
        current <- current$parent
      }
      else {
        current <- current$add_or_get_subdir(cmd$args)
      }
    }
  } else { # Output (from ls command)
    if (substr(line, 1, 1) != "d") { # ignore dir output lines
      ls_output <- parse_ls_output(line)
      current$add_or_get_file(ls_output$name, ls_output$size)
    }
  }
}

# Fill in directory sizes
root_dir <- root_dir$add_or_get("/")
stack <- c(root_dir)
sizes <- numeric()
while (length(stack) > 0) {
  current <- tail(stack, 1)[[1]]
  stack <- stack[-length(stack)]
  sizes <- c(sizes, current$compute_size())
  for (dir in current$directories()) {
    stack <- c(stack, dir)
  }
}
print(sum(sizes[sizes <= 100000]))
needed <- root_dir$size - 40000000
print(min(sizes[sizes >= needed]))