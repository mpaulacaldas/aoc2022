library(purrr)
library(stringr)

input <- readLines("day06.txt")

start_m <- function(by) {
  by:str_length(input) |>
    set_names() |>
    map(~ str_sub(input, start = .x - by + 1, end = .x)) |>
    discard(str_detect, "(.).*\\1") |>
    head(1) |>
    names()
}

start_m(4)  # part 1
start_m(14) # part 2
