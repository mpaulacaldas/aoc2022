library(tidyverse)

path <- "day05.txt"

input <- readLines(path)
split_input <- str_which(input, "^$")

# Parse -------------------------------------------------------------------

moves <- tibble(input = input) |>
  filter(row_number() > split_input) |>
  mutate(num = str_extract_all(input, "\\d+")) |>
  rowwise() |>
  transmute(from = num[2], n = num[1], to = num[3]) |>
  ungroup() |>
  mutate(across(.fns = as.numeric))

stacks <- read_fwf(path, n_max = split_input - 2, show_col_types = FALSE) |>
  mutate(across(.fns = ~str_extract(.x, "\\w"))) |>
  arrange(desc(row_number())) |>
  as.list() |>
  map(~ .x[!is.na(.x)])


# Part 1 ------------------------------------------------------------------

stacks1 <- stacks

for (i in 1:nrow(moves)) {
  m <- moves[i, ]
  piece <- tail(stacks1[[m$from]], m$n)
  stacks1[[m$from]] <- head(stacks1[[m$from]], -m$n)
  stacks1[[m$to]]   <- c(stacks1[[m$to]], rev(piece))
}

stacks1 |>
  compact() |>
  map(~ rev(.x)[1]) |>
  paste0(collapse = "")


# Part2 -------------------------------------------------------------------

stacks2 <- stacks

for (i in 1:nrow(moves)) {
  m <- moves[i, ]
  piece <- tail(stacks2[[m$from]], m$n)
  stacks2[[m$from]] <- head(stacks2[[m$from]], -m$n)
  stacks2[[m$to]]   <- c(stacks2[[m$to]], piece) # remove rev()
}

stacks2 |>
  compact() |>
  map(~ rev(.x)[1]) |>
  paste0(collapse = "")
