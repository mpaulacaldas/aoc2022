library(tidyverse)

input <- readr::read_lines("day03.txt")


# Part 1 ------------------------------------------------------------------

len <- str_length(input) / 2
one <- input |> str_sub(1, len) |> str_split("")
two <- input |> str_sub(len + 1, -1L) |> str_split("")

parse_letter <- function(x) {
  if (x %in% letters) return(which(letters == x))
  which(LETTERS == x) + 26
}

map2_chr(one, two, intersect) |>
  map_dbl(parse_letter) |>
  sum()


# Part 2 ------------------------------------------------------------------

tibble(
  rucksacks = input,
  group = ceiling(seq_along(input) / 3)
  ) |>
  rowwise() |>
  mutate(rucksacks = str_split(rucksacks, "")) |>
  group_by(group) |>
  summarise(
    badge = reduce(rucksacks, intersect),
    priority = parse_letter(badge)
    ) |>
  summarise(sum(priority))
