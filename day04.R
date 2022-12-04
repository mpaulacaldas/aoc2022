library(tidyverse)

input <- read_csv("day04.txt", col_names = c("elf1", "elf2"))

ranges <- input |>
  mutate(pair = row_number()) |>
  separate_rows(elf1, elf2, sep = "-", convert = TRUE) |>
  group_by(pair) |>
  summarise(across(
    c(elf1, elf2),
    ~ list(seq(min(.x), max(.x)))
  ))


# Part 1 ------------------------------------------------------------------

ranges |>
  rowwise() |>
  mutate(
    all1in2 = all(elf1 %in% elf2),
    all2in1 = all(elf2 %in% elf1)
  ) |>
  ungroup() |>
  summarise(answer = sum(all1in2 | all2in1))


# Part 2 ------------------------------------------------------------------

ranges |>
  rowwise() |>
  mutate(any_overlap = any(elf1 %in% elf2)) |>
  ungroup() |>
  summarise(answer = sum(any_overlap))
