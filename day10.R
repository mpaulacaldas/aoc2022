library(tidyverse)

input <- tibble(
  command = read_lines("day10.txt"),
  duration = ifelse(str_starts(command, "addx"), 2, 1),
  units = command |> str_extract("-?\\d+") |> as.numeric()
  )

cycles <- input |>
  mutate(
    cycle = cumsum(duration)+ 1,
    x = cumsum(coalesce(units, 0)) + 1
  ) %>%
  right_join(
    tibble(cycle = 1:sum(.$duration)),
    by = "cycle"
  ) |>
  arrange(cycle) |>
  fill(x)


# Part 1 ------------------------------------------------------------------

cycles |>
  filter(cycle %in% seq(20, 220, by = 40)) |>
  mutate(signal_stregth = cycle * x) |>
  summarise(sum(signal_stregth))


# Part 2 ------------------------------------------------------------------

cycles |>
  mutate(row = ((cycle - 1) %/% 40) + 1) |>
  group_by(row) |>
  mutate(column = row_number(cycle)) |>
  ungroup() |>
  mutate(
    sprite = coalesce(x, 1),
    draw = ifelse(column >= sprite & column <= sprite + 2, "#", ".")
  ) |>
  group_by(row) |>
  summarise(draw = paste0(draw, collapse = "")) |>
  ungroup()
