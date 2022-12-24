library(tidyverse)

example_txt <-
"
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"

# Example -----------------------------------------------------------------

outline_head_path <- function(path) {
  read_table(path, col_names = c("motion", "steps")) |>
    mutate(
      x = recode(motion, R = 1, L = -1, .default = 0) * steps,
      y = recode(motion, U = 1, D = -1, .default = 0) * steps
    ) |>
    mutate(
      across(c(x, y), list(head = cumsum)),
      move = paste(row_number(), motion, steps)
    ) |>
    select(move, x_head, y_head) |>
    mutate(
      across(ends_with("head"), list(lag = \(x) lag(x, default = 0)))
    ) |>
    rowwise() |>
    mutate(
      x = list(x_head_lag:x_head),
      y = list(y_head_lag:y_head)
    ) |>
    ungroup() |>
    select(move, x, y) |>
    unnest_longer(c(x, y))
}

plot_path <- function(df, show.legend = TRUE, ...) {
  ggplot(df, aes(x, y, colour = move)) +
    geom_point(show.legend = show.legend) +
    geom_line(show.legend = show.legend)
}

head_path <- outline_head_path(example_txt)

plot_path(head_path)

# plot without the first and last move in each segment
head_path |>
  group_by(move) |>
  filter(!( row_number() %in% c(1, n()) )) |>
  ungroup() |>
  plot_path() +
  coord_cartesian(xlim = range(head_path$x), ylim = range(head_path$y))

# remove the first point of the segment if the connection with the first point
# of the next segment is not diagonal
tail_maybe <- head_path |>
  group_by(move) |>
  filter(!( row_number() %in% c(1, n()) )) |>
  ungroup()
tail_exclude <- tail_maybe |>
  group_by(move) |>
  filter( row_number() %in% c(1, n()) ) |>
  ungroup() |>
  mutate(pair = row_number() %/% 2) |>
  filter(row_number() > 1) |>
  group_by(pair) |>
  filter(diff(y) == 0 | diff(x) == 0) |>
  filter(row_number() == n()) |>
  ungroup()

# get the same figure as the prompt, without the starting point
tail_maybe |>
  anti_join(tail_exclude, by = c("move", "x", "y")) |>
  plot_path() +
  coord_cartesian(xlim = range(tail_maybe$x), ylim = range(tail_maybe$y))

# works for the example, but not for the real input (returns 6034, too low)
n_positions <- tail_maybe |>
  anti_join(tail_exclude, by = c("move", "x", "y")) |>
  distinct(x, y) |>
  nrow() |>
  (\(x) x + 1)()


# Part 1 ------------------------------------------------------------------

head_path <- outline_head_path("day09.txt") |>
  group_by(move) |>
  filter(row_number() < n()) |>
  ungroup() |>
  select(-move)

tail_locs <- head_path[1, ]

for (i in 2:nrow(head_path)) {

  head_loc <- head_path[i, ]
  tail_loc <- tail(tail_locs, 1)

  dist_xy <- dist(bind_rows(head_loc, tail_loc)) # euclidian dist

  if (dist_xy < 2) next

  dist_x  <- head_loc$x - tail_loc$x
  dist_y  <- head_loc$y - tail_loc$y

  tail_locs <- tail_locs |>
    add_row(
      x = tail_loc$x + sign(dist_x),
      y = tail_loc$y + sign(dist_y)
    )
}

nrow(distinct(tail_locs))


# Part 2 ------------------------------------------------------------------

map_tail <- function(lead_path) {

  tail_locs <- lead_path[1, ]

  for (i in 2:nrow(lead_path)) {

    head_loc <- lead_path[i, ]
    tail_loc <- tail(tail_locs, 1)

    dist_xy <- dist(bind_rows(head_loc, tail_loc)) # euclidian dist

    if (dist_xy < 2) next

    dist_x  <- head_loc$x - tail_loc$x
    dist_y  <- head_loc$y - tail_loc$y

    tail_locs <- tail_locs |>
      add_row(
        x = tail_loc$x + sign(dist_x),
        y = tail_loc$y + sign(dist_y)
      )
  }

  tail_locs

}

tail_last_iter <- tail_locs
j <- 2

while (j <= 9) {
  tail_last_iter <- map_tail(tail_last_iter)
  j <- j + 1
}

nrow(distinct(tail_last_iter))
