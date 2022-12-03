input <- readLines("input02.txt")

# wrote the game matrix with payoffs on a piece of paper
my_score <- function(score) {
  switch(
    score,
    "A X" = 4,
    "A Y" = 8,
    "A Z" = 3,
    "B X" = 1,
    "B Y" = 5,
    "B Z" = 9,
    "C X" = 7,
    "C Y" = 2,
    "C Z" = 6
  )
}

input |>
  vapply(my_score, double(1)) |>
  sum()


my_score2 <- function(score) {
  switch(
    score,
    # lose
    "A X" = 3,
    "B X" = 1,
    "C X" = 2,
    # tie
    "A Y" = 4,
    "B Y" = 5,
    "C Y" = 6,
    # win
    "A Z" = 8,
    "B Z" = 9,
    "C Z" = 7
  )
}

input |>
  vapply(my_score2, double(1)) |>
  sum()
