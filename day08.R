trees <- readr::read_lines("day08.txt") |>
  stringr::str_split("", simplify = TRUE) |>
  apply(2, as.numeric)


# Part 1 ------------------------------------------------------------------

nr <- nrow(trees)
nc <- ncol(trees)

n_visible <- 2 * (nr + nc - 2)

for (i in 2:(nr - 1)) {
  for (j in 2:(nc - 1)) {
    t <- all(trees[-(i:nr), j] < trees[i, j])
    b <- all(trees[-(1:i), j]  < trees[i, j])
    r <- all(trees[i, -(1:j)]  < trees[i, j])
    l <- all(trees[i, -(j:nc)] < trees[i, j])
    is_visible <- any(c(t, b, r, l))
    n_visible <- n_visible + is_visible
  }
}
n_visible


# Part 2 ------------------------------------------------------------------

scores <- trees * 0

nr <- nrow(trees)
nc <- ncol(trees)


for (i in 2:(nr - 1)) {
  for (j in 2:(nc - 1)) {

    tree <- trees[i, j]

    score <- list(
      u = trees[-(i:nr), j],
      l = trees[i, -(j:nc)],
      r = trees[i, -(1:j)],
      b = trees[-(1:i), j]
      ) |>
      purrr::map_at(c("u", "l"), rev) |>
      purrr::map(\(x) if (all(x < tree)) length(x) else which.min(x < tree)) |>
      purrr::reduce(`*`)

    scores[i, j] <- score

  }
}

max(scores)
which(scores == max(scores), arr.ind = TRUE)
