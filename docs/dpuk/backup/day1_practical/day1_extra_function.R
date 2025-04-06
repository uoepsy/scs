# Extra: Writing Functions
# ======================

# rolling a dice
dice <- function(num = 1){
  sum(sample(1:6, size = num, replace = TRUE))
}

dice()

replicate(10, dice()) |>
  table() |>
  barplot()

replicate(100, dice()) |>
  table() |>
  barplot()

replicate(1000, dice()) |>
  table() |>
  barplot()

# rolling a weighted dice
wdice <- function(num = 1){
  sum(sample(1:6, size = num, replace = TRUE, prob = c(.16,.16,.16,.16,.16,.2)))
}

replicate(10, wdice()) |>
  table() |>
  barplot()

replicate(100, wdice()) |>
  table() |>
  barplot()

replicate(1000, wdice()) |>
  table() |>
  barplot()

replicate(10000, wdice()) |>
  table() |>
  barplot()
