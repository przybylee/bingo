#make bingo cards

#' Sample column values
#'
#' @param off_set A natural number, the lower bound for the column's values
#'
#' @return
#' @export
#'
#' @examples #none
sample_col <- function(off_set = 0) {
  col <- sample(1:15, 5, replace = FALSE) + off_set
  col
}

#' Simulate a random bingo card
#'
#' @param free_space A logical value, whether or not to include a free space
#'
#' @return A dataframe with 5 rows and 5 columns to represent a bingo card
#' @export
#'
#' @examples make_card()
make_card <- function(free_space = TRUE) {
  b_col <- sample_col(0)
  i_col <- sample_col(15)
  n_col <- sample_col(30)
  g_col <- sample_col(45)
  o_col <- sample_col(60)

  if(free_space) n_col[3] <- 0

  card <- data.frame(
    b = b_col,
    i = i_col,
    n = n_col,
    g = g_col,
    o = o_col
  )

  return(card)
}


#' Generate n bingo cards
#'
#' @param n A natural number, the number of bingo cards desired
#'
#' @return A list of length `n` bingo cards.
#' @export
#'
#' @examples #none
make_n_cards <- function(n = 8, free_space = TRUE) {
  output <- list()

  for(j in 1:n) {
    output[[j]] <- make_card(free_space)
  }

  return(output)
}


# Add a function to check for ties in a list of n cards
