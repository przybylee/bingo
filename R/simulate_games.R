

#' Draw the order to call the spaces for a game
#'
#' @param cards 
#'
#' @return The order to call the spaces
#' 
#' @importFrom magrittr %>%
#' @import dplyr
#' @export
#'
#' @examples #none
draw_spaces <- function(cards) {
  all_cards <- cards %>% 
    purrr::list_rbind() 
  
  spaces <- all_cards %>% 
    tidyr::pivot_longer(cols = everything()) %>%
    mutate(space = paste(name, value, sep = "-")) %>% 
    pull() %>% 
    unique()
  
  spaces_order <- sample(spaces, size = length(spaces))
  
  # Divide by 5 and take the ceiling to get the number of rounds
  rounds <- ceiling(length(spaces_order) / 5)
  
  
  spaces_grid <- data.frame(
    b = 1:rounds,
    i = 1:rounds + rounds,
    n = 1:rounds + rounds * 2,
    g = 1:rounds + rounds * 3,
    o = 1:rounds + rounds * 4
  )
  
  list(
    order <- spaces_order,
    grid <- spaces_grid,
    order_number <- stringr::str_extract(spaces_order, "\\d+"),
  )
}

#' Check a card for bingo
#'
#' @param card A 5  x 5 matrix representing a bingo card
#' @param drawn A vector of spaces that have been drawn.  Should only contain 
#' integers
#'
#' @return Logical, indicating if the card made a bingo
#' @export
#'
#' @examples #none
check_card <- function(card, drawn) {
  cols <- split(card, seq(nrow(card)))
  rows <- as.list(card)
  diagnals <- list(
    c(card[1, 1], card[2, 2], card[3, 3], card[4, 4], card[5, 5]),
    c(card[1, 5], card[2, 4], card[3, 3], card[4, 2], card[5, 1])
  )

  c(cols, rows, diagnals) %>% 
    purrr::map_lgl(~all(. %in% drawn)) %>% 
    any()
  
}

# use r6 class to contain a simulation of 1 game of bingo
play_game <- R6::R6Class(
  "play_game",
  public = list(
    initialize = function(cards) {
      self$spaces <- draw_spaces(cards)
      self$cards <- cards
      self$drawn <- c()
      self$bingo <- FALSE
    },
    draw = function() {
      if (length(self$drawn) == length(self$spaces$order)) {
        self$bingo <- TRUE
      } else {
        self$drawn <- c(self$drawn, self$spaces$order[length(self$drawn) + 1])
      }
    },
    check_bingo = function() {
      self$bingo
    }
  )
)