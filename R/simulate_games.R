

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
    order = spaces_order,
    grid = spaces_grid,
    order_number = stringr::str_extract(spaces_order, "\\d+")
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
    #' @field cards A list of bingo cards used in the game
    cards = NULL,
    
    #' @field spaces A list of spaces to call during the game
    spaces = NULL,
    
    #' @field order_number The order of the spaces to call
    order_number = NULL,
    
    #' @field turns The number of turns that have been played
    turns = NA_real_,
    
    #' @field cards_in_play Integer vector to index cards that are still in play
    cards_in_play = NULL,
    
    #' @field drawn A vector of spaces that have been drawn
    drawn = NULL,
    
    #' @field bingos A data frame to track when cards win a bingo
    bingos = NULL,
    
    initialize = function(n = 2, cards = NULL) {
      if(is.null(cards)) {
        cards <- make_n_cards(n)
       } else if (n != length(cards)) {
       futile.logger::flog.warn(
         "Playing with %i cards, but %i cards were expected",
         n,
         length(cards)
       )
      }
      self$cards <- cards
      self$spaces <- draw_spaces(cards)
      self$order_number <- self$spaces$order_number
      self$turns <- 0
      self$cards_in_play <- 1:length(self$cards)
      self$drawn <- 0
      self$bingos <- data.frame(
        card = 1:length(cards),
        bingo_turn = 0
      )
    },
    draw = function() {
      self$turns <- self$turns + 1
      self$drawn <- c(self$drawn, self$order_number[self$turns])
    },
    check_cards = function() {
      cards_to_check <- self$cards[self$cards_in_play]
      bingo_check <- cards_to_check %>%
        purrr::map_lgl(~check_card(., self$drawn))

      if(sum(bingo_check) > 0) {
        winning_cards <- self$cards_in_play[bingo_check]
        # Mark the turn that cards won a bingo
        self$bingos$bingo_turn[winning_cards] <- self$turns
        self$cards_in_play <- self$cards_in_play[!bingo_check]
      }
    },
    play_turn = function() {
      self$draw()
      self$check_cards()
      },
    play_game = function() {
      while(length(self$cards_in_play) > 0) {
        self$play_turn()
      }
      
      return(self$bingos)
    } 
    

  )
)
