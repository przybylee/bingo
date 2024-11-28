devtools::load_all()

play_games_w_results <- function(n_cards) {
  game1 <- play_game$new(n_cards)
  results <- game1$play_game()
  
  # turn with the first bingo
  first_hit <- min(results$bingo_turn)
  
  # turn with the last bingo
  last_hit <- max(results$bingo_turn)
  
  # average turn for a bingo
  avg_hit <- mean(results$bingo_turn)
  
  # count the ties
  ties <- results %>% 
    group_by(bingo_turn) %>% 
    summarise(n = n()) %>%
    filter(n > 1) %>% 
    summarise(tie_2 = sum(n >= 2),
              tie_3 = sum(n >= 3),
              tie_4 = sum(n >= 4),
              tie_5 = sum(n >= 5),
              tie_6 = sum(n >= 6),
              tie_7 = sum(n >= 7),
              tie_8 = sum(n >= 8)
    )
  
  output <- cbind(first_hit, last_hit, avg_hit, ties)
  return(output)
}

play_games_w_results(2)

# Get results for n games
n <- 100
n_cards <- 8
x <- rep(n_cards, n)

t0 <- Sys.time()
game_results <- purrr::map_dfr(x, play_games_w_results) %>% 
  as_tibble()
tf <- Sys.time()

print(tf - t0)

game_results %>% 
  summarise(
    n = n(),
    games_with_ties = sum(tie_2 > 0),
    across(ends_with("hit"), mean),
    across(starts_with("tie"), sum)
  )

# Get bingo cards and draw numbers
game1$cards

game1$spaces   
