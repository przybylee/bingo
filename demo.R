devtools::load_all()

game1 <- play_game$new(8)
results <- game1$play_game()

# turn with the first bingo
min(results$bingo_turn)

# turn with the last bingo
max(results$bingo_turn)

# average turn for a bingo
mean(results$bingo_turn)

# count the ties
results %>% 
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
