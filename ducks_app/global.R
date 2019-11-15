round_05 <- function(x) {
  digit <- x - floor(x)

    if (digit >= 0.5) {
    return(floor(x) + 0.5)
  }
  
  if (digit < 0.5) {
    return(floor(x))
  }
}
calc_pot <- function(old, current, to_pay = -50) {
  
  # update pot money
  data <- current %>% 
    full_join(old, by = "Name", suffix = c("_c", "_o")) %>%
    mutate(pot = purrr::map2_dbl(pot_o, pot_c, 
                                 sum, na.rm = TRUE)) 
  # calculate remaining money to divide
  remain <- sum(data$pot) + to_pay
  
  # allocate remaining to people who paid £10 in the pot
  if (to_pay != 0) {
    
    pot <- data %>%
      filter(pot == 10) %>%
      mutate(dividor = length(unique(Name)),
             pot = purrr::map_dbl(remain/dividor,
                                  round_05)) %>%
      select(Name, pot)
    
  } else {
    return(data[c("Name", "pot")])
  }
  
  pot
  
}

old_pot <- data.frame(Name = character(),
                      pot = numeric(0),
                      stringsAsFactors = FALSE)

