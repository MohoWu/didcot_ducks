# functions
which_week <- function(the_date, cycle = FALSE) {
  
  weeks_from_start <- as.integer(floor((the_date - as_date("2019-11-11"))/7) + 1)
  
  no_cycle <- ifelse(weeks_from_start %% 4 == 0, 
                     floor(weeks_from_start / 4) - 1,
                     floor(weeks_from_start / 4))
  
  no_week <- ifelse(weeks_from_start > 4, 
                    weeks_from_start %% 4, 
                    weeks_from_start)
  
  # return cycle number or week number
  if (cycle) no_cycle else as.numeric(no_week)
  
}
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
      filter(pot >= 10,
             Type == "paym") %>%
      mutate(dividor = length(unique(Name)),
             pot = purrr::map_dbl(remain/dividor,
                                  round_05)) %>%
      select(Name, pot)
    
  } else {
    
    pot <- data %>%
      filter(Type == "paym") %>%
      select(Name, pot)
  }
  
  pot
  
}

old_pot <- data.frame(Name = character(),
                      pot = numeric(0),
                      stringsAsFactors = FALSE)

payment_sheet <- "https://docs.google.com/spreadsheets/d/1_KxXTywIfc3uySg9eIGZ97YWTNUqtiam2HsrND7W9fU/export?usp=sharing&format=csv"

player_sheet <- "https://docs.google.com/spreadsheets/d/1qFj1TMgxHL37P-pT5UyTU3-ei5tEwrSFkBIp7-XB50s/export?usp=sharing&format=csv"